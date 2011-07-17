package Config::Zilla::Engine;
use strict;
use warnings;
use utf8;
use Any::Moose;
use Carp qw/cluck/;
use Data::Dumper;
use List::Util qw/max sum/;
use Module::Load;
use Try::Tiny;

use Config::Zilla::Capture qw/capture/;
use Config::Zilla::CaptureLogger;
use Config::Zilla::Constants qw/:EXIT_CODE/;
use Config::Zilla::ExecutorState;
use Config::Zilla::Rule;
use Config::Zilla::RuleExecutor;
use constant {
    DEFAULT_MAX_TIME        => 30 * 60,     # 0.5 hour
    DEFAULT_MAX_CONCURRENT  => 5,           # max child processes
};

our $VERSION = 0.1;


has 'ruleset'       => (is => 'ro', isa => 'HashRef[Config::Zilla::Rule]',
                        default => sub { {} });

sub addRule {
    my ($self, $rule) = @_;

    confess "Invalid argument" unless blessed $rule && $rule->isa('Config::Zilla::Rule');

    my $ruleset = $self->ruleset;
    my $name = $rule->name;

    if (exists $ruleset->{$name}) {
        my $r = $ruleset->{$name};

        confess "Conflict rule names: $name ($r->shortdesc)";
    }

    $ruleset->{$name} = $rule;
}


sub run {
    my ($self, %options) = @_;
    my $ruleset = $self->ruleset;

    _validate($ruleset);

    my $executors = _resolve_executors($ruleset);

    # _resolve_executors() also contains necessary validation
    return if exists $options{VALIDATE_ONLY} && $options{VALIDATE_ONLY};

    _run_loop($ruleset, $executors, %options);
}


sub _validate {
    my ($ruleset) = @_;

    _validate_dependent_rules_exist($ruleset);

    _validate_circular_dependency($ruleset);
}


# check all dependent rules whether exist
sub _validate_dependent_rules_exist {
    my ($ruleset) = @_;

    while (my ($name, $rule) = each %$ruleset) {
        my $depends = $rule->depends;

        for my $dep (@$depends) {
            confess "Rule \"$name\" depends non-exist rule \"$dep\"" unless
                    exists $ruleset->{$dep};
        }
    }
}


sub _validate_circular_dependency {
    my ($ruleset) = @_;
    my ($rulegraph, $got);

    $rulegraph = _construct_rule_graph($ruleset);

    while (keys(%$rulegraph) > 0) {
        $got = 0;

        for my $name (keys %$rulegraph) {
            if (keys %{ $rulegraph->{$name} } == 0) {
                $got = 1;

                delete $rulegraph->{$name};

                map { delete $_->{$name} } values %$rulegraph;
            }
        }

        if (! $got && keys(%$rulegraph) > 0) {
            confess "Found circular dependency:\n" . Dumper($rulegraph);
        }
    }
}


sub _construct_rule_graph {
    my ($ruleset) = @_;
    my %g;

    while (my ($name, $rule) = each %$ruleset) {
        $g{$name} = {};

        my $depends = $rule->depends;

        for my $dep (@$depends) {
            $g{$name}->{$dep} = 1;
        }
    }

    return \%g;
}


sub _resolve_executors {
    my ($ruleset) = @_;
    my %executors;

    while (my ($name, $rule) = each %$ruleset) {
        my $executor = $rule->executor;
        # XXX: is it better to call ref() to get the class name?
        $executor = $rule->meta->name . "Executor" unless defined $executor;

        load $executor;
        $executors{$name} = $executor->new($rule);
    }

    return \%executors;
}


sub _run_loop {
    my ($ruleset, $executors, %options) = @_;

    my $rulegraph = _construct_rule_graph($ruleset);
    my $logger = Config::Zilla::CaptureLogger->new();
    my %states;

    $options{MAX_CONCURRENT} = DEFAULT_MAX_CONCURRENT unless
            exists $options{MAX_CONCURRENT} && $options{MAX_CONCURRENT} > 0;

    unless (exists $options{MAX_TIME} && $options{MAX_TIME} > 0) {
        # pass 0 to sum() to avoid returning undef when no rule is specified
        $options{MAX_TIME} = max(DEFAULT_MAX_TIME,
                                sum(0, map { $_->maxtime() } values %$ruleset));
    }

    my $start_time = time();
    my $left_time;

    # run rule executors by topological order
    while (keys(%$rulegraph) > 0) {
        for my $name (keys %$rulegraph) {

            # has no predecessor
            if (keys %{ $rulegraph->{$name} } == 0) {
                $left_time = $options{MAX_TIME} - (time() - $start_time);
                if ($left_time <= 0) {
                    last TIMEOUT;
                }

                $states{$name} = Config::Zilla::ExecutorState->new(
                        exit_code => EC_FAIL_UNKNOWN);

                my ($pid, $child_stdout, $child_stderr);

                try {
                    ($pid, $child_stdout, $child_stderr) = capture {
                        # run the executor in child process
                        $executors->{$name}->execute();
                    };
                } catch {
                    cluck "Catch exception during start of rule $name: $_";
                };

                if (defined $pid) {
                    try {
                        $logger->addCapturer($name, $pid, $child_stdout, $child_stderr);

                        if ($logger->count() >= $options{MAX_CONCURRENT}) {
                            $logger->run($left_time);
                        }
                    } catch {
                        cluck "Catch exception during capture of rule $name: $_";
                    };
                } else {
                    $states{$name}->endtime(time());
                    $states{$name}->exit_code(EC_FAIL_START);

                    _remove_rule_from_graph($name, $rulegraph);
                }
            } # end if
        } # end for
    } # end while

    return 0;

TIMEOUT:
    cluck "Engine exceeds total max time $options{MAX_TIME} seconds," .
            "started at " . scalar(localtime($start_time));

    # TODO: _kill_children();
    return 1;
}


sub _remove_rule_from_graph {
    my ($name, $rulegraph) = @_;

    delete $rulegraph->{$name};
    map { delete $_->{$name} } values %$rulegraph;
}


no Any::Moose;
__PACKAGE__->meta->make_immutable();

