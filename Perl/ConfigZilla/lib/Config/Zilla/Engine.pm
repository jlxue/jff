package Config::Zilla::Engine;
use strict;
use warnings;
use utf8;
use Any::Moose;
use Carp qw/cluck/;
use Data::Dumper;
use List::Util qw/max sum/;
use Module::Load;
use POSIX qw/:sys_wait_h/;
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
    my $start_time = time();
    my $left_time;
    my %states;

    ## sanity check on options

    $options{MAX_CONCURRENT} = DEFAULT_MAX_CONCURRENT unless
            exists $options{MAX_CONCURRENT} && $options{MAX_CONCURRENT} > 0;

    unless (exists $options{MAX_TIME} && $options{MAX_TIME} > 0) {
        # pass 0 to sum() to avoid returning undef when no rule is specified
        $options{MAX_TIME} = max(DEFAULT_MAX_TIME,
                                sum(0, map { $_->maxtime() } values %$ruleset));
    }


    ## run rule executors by topological order

    while (keys(%$rulegraph) > 0) {
        for my $name (keys %$rulegraph) {

            ## topological sort, whether this node has no predecessor
            next unless keys %{ $rulegraph->{$name} } == 0;


            $left_time = $options{MAX_TIME} - (time() - $start_time);   # XXX: overflow?
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


            if (defined $pid) {     # create child process successfully
                try {
                    $logger->addCapturer($name, $pid, $child_stdout, $child_stderr);

                    if ($logger->count() >= $options{MAX_CONCURRENT}) {
                        my @name_pids = $logger->run($left_time);
                        last TIMEOUT unless @name_pids;

                        _reap_children(\%states, @name_pids);

                        for (my $i = 0; $i < @name_pids; $i += 2) {
                            _remove_rule_from_graph($name_pids[$i], $rulegraph);
                        }
                    }
                } catch {
                    cluck "Catch exception during capture of rule $name: $_";
                };
            } else {
                $states{$name}->endtime(time());
                $states{$name}->exit_code(EC_FAIL_START);

                _remove_rule_from_graph($name, $rulegraph);
            }

        } # end for
    } # end while

    return 0;

TIMEOUT:
    cluck "Engine exceeds total max time $options{MAX_TIME} seconds," .
            "started at " . scalar(localtime($start_time));

    if ($logger->count() > 0) {
        $logger->run(1);    # last chance to flush children's output

        my @name_pids = $logger->name_pids();
        for (my $i = 0; $i < @name_pids; $i += 2) {
            _remove_rule_from_graph($name_pids[$i], $rulegraph);
        }

        $logger->clear();
        _reap_children(\%states, @name_pids);
    }
    return;
}


sub _remove_rule_from_graph {
    my ($name, $rulegraph) = @_;

    delete $rulegraph->{$name};
    map { delete $_->{$name} } values %$rulegraph;
}


sub _reap_children {
    my ($states, @name_pids) = @_;

    for (my $i = 0; $i < @name_pids; $i += 2) {
        my ($name, $pid) = @name_pids[$i .. ($i+1)];
        my ($n, $ret);

        $n = 10;
        do {
            $ret = waitpid($pid, WNOHANG);
            sleep 1;
        } while ($ret != $pid && --$n > 0);


        if ($ret != $pid) {
            $n = 5;
            do {
                if ($n > 3) {
                    kill 2, $pid;   # INT
                } elsif ($n > 1) {
                    kill 15, $pid;  # TERM
                } else {
                    kill 9, $pid;   # KILL
                }
                sleep 1;
                $ret = waitpid($pid, WNOHANG);
            } while ($ret != $pid && --$n > 0);

            $states->{$name}->exit_code(EC_FAIL_UNKNOWN);
        } else {
            $states->{$name}->exit_code($? >> 8);
        }

        $states->{$name}->endtime(time());
    }
}


no Any::Moose;
__PACKAGE__->meta->make_immutable();

