package Config::Zilla::Engine;
use strict;
use warnings;
use utf8;
use Any::Moose;
use Data::Dumper;
use Module::Load;
use Try::Tiny;

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

    return if exists $options{VALIDATE_ONLY} && $options{VALIDATE_ONLY};

    my $rulegraph = _construct_rule_graph($ruleset);
    my %rulestate;

    while (keys(%$rulegraph) > 0) {
        for my $name (keys %$rulegraph) {
            if (keys %{ $rulegraph->{$name} } == 0) {

                # run one rule

                delete $rulegraph->{$name};
                map { delete $_->{$name} } values %$rulegraph;
            }
        }
    }
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


no Any::Moose;
__PACKAGE__->meta->make_immutable();

