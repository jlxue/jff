package Config::Zilla::Engine;
use strict;
use warnings;
use utf8;
use Any::Moose;
use Data::Dumper;

has 'ruleset'       => (is => 'ro', isa => 'HashRef[Config::Zilla::Rule]');
has 'sorted_rules'  => (is => 'ro', isa => 'ArrayRef[Str]');

sub addRule {
    my ($self, $rule) = @_;

    confess "Invalid argument" unless blessed $rule && $rule->isa('Config::Zilla::Rule');

    my $rules = $self->ruleset;
    my $name = $rule->name;

    if (exists $rules->{$name}) {
        my $rule2 = $rules->{$name};

        confess "Conflict rule names: $name (" . $rule2->shortdesc .  ")";
    }

    $rules->{$name} = $rules;
}

sub validate {
    my ($self) = @_;
    my $rules = $self->ruleset;

    return unless defined $rules;

    my %h;
    while (my ($name, $rule) = each %$rules) {
        $h{$name} = {};

        my $depends = $rule->depends();
        next unless defined $depends;

        for my $dep (@$depends) {
            confess "Rule \"$name\" depends non-exist rule \"$dep\"" unless
                    exists $rules->{$dep};

            $h{$name}->{$dep} = 1;
        }
    }

    my @sorted_rules;
    my $got;
    while (keys %h > 0) {
        $got = 0;

        for my $name (keys %h) {
            if (keys %{ $h{$name} } == 0) {
                $got = 1;

                push @sorted_rules, $name;
                delete $h{$name};

                map { delete $_->{$name} } values %h;
            }
        }

        if (! $got && keys %h > 0) {
            confess "Found circular dependency:\n" . Dumper(\%h);
        }
    }

    $self->{sorted_rules} = \@sorted_rules;
}


sub run {
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
