package Config::Zilla::RuleEngine;
use strict;
use warnings;
use Carp;
use Any::Moose;

has ruleset     => (is => 'ro', isa => 'HashRef');

sub addRule {
    my ($self, $rule) = @_;
    my $rules = $self->ruleset;
    my $name = $rule->name;

    if (exists $rules->{$name}) {
        my $rule2 = $rules->{$name};

        confess "Conflict rule names: $name (" . $rule2->shortdesc .  ")";
    }

    $rules->{$name} = $rules;
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
