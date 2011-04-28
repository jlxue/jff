package Config::Zilla::Engine;
use strict;
use warnings;
use Any::Moose;

has ruleset     => (is => 'ro', isa => 'HashRef[Config::Zilla::Rule]');

sub addRule {
    my ($self, $rule) = @_;

    die "Invalid argument" if ! $rule->isa('Config::Zilla::Rule');

    my $rules = $self->ruleset;
    my $name = $rule->name;

    if (exists $rules->{$name}) {
        my $rule2 = $rules->{$name};

        die "Conflict rule names: $name (" . $rule2->shortdesc .  ")";
    }

    $rules->{$name} = $rules;
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
