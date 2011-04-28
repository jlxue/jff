package Config::Zilla::Engine;
use strict;
use warnings;
use Any::Moose;

has ruleset     => (is => 'ro', isa => 'HashRef[Config::Zilla::Rule]');

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

}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
