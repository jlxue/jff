package Config::Zilla::FileRule;
use strict;
use warnings;
use Any::Moose;

extends 'Config::Zilla::Rule';

has 'packages'      => (is => 'ro', isa => 'ArrayRef[Str]', required => 1);

sub validate {
    my ($self) = @_;

    $self->SUPER::validate();

    my @pkgNames = @{ $self->packages };
    for my $name (@pkgNames) {
        die 'Package name must match /^[\w\.\-\~\+]+$/' if $name !~ /^[\w\.\-\~\+]+$/;
    }
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
