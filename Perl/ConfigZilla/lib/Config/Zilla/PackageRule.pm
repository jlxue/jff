package Config::Zilla::PackageRule;
use strict;
use warnings;
use Any::Moose;

extends 'Config::Zilla::Rule';

has 'packages'      => (is => 'ro', isa => 'ArrayRef[Str]', required => 1);

override validate => sub {
    my ($self) = @_;

    super();

    my @pkgNames = @{ $self->packages };
    for my $name (@pkgNames) {
        confess 'Package name must match /^[\w\.\-\~\+]+$/' if $name !~ /^[\w\.\-\~\+]+$/;
    }
};

no Any::Moose;
__PACKAGE__->meta->make_immutable();
