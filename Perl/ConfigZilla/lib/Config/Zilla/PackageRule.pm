package Config::Zilla::PackageRule;
use strict;
use warnings;
use utf8;
use Any::Moose;

extends 'Config::Zilla::Rule';

has 'packages'      => (is => 'ro', isa => 'ArrayRef[Str]', required => 1);

sub BUILD {
    my ($self, $args) = @_;

    my @pkgNames = @{ $self->packages };
    for my $name (@pkgNames) {
        confess 'Invalid package name' if $name !~ /^[\w\.\-\~\+]+$/;
    }
};

no Any::Moose;
__PACKAGE__->meta->make_immutable();
