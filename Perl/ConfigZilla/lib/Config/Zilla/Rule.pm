package Config::Zilla::Rule;
use strict;
use warnings;
use Any::Moose;

has 'name'      => (is => 'ro', isa => 'Str', required => 1);
has 'shortdesc' => (is => 'ro', isa => 'Str', required => 1);
has 'longdesc'  => (is => 'ro', isa => 'Str', default => '');

# Depends on which rules
has 'depend'    => (is => 'ro', isa => 'ArrayRef[Str]');

# Only execute this rule after 'ifelapsed' seconds
has 'ifelapsed' => (is => 'ro', isa => 'Int', default => 0);

has 'maxtime'   => (is => 'ro', isa => 'Int', default => 0);

has 'executor'  => (is => 'ro', isa => 'Str');

sub validate {
    my ($self) = @_;

    confess 'Rule name must match /^\w+$/' if $self->name !~ /^\w+$/;

    my @deps = @{ $self->depend };
    for my $dep (@deps) {
        confess 'Dependent must match /^\w+$/' if $dep !~ /^\w+$/;
    }
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
