package MyTrac::Database::Operation;
use Any::Moose;
use namespace::autoclean;

our $VERSION = '0.01';

has 'id'        => (is => 'ro', isa => 'Str');
has 'data'      => (is => 'ro', isa => 'Str');
has 'filename'  => (is => 'rw', isa => 'Str');

sub BUILD {
}

sub prepare {
}

sub execute {
}

sub rollback {
}

sub DEMOLISH {
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
