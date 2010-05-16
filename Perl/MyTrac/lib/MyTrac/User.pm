package MyTrac::User;
use Moose;
use namespace::autoclean;

our $VERSION = '0.01';

extends 'MyTrac::Item';

has 'name'      => (is => 'rw', isa => 'Str');
has 'email'     => (is => 'rw', isa => 'Str');
has 'groups'    => (is => 'rw', isa => 'ArrayRef[Str]');

no Moose;
__PACKAGE__->meta->make_immutable();
1;
