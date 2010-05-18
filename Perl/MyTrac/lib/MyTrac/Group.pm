package MyTrac::Group;
use Any::Moose;
use namespace::autoclean;

our $VERSION = '0.01';

extends 'MyTrac::Item';

has 'name'      => (is => 'rw', isa => 'Str');
# use id instead of user name, because name can be changed!
has 'memeberIds'=> (is => 'rw', isa => 'ArrayRef[Str]');

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
