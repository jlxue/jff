package MyTrac::Item;
use Any::Moose;
use namespace::autoclean;

our $VERSION = '0.01';

has 'id'        => (is => 'rw', isa => 'Str');  # item id
has 'pid'       => (is => 'rw', isa => 'Str');  # parent id
has 'tid'       => (is => 'rw', isa => 'Str');  # topic id
has 'revision'  => (is => 'rw', isa => 'Str');
# use id instead of name, because name can be changed!
has 'authorId'  => (is => 'rw', isa => 'Str');
has 'timestamp' => (is => 'rw', isa => 'Int');

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
