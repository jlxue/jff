package MyTrac::Article;
use Any::Moose;
use namespace::autoclean;

our $VERSION = '0.01';

extends 'MyTrac::Item';

has 'title'     => (is => 'rw', isa => 'Str', required => 1);
has 'content'   => (is => 'rw', isa => 'Str', required => 1);

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
