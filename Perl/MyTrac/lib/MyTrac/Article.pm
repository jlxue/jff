package MyTrac::Article;
use Any::Moose;
use namespace::autoclean;

our $VERSION = '0.01';

extends 'MyTrac::Item';

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
