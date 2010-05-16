package MyTrac::Article;
use Moose;
use namespace::autoclean;

our $VERSION = '0.01';

extends 'MyTrac::Item';

no Moose;
__PACKAGE__->meta->make_immutable();
1;
