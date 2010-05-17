package MyTrac::Item;
use Any::Moose;
use Any::Moose 'X::Storage';
use namespace::autoclean;

our $VERSION = '0.01';

if (any_moose eq 'Moose') {
    with Storage(format => [JSONpm => {json_opts => {canonical => 1, pretty => 1}}],
                 io => 'File');
} else {
    with 'MouseX::Storage';
}

has 'id'        => (is => 'rw', isa => 'Str');  # item id
has 'pid'       => (is => 'rw', isa => 'Str');  # parent id
has 'tid'       => (is => 'rw', isa => 'Str');  # topic id
has 'revision'  => (is => 'rw', isa => 'Str');
has 'author'    => (is => 'rw', isa => 'Str');
has 'subject'   => (is => 'rw', isa => 'Str');
has 'ctime'     => (is => 'rw', isa => 'Int');
has 'mtime'     => (is => 'rw', isa => 'Int');

no Any::Moose;
__PACKAGE__->meta->make_immutable();
1;
