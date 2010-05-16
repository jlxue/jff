package MyTrac::Item;
use Moose;
use MooseX::Storage;
use namespace::autoclean;

our $VERSION = '0.01';

with Storage(format => [JSONpm => {json_opts => {canonical => 1, pretty => 1}}],
             io => 'File');

has 'id'        => (is => 'rw', isa => 'Str');  # item id
has 'pid'       => (is => 'rw', isa => 'Str');  # parent id
has 'tid'       => (is => 'rw', isa => 'Str');  # topic id
has 'revision'  => (is => 'rw', isa => 'Str');
has 'author'    => (is => 'rw', isa => 'Str');
has 'subject'   => (is => 'rw', isa => 'Str');
has 'ctime'     => (is => 'rw', isa => 'Int');
has 'mtime'     => (is => 'rw', isa => 'Int');

no Moose;
__PACKAGE__->meta->make_immutable();
1;
