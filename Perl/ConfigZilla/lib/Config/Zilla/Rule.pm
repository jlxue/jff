package Config::Zilla::Rule;
use strict;
use warnings;
use Any::Moose;

has 'name'      => (is => 'r', isa => 'Str', required => 1);
has 'shortdesc' => (is => 'r', isa => 'Str', required => 1);
has 'longdesc'  => (is => 'r', isa => 'Str');
has 'depend'    => (is => 'r', isa => 'Str[]');
has 'notify'    => (is => 'r', isa => 'Str[]');

__PACKAGE__->meta->make_immutable();
