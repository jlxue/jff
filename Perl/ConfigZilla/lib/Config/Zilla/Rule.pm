package Config::Zilla::Rule;
use strict;
use warnings;
use Any::Moose;

has 'name'      => (is => 'ro', isa => 'Str', required => 1);
has 'shortdesc' => (is => 'ro', isa => 'Str', required => 1);
has 'longdesc'  => (is => 'ro', isa => 'Str');
has 'target'    => (is => 'ro', isa => 'Str');
has 'depend'    => (is => 'ro', isa => 'ArrayRef[Str]');
has 'notify'    => (is => 'ro', isa => 'ArrayRef[Str]');
has 'expect'    => (is => 'ro');

no Any::Moose;
__PACKAGE__->meta->make_immutable();
