package Config::Zilla::Types;
use strict;
use warnings;
use Any::Moose qw/::Util::TypeConstraints/;


subtype 'NonNegativeInt',
    as 'Int',
    where { $_ >= 0 },
    message { "$_ is not a non-negative integer!" };


no Any::Moose qw/::Util::TypeConstraints/;
1;
