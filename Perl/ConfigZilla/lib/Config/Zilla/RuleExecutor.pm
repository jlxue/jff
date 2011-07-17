package Config::Zilla::Executor;
use strict;
use warnings;
use Any::Moose qw/Role/;

requires qw/check execute rollback/;

1;

