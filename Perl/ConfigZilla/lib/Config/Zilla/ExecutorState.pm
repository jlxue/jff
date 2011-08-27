package Config::Zilla::ExecutorState;
use strict;
use warnings;
use utf8;
use Any::Moose;

use Config::Zilla::Constants qw/EC_FAIL_UNKNOWN/;

has 'starttime' => (is => 'rw', isa => 'Num', default => sub { time() });
has 'endtime'   => (is => 'rw', isa => 'Num');
has 'exit_code' => (is => 'rw', isa => 'Int', default => EC_FAIL_UNKNOWN);

no Any::Moose;
__PACKAGE__->meta->make_immutable();
