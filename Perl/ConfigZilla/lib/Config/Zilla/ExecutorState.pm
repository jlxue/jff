package Config::Zilla::ExecutorState;
use strict;
use warnings;
use utf8;
use Any::Moose;

use Config::Zilla::Constants qw/EC_FAIL_UNKNOWN/;

has 'pid'       => (is => 'ro', isa => 'Maybe[Int]', required => 1);
has 'stdout'    => (is => 'ro', isa => 'Maybe[FileHandle]', required => 1);
has 'stderr'    => (is => 'ro', isa => 'Maybe[FileHandle]', required => 1);
has 'starttime' => (is => 'rw', isa => 'Num', default => time());
has 'endtime'   => (is => 'rw', isa => 'Num');
has 'exit_code' => (is => 'rw', isa => 'Int', default => EC_FAIL_UNKNOWN);

no Any::Moose;
__PACKAGE__->meta->make_immutable();
