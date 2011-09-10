package Config::Zilla::ExecutorState;
use strict;
use warnings;
use utf8;
use Any::Moose;

has 'start_time'=> (is => 'rw', isa => 'Num', default => sub { time() });
has 'end_time'  => (is => 'rw', isa => 'Num');
has 'exit_code' => (is => 'rw', isa => 'Int', default => 255);

no Any::Moose;
__PACKAGE__->meta->make_immutable();
