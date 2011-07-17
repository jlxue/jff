package Config::Zilla::CaptureLogger;
use strict;
use warnings;
use utf8;
use Any::Moose;
use IO::Select;

use constant {
    T_STDOUT               => 1,
    T_STDERR               => 2,
};


has 'pid_to_name'   => (is => 'ro', isa => 'HashRef[Str]', default => sub { {} });
has 'fd_to_pid'     => (is => 'ro', isa => 'HashRef[Int]', default => sub { {} });
has 'fd_to_type'    => (is => 'ro', isa => 'HashRef[Int]', default => sub { {} });
has 'selector'      => (is => 'ro', isa => 'IO::Select', default => sub { IO::Select->new() });


sub addCapturer {
    my ($self, $name, $pid, $stdout, $stderr) = @_;

    $stdout->blocking(0);
    $stderr->blocking(0);

    $self->pid_to_name->{$pid} = $name;
    $self->fd_to_pid->{ fileno($stdout) } = $pid;
    $self->fd_to_pid->{ fileno($stderr) } = $pid;
    $self->fd_to_type->{ fileno($stdout) } = T_STDOUT;
    $self->fd_to_type->{ fileno($stderr) } = T_STDERR;

    $self->selector->add($stdout, $stderr);
}


sub count {
    my ($self) = @_;

    return scalar(keys %{ $self->pid_to_name });
}

sub run {
    my ($self, $left_time) = @_;

    $left_time = 1 unless defined($left_time) && $left_time > 0;
}


no Any::Moose;
__PACKAGE__->meta->make_immutable();
