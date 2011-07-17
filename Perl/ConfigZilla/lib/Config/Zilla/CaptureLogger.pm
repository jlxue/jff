package Config::Zilla::CaptureLogger;
use strict;
use warnings;
use utf8;
use Any::Moose;
use IO::Select;
use POSIX qw(:errno_h);

use constant {
    T_STDOUT    => 1,
    T_STDERR    => 2,

    BUF_LEN     => 8192,
};


has 'pid_to_name'   => (is => 'ro', isa => 'HashRef[Str]', default => sub { {} });
has 'fd_to_pid'     => (is => 'ro', isa => 'HashRef[Int]', default => sub { {} });
has 'fd_to_type'    => (is => 'ro', isa => 'HashRef[Int]', default => sub { {} });
has 'selector'      => (is => 'ro', isa => 'IO::Select', default => sub { IO::Select->new() });
has 'cached_bufs'   => (is => 'ro', isa => 'HashRef[Str]', default => sub { {} });


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
    my $cached_bufs = $self->cached_bufs;
    my @name_pids;
    my $start_time;

    die "Bad argument!" unless defined($left_time) && $left_time > 0;
    die "Bad usage!" if $self->count() == 0;


    do {
        $start_time = time();

        my @ready = $self->seletor->can_read($left_time);

        return if @ready == 0;  # timeout

        for my $fh (@ready) {
            my $fileno = fileno($fh);
            my $pid = $self->fd_to_pid->{$fileno};
            my $type = $self->fd_to_type->{$fileno};
            my $name = $self->pid_to_name->{$pid};
            my $out = $type == T_STDOUT ? \*STDOUT : \*STDERR;

            my $buf;
            if (exists $cached_bufs->{$fileno}) {
                $buf = $cached_bufs->{$fileno};
                delete $cached_bufs->{$fileno};
            } else {
                $buf = "";
            }

            my $len;
            while ($len = sysread $fh, $buf, BUF_LEN, length($buf)) {
                my $tmpbuf = $buf;

                open my $tmpfh, \$tmpbuf or die "$!\n";
                $buf = "";

                while (my $line = <$tmpfh>) {
                    if (0 == chomp $line) {
                        $buf = $line;
                    } else {
                        print $out "$name($pid): $line\n";
                    }
                }

                close $tmpfh;
            }

            if (!defined $len) {    # read nothing
                if ($! == EAGAIN) { # See "Perl Cookbook" Ch 7.14 "Doing Non-Blocking I/O"
                    # would blocked
                } else {
                    # XXX: what to do ?
                }
                if (length($buf) > 0) {
                    $cached_bufs->{$fileno} = $buf;
                }
            } else  {               # $len is 0, pipe closed
                if (length($buf) > 0) {
                    print $out "$name($pid): $buf\n";
                }

                delete $self->fd_to_pid->{$fileno};
                delete $self->fd_to_type->{$fileno};
                delete $self->pid_to_name->{$pid};
                $self->seletor->remove($fh);
                close($fh);

                push @name_pids, $name, $pid;
            }
        }

        $left_time -= time() - $start_time;
    } while ($left_time > 0);

    return @name_pids;
}


sub name_pids {
}


sub clear {
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
