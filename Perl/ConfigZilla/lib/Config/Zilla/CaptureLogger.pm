package Config::Zilla::CaptureLogger;
use strict;
use warnings;
use utf8;
use Any::Moose;
use IO::Select;

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

            my $len = sysread $fh, $buf, BUF_LEN, length($buf);

            if (!defined($len) or $len == 0) {
                if (length($buf) > 0) {
                    chomp $buf;
                    print $out "$name($pid): $buf\n";
                }

                delete $self->fd_to_pid->{$fileno};
                delete $self->fd_to_type->{$fileno};
                delete $self->pid_to_name->{$pid};
                $self->seletor->remove($fh);
                close($fh);

                push @name_pids, $name, $pid;
            } else {
                open my $fh2, \$buf or die "$!\n";
                while (my $line = <$fh2>) {
                    if (0 == chomp $line) {
                        $cached_bufs->{$fileno} = $line;
                        last;
                    } else {
                        print $out "$name($pid): $buf\n";
                    }
                }
                close $fh2;
            }
        }
    } while ($left_time > 0);

    return @name_pids;
}


sub name_pids {
}


sub clear {
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
