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

    BUF_LEN     => 2 * 1024,

    IDX_NAME    => 0,
    IDX_FDCOUNT => 1,
};

use constant NL => eval {
    local $\;
    pipe my $r, my $w or die "Can't create pipe: $!";
    open my $fh, '>&=', fileno($w) or die "Can't fdopen: $!";
    select((select($fh), $| = 1)[0]);
    print $fh "\n";
    sysread $r, my $buf, 8;
    close $fh;
    close $w;
    close $r;
    $buf;
};

sub _foreach_lines_with_open {
    my ($lines, $out_fh, $name, $pid) = @_;

    open my $fh, "<", \$lines or die "$!";
    my $buf = "";

    while (my $line = <$fh>) {
        if (0 == chomp $line) {
            $buf = $line;
        } else {
            print $out_fh "$name($pid): $line\n";
        }
    }

    close $fh;
    return $buf;
}

sub _foreach_lines_with_index {
    my ($lines, $out_fh, $name, $pid) = @_;

    my $offset = 0;
    my $pos;
    while (($pos = index($lines, NL, $offset)) >= 0) {
        print $out_fh substr($lines, $offset, $pos - $offset), "\n";
        $offset = $pos + length(NL);
    }

    if ($offset < length($lines)) {
        return substr($lines, $offset);
    } else {
        return "";
    }
}

BEGIN {
    if (NL) {
        *_foreach_lines = \&_foreach_lines_with_index;
    } else {
        *_foreach_lines = \&_foreach_lines_with_open;
    }
}


has 'pid_to_info'   => (is => 'ro', isa => 'HashRef[ArrayRef]', default => sub { {} });
has 'fd_to_pid'     => (is => 'ro', isa => 'HashRef[Int]', default => sub { {} });
has 'fd_to_type'    => (is => 'ro', isa => 'HashRef[Int]', default => sub { {} });
has 'selector'      => (is => 'ro', isa => 'IO::Select', default => sub { IO::Select->new() });
has 'cached_bufs'   => (is => 'ro', isa => 'HashRef[Str]', default => sub { {} });


sub addCapturer {
    my ($self, $name, $pid, $stdout, $stderr) = @_;

    $stdout->blocking(0);
    $stderr->blocking(0);

    $self->pid_to_info->{$pid} = [$name, 2];
    $self->fd_to_pid->{ fileno($stdout) } = $pid;
    $self->fd_to_pid->{ fileno($stderr) } = $pid;
    $self->fd_to_type->{ fileno($stdout) } = T_STDOUT;
    $self->fd_to_type->{ fileno($stderr) } = T_STDERR;

    $self->selector->add($stdout, $stderr);
}


sub count {
    my ($self) = @_;

    return scalar(keys %{ $self->pid_to_info });
}

sub run {
    my ($self, $left_time) = @_;
    my $cached_bufs = $self->cached_bufs;
    my @name_pids;
    my $start_time;

    die "Bad argument!" unless defined($left_time) && $left_time > 0;
    die "Bad usage!" if $self->count() == 0;


    { # extra brace for "last", see `perldoc perlsyn':  Statement Modifiers
    do {
        $start_time = time();

        my @ready = $self->selector->can_read($left_time);

        return if @ready == 0;  # timeout

        for my $fh (@ready) {
            my $fileno = fileno($fh);
            my $pid = $self->fd_to_pid->{$fileno};
            my $type = $self->fd_to_type->{$fileno};
            my $name = $self->pid_to_info->{$pid}[IDX_NAME];
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
                $buf = _foreach_lines($buf, $out, $name, $pid);
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
                if (-- $self->pid_to_info->{$pid}[IDX_FDCOUNT] <= 0) {
                    delete $self->pid_to_info->{$pid};
                    push @name_pids, $name, $pid;
                }

                $self->selector->remove($fh);
                close($fh);
            }
        }

        last if @name_pids;

        $left_time -= time() - $start_time;
    } while ($left_time > 0);
    } # extra brace for "last", see `perldoc perlsyn':  Statement Modifiers

    return @name_pids;
}


sub name_pids {
}


sub clear {
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
