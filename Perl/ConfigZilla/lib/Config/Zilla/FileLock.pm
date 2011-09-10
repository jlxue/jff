package Config::Zilla::FileLock;
use strict;
use warnings;
use utf8;
use threads;
use Log::Log4perl qw/:easy/;
use POSIX qw/:fcntl_h/;

sub new {
    my ($class, $path, $comment) = @_;
    LOGCONFESS "Path not specified!" unless defined $path;
    $comment = "" unless defined $comment;

    my $self = {
        path    => $path,
        opened  => 0,
        pid     => $$,
        tid     => threads->tid,
    };

    # XXX: not safe for NFS file system, see `perldoc -q lock`
    # XXX: sysopen() may have 255 fd limit, see `perldoc -f sysopen`
    my $fd;
    if ($fd = POSIX::open($path, O_WRONLY | O_EXCL | O_CREAT, 0644)) {
        $self->{opened} = 1;

        my $t = time();
        my $s = "$$\nuid=$<, euid=$>, gid=$(, egid=$), time=$t\n" .
            scalar(localtime($t)) . "\n$comment\n";

        POSIX::write($fd, $s, length($s));
        POSIX::close($fd);
    } else {
        WARN("Can't exclusively create file \"$path\": $!");
    }

    bless $self, $class;
}

sub locked {
    my ($self) = @_;

    return $self->{opened} &&
        $$ == $self->{pid} &&
        threads->tid == $self->{tid};
}

sub path {
    my ($self) = @_;

    return $self->{path};
}

sub DESTROY {
    my ($self) = @_;

    if ($self->locked) {
        unlink $self->{path};
    }
}

1;

