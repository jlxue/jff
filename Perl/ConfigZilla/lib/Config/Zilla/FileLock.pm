package Config::Zilla::FileLock;
use strict;
use warnings;
use utf8;
use POSIX qw/:fcntl_h/;

sub new {
    my ($class, $path) = @_;

    # XXX: not safe for NFS file system, see `perldoc -q lock`
    if (sysopen my $fh, "$path", O_WRONLY | O_EXCL | O_CREAT, 0644) {
        my $t = time();

        syswrite $fh, "$$\nuid=$<, euid=$>, gid=$(, egid=$), time=$t\n" .
            scalar(localtime($t)) . "\n";
    } else {
        $path = undef;
    }

    bless \$path, $class;
}

sub locked {
    my ($self) = @_;

    return defined $$self;
}

sub DESTROY {
    my ($self) = @_;

    if ($self->locked) {
        unlink $$self;
    }
}

1;

