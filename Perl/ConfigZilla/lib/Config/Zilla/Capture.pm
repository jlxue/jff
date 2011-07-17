package Config::Zilla::Capture;
use strict;
use warnings;
use Carp qw/confess/;
use Exporter 'import';
use File::Spec;
use IO::File;
use IO::Pipe;

our $VERSION = 0.1;
our @EXPORT_OK = qw/capture/;

sub capture(&) {
    my ($code) = @_;
    my $nullfh = IO::File->new(File::Spec->devnull);
    my $stdoutpipe = IO::Pipe->new();
    my $stderrpipe = IO::Pipe->new();

    my $pid = fork();
    return unless defined $pid;

    if ($pid) { # parent process
        $stdoutpipe->reader();
        $stderrpipe->reader();

        return ($pid, $stdoutpipe, $stderrpipe);
    } else {    # child process
        untie *STDIN;
        untie *STDOUT;
        untie *STDERR;

        $stdoutpipe->writer();
        $stderrpipe->writer();

        open STDIN, '<&', $nullfh->fileno() or confess "Can't redirect stdin($$): $!";
        open STDOUT, '>&', $stdoutpipe->fileno() or confess "Can't redirect stdout($$): $!";
        open STDERR, '>&', $stderrpipe->fileno() or confess "Can't redirect stdout($$): $!";

        $code->();
    }
}

1;
