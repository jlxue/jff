#!/usr/bin/perl -w
use strict;
use warnings;
use POSIX;

my $filename = "";
my $in = $ENV{HOME} . "/.tex-auto-o";       # latex write filename to it
my $out = $ENV{HOME} . "/.tex-auto-i";      # latex read result from it

# begin with a clean world
unlink $in;
unlink $out;
mkfifo($in, 0644) || die "$in: $!\n";
mkfifo($out, 0644) || die "$out: $!\n";

while (1) {
    open(IN, $in) || die "$!";
    open(OUT, ">" . $out) || die "$!";

    while (sysread(IN, $filename, PATH_MAX)) {
        syswrite(STDERR, $filename);
        if (0 == install_package_for($filename)) {
            syswrite(OUT, "ok\n");
        } else {
            syswrite(OUT, "failed\n");
        }
    }

    close(IN);
    close(OUT);
}


#######################################################
#
sub install_package_for {
    my $filename = shift;

    open(F, ">$filename") || return -1;
    print F "$filename: hello\n" x 10;
    close F;
    sleep 1;

    return 0;
}


