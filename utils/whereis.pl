#!/usr/bin/perl -w
#
# Find files in directories specified by PATH environment variable.
#
use strict;
use warnings;
use Cwd;

die "Usage: whereis.pl glob_pattern\n  eg.\twhereis.pl \"gcc*\"\n" if @ARGV != 1;

my $sep = ':';
my $dir = '/';

if ($^O =~ /MSWin32/i) {
    $sep = ';';
    $dir = '\\';
}

my @paths = split $sep, $ENV{PATH};
my @expands;
my $cwd;
for (@paths) {
    $cwd = cwd();
    chdir $_ || next;
    @expands = glob($ARGV[0]);
    # if expansion fails, glob() returns the pattern, so @expands == 1
    if (@expands > 0 && -e $expands[0]) {
        print $_, "\n\t", join("\n\t", @expands), "\n";
    }
    chdir $cwd || next;
}

