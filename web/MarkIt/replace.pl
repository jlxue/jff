#!/usr/bin/perl
use strict;
use warnings;

my $mark = shift;
my $string;

open F, $ARGV[0] or die "Can't open $ARGV[0] to read: $!\n";
shift;

{
    local $/;
    $string = <F>;
}
close F;

while (<>) {
    s/$mark/$string/g;
    print;
}

