#!/usr/bin/perl
use strict;
use warnings;

my $record;
my ($exited, $addr);
my %h = ();

open F, "$ARGV[0]" or die "$!\n";
binmode F;

while (sysread(F, $record, 5)) {
    ($exited, $addr) = unpack "cL", $record;

    next if $exited;

    $h{$addr}++;
}

while (my ($k, $v) = each %h) {
    print "$k $v\n";
}

close F;

