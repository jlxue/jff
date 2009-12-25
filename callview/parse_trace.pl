#!/usr/bin/perl
use strict;
use warnings;

my $record;
my %h = ();
my @a = ();

open F, "$ARGV[0]" or die "$!\n";
binmode F;

while (sysread(F, $record, 5 * 1024 * 8)) {
    @a = unpack "(cL)*", $record;
    for (my $i = 0; $i < @a; $i += 2) {
        next if $a[$i];

        $h{$a[$i+1]}++;
    }
}
undef @a;
undef $record;

while (my ($k, $v) = each %h) {
    printf "%08x %d\n", $k, $v;
}

close F;

