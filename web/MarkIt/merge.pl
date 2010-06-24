#!/usr/bin/perl
use strict;
use warnings;

my $html = shift;
open F, $html or die "Can't open $html to read: $!\n";
my @a = <F>;
chomp @a;
close F;

my $a = join("' +\n'", @a);
while (<>) {
    s/##DIALOG_HTML##/$a/;
    print
}

