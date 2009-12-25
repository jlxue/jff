#!/usr/bin/perl -w
use Text::WrapI18N;
use Getopt::Long;

use strict;
use warnings;

my $para = '';
my $columns = $Text::WrapI18N::columns;
my $indent1 = "    ";
my $indent2 = "";

GetOptions("width=i"    => \$columns,
           "indent1=s"  => \$indent1,
           "indent2=s"  => \$indent2);
$Text::WrapI18N::columns = $columns if $columns > 0;

while (<>) {
    if (/^\s*$/) {
        if (length($para) > 0) {
            print wrap($indent1, $indent2, $para), "\n";
            $para = '';
        }
        print "\n";
    } else {
        chomp;
        $para .= ' ' if $para =~ /[[:alpha:]]$/ && /^[[:alpha:]]/;
        $para .= $_;
    }
}

