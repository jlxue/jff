#!/usr/bin/perl
#
# Usage:
#
# fc-list :lang=zh | cut -d, -f1 | cut -d: -f1 | sort | perl fontshow.pl 16 hello world 中国人民你好 > a.html
# firefox a.html
#
use strict;
use warnings;

my ($size, @strings) = @ARGV;

if (@strings == 0) {
    print STDERR "Usage: $0 font-size string... < fonts-list";
    exit 1;
}

while (<STDIN>) {
    chomp;
    print "<font size=\"$size\" face=\"$_\">@strings $_:$size</font><br/>\n";
}

