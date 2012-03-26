#!/usr/bin/perl
#
# Usage:
#  ./contrib/visualize-service-dependency.pl service-dependence.txt |
#       dot -Grankdir=BT -Tsvg -o a.svg
#
use strict;
use warnings;

print "digraph g {\n";

while (<>) {
    next if /^\s*(?:#|$)/;

    s/^\s*|\s*$//g;

    my ($a, @a) = split /[\s:]+/;
    for (@a) {
        print "\t$a -> $_;\n";
    }
}

print "}\n";

