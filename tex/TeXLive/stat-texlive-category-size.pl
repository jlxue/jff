#!/usr/bin/perl -w
#
# Usage:
#   perl stat-texlive-category-size.pl < texlive.tlpdb
#
use strict;
use warnings;
use Data::Dumper;

my %sizes = ();
my %pkg = ();
my ($k, $v, $c);

while (<STDIN>) {
    chomp;

    if (/^$/) {
        for $k (qw/binfiles runfiles docfiles srcfiles
                containersize doccontainersize srccontainersize/) {
            $sizes{$c} = {} if ! exists $sizes{$c};
            $sizes{$c}->{$k} += $pkg{$k} if exists $pkg{$k};
        }
        %pkg = ();
    } else {
        next if ! /^\w/;
        ($k, $v) = split / /, $_, 2;
        if ($k eq 'category') {
            $c = $v;
        } elsif ($k =~ /size$/) {
            $pkg{$k} = $v;
        } elsif ($v =~ /\bsize=(\d+)$/) {
            $pkg{$k} = $1;
        }
    }
}

print Dumper(\%sizes);

