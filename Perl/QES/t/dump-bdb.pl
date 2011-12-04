#!/usr/bin/env perl
use strict;
use warnings;
use BerkeleyDB;
use Data::Dumper;

tie my %h, "BerkeleyDB::Hash",
    -Filename   => $ARGV[0],
    -Flags      => DB_RDONLY
        or die "Can't open $ARGV[0]: $! $BerkeleyDB::Error";

print Dumper(\%h);
