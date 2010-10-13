#!/usr/bin/perl
#
# Purpose:
#   count popular systems by dependence collected by quiklisp project
#
# Usage:
#   See `perl quiklisp-popular.pl --help` for details.
#
# Author:
#   Yubao Liu <yubao.liu@gmail.com>
#
# License:
#   GPL v3
#
# ChangeLog:
#   2010-10-13  Yubao Liu
#       * initial release

use strict;
use warnings;
use Getopt::Long;


# whether to show verbose message
my $opt_verbose = 0;
# whether to skip dependencice in same project
my $opt_distinct = 0;
# everse the result
my $opt_reverse = 0;
# which other systems a system depends, "system" -> ["dep1", "dep2"]
my %dependencies = ();
# recursive dependence count by other systems, "system" -> 233
my %dep_count = ();
# which project a system belongs to, "system" -> "project"
my %belongs = ();


GetOptions("distinct"  => \$opt_distinct,
           "verbose"   => \$opt_verbose,
           "reverse"   => \$opt_reverse,
           "help|?"    => \&usage
          ) or usage();

push @ARGV, "$ENV{HOME}/quicklisp/dists/quicklisp/systems.txt" if @ARGV == 0;


while (<>) {
    next if /^\s*#/;

    my @a = split;
    $belongs{$a[2]} = $a[0];

    next if @a <= 3;    # no external dependencies

    $dependencies{$a[2]} = [ @a[3..$#a] ];
}


while (my ($system, $deps) = each %dependencies) {
    count($system, @$deps);
}


my @systems = $opt_reverse ? sort { $dep_count{$b} <=> $dep_count{$a} } keys %dep_count
                           : sort { $dep_count{$a} <=> $dep_count{$b} } keys %dep_count;
for my $sys (@systems) {
    printf "%6d %s\n", $dep_count{$sys}, $sys;
}

#################################################################################
sub count {
    my ($system, @deps) = @_;

    for my $sys (@deps) {
        if (! exists $belongs{$sys}) {
            warn "WARNING: $system depends $sys but $sys isn't included by quicklisp!\n";
        } elsif (! $opt_distinct) {
            if ($belongs{$system} eq $belongs{$sys}) {
                print STDERR "In same project $belongs{$system}: $system $sys\n" if $opt_verbose;
                next;
            }

            if ($belongs{$system} =~ /^((?:cl-)?[\w-]+)/) {
                my $prefix = $1;
                if ($belongs{$sys} =~ /^((?:cl-)?[\w-]+)/ && $prefix eq $1) {
                    print STDERR "In same super project $prefix ($belongs{$system} $belongs{$sys}): $system $sys\n" if $opt_verbose;
                    next;
                }
            }
        }

        $dep_count{$sys}++;

        if (exists $dependencies{$sys}) {
            my $deps = $dependencies{$sys};
            count($sys, @$deps);
        }
    }
}

sub usage {
    print <<EOF;
Usage:
    perl quicklisp-popolar [OPTIONS] [path/to/quicklisp/dists/quicklisp/systems.txt]

    Options:
        --help              show this help information
        --distinct          don't skip dependencies in same project
        --reverse           reverse the result
        --verbose           show verbose message
EOF

    exit 1;
}

