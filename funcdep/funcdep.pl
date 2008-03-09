#!/usr/bin/perl
#
# Generate function dependencies for C source code.
#
# Usage:
#   cd vim7/src
#   bash funcdep.sh
#   perl funcdep.pl --limit=50 > dep.dot
#   dot -Tpng dep.png dep.dot
#
#   Note: backup your `tags' and `cscope.*' files before running this
#   script.
#
# License:
#   GPLv3
#
# Author:
#   Liu Yubao <yubao.liu@gmail.com>
#
# ChangeLog:
#   2008-03-09
#       * initial version.
#       
use strict;
use warnings;
use Data::Dumper;
use Getopt::Long;
use IPC::Open2;

use constant DEBUG => 0;

my $limit = 0;      # maximum number of edges

GetOptions("limit=i"    => \$limit);

my %symbols = read_tags("tags");

my %symbols_dependencies = ();
my ($line, $file, $name, $num, $text);
my ($child_out, $child_in);
my $pid = open2($child_out, $child_in, 'cscope', '-dl');

for my $k (keys %symbols) {
    next if defined $symbols{$k}->[4];

    # calls `cscope -dl < 3myfunc`.
    print $child_in 3, $symbols{$k}->[0], "\n";
    my ($n) = scalar(<$child_out>) =~ /(\d+)/;
    #print STDERR "$total\t", $symbols{$k}->[1], ':', $symbols{$k}->[0], "\n";

    $symbols_dependencies{$k} = {};
    while ($n-- > 0) {
        $line = <$child_out>;
        chomp $line;
        ($file, $name, $num, $text) = split / /, $line, 4;
        next if ($file eq $symbols{$k}->[1] || !exists $symbols{"$file:$name"} ||
                 defined $symbols{"$file:$name"}->[4]);
        # remove duplications
        $symbols_dependencies{$k}->{"$file:$name"} = 1;
    }
}

close($child_in);
close($child_out);

print <<EOF;
strict digraph func_dependence {
    center=1;
    splines=true;

EOF

if ($limit <= 0) {
    for my $k (keys %symbols_dependencies) {
        my $depends = $symbols_dependencies{$k};
        for my $k2 (keys %$depends) {
            print '"', $k2, "\"\t\t-> \"$k\";\n";
        }
    }
} else {
    my $count = 0;
    my ($got_new, $k, $v, @keys);
    while (1) {
        $got_new = 0;
        @keys = keys %symbols_dependencies;
        
        for $k (@keys) {
            $v = $symbols_dependencies{$k};
            if (0 == scalar(keys %$v)) {
                $got_new = 1;
                delete $symbols_dependencies{$k};
                while (my ($k2, $v2) = each %symbols_dependencies) {
                    if (exists $v2->{$k}) {
                        last if ++$count >= $limit;
                        delete $v2->{$k};
                        print '"', $k, "\"\t\t-> \"$k2\";\n";
                    }
                }
            }
            last if $count >= $limit;
        }

        last if !$got_new || $count >= $limit;
    }
    print STDERR "left: ", scalar(keys %symbols_dependencies), "\n";
}

print "\n}\n\n";



############################################################################
#
sub read_tags {
    my $tags_file = shift;
    my ($name, $file, $position, $type, $scope);
    my ($a, $b, $i);
    my %symbols = ();

    open my $f, $tags_file || die "Can't open $tags_file: $!\n";

    while (<$f>) {
        next if /^!/;

        chomp;
        $i = rindex $_, ";\"\t";
        next if $i < 0;
        $a = substr $_, 0, $i;
        $b = substr $_, $i + 3;

        ($name, $file, $position) = split /\t/, $a, 3;
        ($type, $scope) = split /\t/, $b, 2;

        if (defined $scope) {
            debug("$name\t$file\t$position;\"\t$type\t$scope\n");
        } else {
            debug("$name\t$file\t$position;\"\t$type\n");
        }

        if ('f' eq $type) {
            $symbols{"$file:$name"} = [$name, $file, $position, $type, $scope];
        }
    }

    close $f;

    return %symbols;
}


sub debug {
    print @_ if DEBUG;
}

