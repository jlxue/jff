#!/usr/bin/perl
#
# Generate function dependencies for C source code.
#
# Usage:
#   # prepare tag file
#   cd vim7/src
#   find . -iname "*.[ch]" -o -iname "*.cpp" | sed -e "s/^\.\///" > cscope.files
#   ctags -L cscope.files
#   cscope -bkqu
#
#   # prepare filter file
#   cat > filter
#   fileio.c
#   memfile.c
#   memline.c
#   ^D
#
#   # generate dot file for function dependencies
#   perl funcdep.pl --limit 50 --filter filter > dep.dot
#   echo fileio.c | perl funcdep.pl --filter - > dep.dot
#
#   # generate png picture from dot file
#   dot -Tpng -o dep.png dep.dot
#   twopi -Tpng -o dep.png dep.dot
#
#   NOTICE: backup your `tags' and `cscope.*' files before running this
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
#   2008-03-10
#       * add --filter support.
#       
use strict;
use warnings;
use Data::Dumper;
use Getopt::Long;
use IPC::Open2;

use constant DEBUG => 0;

my $limit = 0;      # maximum number of edges
my $filter;         # a list file contains file:name filter

GetOptions("limit=i"    => \$limit,
           "filter=s"   => \$filter);

$filter = read_filter($filter) if defined $filter;

my %symbols = read_tags("tags");

my %symbols_dependencies = ();
my ($line, $file, $name, $num, $text);
my ($child_out, $child_in);
my $pid = open2($child_out, $child_in, 'cscope', '-dl');


for my $k (keys %symbols) {
    next if defined $symbols{$k}->[4];
    next if defined $filter && $k !~ /$filter/o;

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
    overlap=scale;      // for twopi

EOF

if ($limit <= 0) {
    for my $k (keys %symbols_dependencies) {
        my $depends = $symbols_dependencies{$k};
        for my $k2 (keys %$depends) {
            print '"', short_tag($k2), "\"\t\t-> \"", short_tag($k), "\";\n";
        }
    }
} else {
    my $count = 0;
    my ($got_new, $k, $v, @keys);

    # output by topological order
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
                        print '"', short_tag($k), "\"\t\t-> \"", short_tag($k2), "\";\n";
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


sub read_filter {
    my $pattern = "";
    my $f;

    if ('-' eq $_[0]) {
        open $f, $_[0] || die "Can't open $_[0]: $!\n";
    } else {
        $f = *STDIN;
    }
    while (<$f>) {
        chomp;
        next if 0 == length;
        if (length($pattern) > 0) {
            $pattern .= "|(?:" . quotemeta($_) . ")";
        } else {
            $pattern = "(?:" . quotemeta($_) . ")";
        }
    }
    close $f if '-' ne $_[0];

    return $pattern;
}


sub short_tag {
    my $tag = shift;        # /path/to/a.c:hello
    
    $tag =~ s/.*(?:\\|\/)//;
    return $tag;
}

