#!/usr/bin/perl -w
#
# Purpose:
#   Check the precedence order of dynamic libraries(or shared objects)
# in GCC linkage time. For example:
#     a.o depends b.so
#     b.o depends a.so
#     c.o depends c.so
#     a.so depends c.so
#     b.so depends a.so
# then this little script will tell you '-lb -la -lc'.
#
#
# Usage:
#   $ find myprj -iname "*.o" > obj_list
#   $ find myprj -iname "*.so" > lib_list
#   $ find 3rd/lib -iname "*.so" >> lib_list
#   $ ./sort_libs.pl obj_list lib_list >ld_flags.txt 2>log.txt
#   
#   If circular dependence problem occurs, you can provide a list
#   to tell sort_libs.pl some libraries don't depend other libraries,
#   for example:
#
#   # break the dependence of libfoo.so on libbar.so
#   #
#   $ cat > dep_break_list <<EOF
#   path/to/libfoo.so -> path/to/libbar.so
#   EOF
#   $ ./sort_libs obj_list lib_list dep_break_list >ld_flags.txt 2>log.txt
#
#   If a `--dot=<file>' option provided, a .dot file describes the dependence
#   between all required libraries will be generated, then you can issue this
#   command to get a PNG image(need GraphViz):
#
#   $ dot -Tpng -o dep.png dep.dot
#
#   Use `--debug=1' to enable verbose message.
#
#
# Author:
#   Liu Yubao <yubao.liu@gmail.com>
#
# License:
#   GPL v3
#
# ChangeLog:
#   2008-02-26
#       * initial version, v1.0
#   2008-02-27
#       * fix a big bug: we should sort by topological order not by degree
#       * support dependence break list to solve circular dependence problem
#       * support dot file output for library dependence
#       * release v2.0
#
use strict;
use warnings;
use Data::Dumper;
use Getopt::Long;

die "sort_libs.pl obj_list lib_list [dep_break_list]\n" if @ARGV < 2;

my ($obj_list, $lib_list, $dep_break_list) = @ARGV;
my %objs_symbols = dump_list($obj_list);    # all object files.
my %libs_symbols = dump_list($lib_list);    # all dynamic libraries.
my %libs_graph = ();                        # graph of library dependence.

my $debug = 0;
my $dot_file;

GetOptions('--dot=s'    => \$dot_file,
           '--debug=i'  => \$debug);

print STDERR Data::Dumper->Dump([\%objs_symbols, \%libs_symbols],
                                [qw(*objs_symbols *libs_symbols)]) if $debug > 0;


my %libs_refcnt = ();   # reference counts for dynamic libraries.

# find all direct depended dynamic libraries by object files.
while (my ($obj, $symbols) = each %objs_symbols) {
    my %libs_depended = find_direct_depended_libs($symbols, \%libs_symbols);

    for my $lib (keys %libs_depended) {
        if (! exists $libs_refcnt{$lib}) {
            $libs_refcnt{$lib} = 0;
            $libs_graph{$lib} = {};
        }
    }
}


# flags to tell whether we have searched all direct depended libraries of a library.
my %libs_searched = ();
# flag to tell whether we have checked all libraries for their direct dependence.
my $got_new;

while (1) {
    $got_new = 0;

    for my $lib (keys %libs_refcnt) {
        if (exists $libs_searched{$lib}) {
            next;   # we have checked all direct dependence of this library.
        } else {
            $libs_searched{$lib} = 1;
            $got_new = 1;
        }

        # find all direct depended dynamic libraries by this library.
        my %libs_depended = find_direct_depended_libs($libs_symbols{$lib}, \%libs_symbols);

        # $lib depends $lib2.
        for my $lib2 (keys %libs_depended) {
            if (exists $libs_refcnt{$lib2}) {
                ++$libs_refcnt{$lib2};
            } else {
                $libs_refcnt{$lib2} = 1;    # we get a library to check in next 'while' loop.
            }

            # construct the graph
            if (exists $libs_graph{$lib2}) {
                $libs_graph{$lib2}->{$lib} = 1;
            } else {
                $libs_graph{$lib2} = {$lib => 1};
            }
        }
    }

    last if ! $got_new; # we have checked all libraries for their direct dependence.
}
print STDERR Data::Dumper->Dump([\%libs_refcnt, \%libs_searched, \%libs_graph],
                                [qw(*libs_refcnt *libs_searched *libs_graph)]) if $debug > 0;


my @libs_required = sort { $libs_refcnt{$a} <=> $libs_refcnt{$b} } keys %libs_refcnt;

print STDERR "\nRequired libraries and their reference counts from other libraries:\n\n";
for my $lib (@libs_required) {
    print STDERR " ", $lib, " => ", $libs_refcnt{$lib}, "\n";
}
print STDERR "\n";


# break circular dependence
break_dependence(\%libs_graph, $dep_break_list) if defined $dep_break_list;

gen_dot_file(\%libs_graph, $dot_file) if defined $dot_file;

# output libraries by topological order and alphabetical order.
while (1) {
    $got_new = 0;
    
    for my $lib (sort { get_lib_name($a) cmp get_lib_name($b) } keys %libs_graph) {
        my $depends = $libs_graph{$lib};
        if (0 == scalar(keys %$depends)) {
            print "-l", get_lib_name($lib), "\n";

            delete $libs_graph{$lib};
            $got_new = 1;
            for $depends (values %libs_graph) {
                delete $depends->{$lib} if exists $depends->{$lib};
            }
            last;
        }
    }

    last if ! $got_new;
}

if (0 != scalar(keys %libs_graph)) {
    print STDERR "ERROR: circular dependence found:\n";
    print STDERR Data::Dumper->Dump([\%libs_graph], [qw(*libs_graph)]);
}



################################################################################
# object file list -> array of object file.
#
sub read_file {
    my $file = shift;
    my @lines;

    open my $fh, $file || die "Can't open $file: $!\n";
    @lines = <$fh>;
    close($fh);
    chomp(@lines);

    return @lines;
}


# object file -> hash_ref of symbol name to symbol attribute.
#
sub dump_symbols {
    my $file = shift;
    my $fh;
    my %symbols = ();

    if ($file =~ /\.o$/) {
        open $fh, "nm $file|" || die "Can't open $file: $!\n";
    } elsif ($file =~ /\.so$/) {
        open $fh, "nm -D $file|" || die "Can't open $file: $!\n";
    } else {
        die "$file is not an object file or dynamic library.\n";
    }

    while (<$fh>) {
        chomp;
        my $i = rindex $_, ' ';
        $symbols{substr($_, $i + 1)} = substr($_, $i - 1, 1);
    }
    close($fh);

    return \%symbols;
}


# object file list -> hash of object file to hash_ref of symbol name
# to symbol attribute.
#
sub dump_list {
    my $list = shift;
    my @objs = read_file($list);
    my %objs_symbols = ();

    for my $obj (@objs) {
        $objs_symbols{$obj} = dump_symbols($obj);
    }

    return %objs_symbols;
}


# find direct depended dynamic libraries by an object file.
#
sub find_direct_depended_libs {
    my ($obj_symbols, $libs_symbols) = @_;
    my %libs_depended = ();

    while (my ($name, $attr) = each %$obj_symbols) {
        if ('U' eq $attr) {
            while (my ($lib, $symbols) = each %$libs_symbols) {
                $libs_depended{$lib} = $symbols if
                    exists $symbols->{$name} &&
                    'T' eq $symbols->{$name};
            }
        }
    }

    return %libs_depended;
}


sub get_lib_name {
    my $path = shift;

    $path =~ s/^.*lib//;
    $path =~ s/\.so$//;

    return $path;
}


sub break_dependence {
    my ($libs_graph, $file) = @_;
    my ($a, $b, $fh);

    open $fh, $file || die "Can't open $file: $1\n";
    while (<$fh>) {
        chomp;
        ($a, $b) = split /,|(?:->)|(?:=>)/;
        next if ! defined $b;
        $a =~ s/^\s*|\s*$//g;
        $b =~ s/^\s*|\s*$//g;

        # $a doesn't depend $b:
        my $depends = $libs_graph->{$b};
        delete $depends->{$a} if exists $depends->{$a};
    }
    close($fh);
}


sub gen_dot_file {
    my ($libs_graph, $file) = @_;
    my $fh;

    die "Can't overwrite $file, delete manually it first!" if -f $file;

    open $fh, ">$file" || die "Can't write to $file: $!\n";
    print $fh <<EOF;
strict digraph lib_dependence {
    center=1;
    splines=true;

EOF
    while (my ($lib, $depends) = each %$libs_graph) {
        for my $lib2 (sort keys %$depends) {
            print $fh '    "', get_lib_name($lib2), "\"\t\t-> \"",
                  get_lib_name($lib), "\"\t\t;\n";
        }
    }
    print $fh "}\n\n";

    close($fh);
}

# vi: et ts=4 sw=4

