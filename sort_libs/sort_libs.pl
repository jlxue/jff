#!/usr/bin/perl -w
#
# Purpose:
#   check the precedence order of dynamic libraries(or shared objects)
# in GCC linkage time. For example:
#     a.o depends b.so
#     b.o depends a.so
#     c.o depends c.so
#     a.so depends c.so
#     b.so depends a.so
# then this little script will tell you '-lb -la -lc'.
#
# Usage:
#   find myprj -iname "*.o" > obj_list
#   find myprj -iname "*.so" > lib_list
#   find 3rd/lib -iname "*.so" >> lib_list
#   ./sort_libs.pl obj_list lib_list >ld_flags.txt 2>log.txt
#
# Author:
#   Liu Yubao <yubao.liu@gmail.com>
#
# License:
#   GPL v3
#
# ChangeLog:
#   2008-02-26
#       * intial version, v1.0
#
use strict;
use warnings;
use Data::Dumper;

die "sort_libs.pl obj_list lib_list\n" if @ARGV < 2;

my ($obj_list, $lib_list) = @ARGV;
my %objs_symbols = dump_list($obj_list);    # all object files.
my %libs_symbols = dump_list($lib_list);    # all dynamic libraries.
#print STDERR Dumper(\%libs_symbols);


my %libs_refcnt = ();   # reference counts for dynamic libraries.

# find all direct depended dynamic libraries by object files.
while (my ($obj, $symbols) = each %objs_symbols) {
    my %libs_depended = find_direct_depended_libs($symbols, \%libs_symbols);

    for my $lib (keys %libs_depended) {
        $libs_refcnt{$lib} = 1 if ! exists $libs_refcnt{$lib};
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

        for my $lib2 (keys %libs_depended) {
            if (exists $libs_refcnt{$lib2}) {
                ++$libs_refcnt{$lib2};
            } else {
                $libs_refcnt{$lib2} = 1;    # we get a library to check in next 'while' loop.
            }
        }
    }

    last if ! $got_new; # we have checked all libraries for their direct dependence.
}


my @libs_required = sort { $libs_refcnt{$a} <=> $libs_refcnt{$b} } keys %libs_refcnt;

print STDERR "\nLibraries and their reference counts:\n\n";
for my $lib (@libs_required) {
    print STDERR " ", $lib, " => ", $libs_refcnt{$lib}, "\n";
}
print STDERR "\n";

for my $lib (@libs_required) {
    my $l = $lib;
    $l =~ s/^.*lib//;
    $l =~ s/\.so$//;
    print "-l$l\n";
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


# vi: et ts=4 sw=4

