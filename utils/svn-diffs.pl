#!/usr/bin/perl -w
#
# svn-diffs.pl:
#   generate a diff series for svn, they can be applied by another
#   little Perl script svn-patches.pl.
#
#   set environment variables "SVNUSER" and "SVNPASSWD" to provide
#   user name and password if needed.
#
# note:
#   svn properties are ignored.
#
# 2007-01-09    Liu Yubao <yubao.liu@gmail.com>
#
use strict;
use warnings;

my $URL = shift;
my $fromREV = shift;
my $toREV = shift;

die "Usage: perl $0 URL [fromREV] [toREV]\n" if !defined $URL;
$fromREV ||= 2;

my $LANG=$ENV{LANG};
$ENV{LANG} = "en_US.UTF-8";     # make parsing easy
my $svncmd = "svn";
if (exists $ENV{SVNUSER} && exists $ENV{SVNPASSWD}) {
    $svncmd .= " --username $ENV{SVNUSER} --password $ENV{SVNPASSWD} ";
}

######################################################
# check whether svn has been installed
#
if (! qx/svn --version/) {
    die "[svn-diffs] Can't execute svn: $!\n";
}

######################################################
# check whether we are in an empty directory, avoid file overwrite by mistake
#
{
    my @dirs = <*>;
    die "[svn-diffs] Please run me in an empty directory.\n" if @dirs > 0;
}

######################################################
# check URL
#
my ($line, @lines);
open F, "$svncmd info $URL |" || die "[svn-diffs] Can't execute 'svn info': $!\n";
@lines = <F>;
close F;

($line) = grep /^Last Changed Rev:/, @lines;
die "[svn-diffs] Error happened when executed 'svn info': $!\n" if !defined $line;
($line) = $line =~ /(\d+)/;

$toREV = $line if (!defined $toREV) || ($toREV > $line);
print "[svn-diffs] generate diffs for $URL [$fromREV, $toREV]\n";


######################################################
# get log and generate diff
#
open F, "$svncmd log -r $fromREV:$toREV $URL |" || die "[svn-diffs] Can't execute 'svn log': $!\n";
$line = <F>;
die "[svn-diffs] Error happened when executed 'svn log': $!\n" if $line !~ /-{72}/;

my ($rev, $author, $date, $n);
my ($count, $file) = (0, "");
while ($line = <F>) {
    print "-----------------------------------------\n";
    ######################### parse log header #################################
    ++$count;
    ($rev, $author, $date, $n) = split / \| /, $line;
    $rev = substr $rev, 1;
    ($n) = $n =~ /(\d+)/;
    ++$n;       # include blank line after log header

    $file = "$fromREV-$toREV-$count";

    ######################### generate log for one revision ####################
    print "write log for revision $rev as '$file.log'...\n";
    open LOG, ">$file.log" || die "[svn-diffs] Can't write $file.log: $!\n";
    print LOG $line;
    do {
        $line = <F>;
        print LOG $line;
    } while (--$n > 0);
    close LOG;
    $line = <F>;        # skip '----------------' log separator

    ######################### generate patch and binary files for one revision #
    print "write diff for revision $rev as '$file.diff'...\n";
    open F2, "$svncmd diff -r " . ($rev - 1) . ":$rev $URL |" || die "[svn-diffs] Can't execute 'svn diff': $!\n";
    open DIFF, ">$file.diff" || die "[svn-diffs] Can't write $file.diff: $!\n";
    my $index;
    my @binary_files = ();
    while ($line = <F2>) {
        print DIFF $line;

        if ($line =~ /^Index: (.*)/) {
            $index = $1;
        } elsif ($line =~ /^Cannot display: file marked as a binary type.$/) {
            push @binary_files, $index;
            print "write binary file '$index' for revision $rev as '$file-$#binary_files.bin'...\n";
            system("$svncmd cat -r $rev $URL/$index > $file-$#binary_files.bin") &&
                    die "[svn-diff] Can't execute 'svn cat': $!\n";
        }
    }
    close DIFF;
    close F2;

    ######################### generate list of binary files ####################
    if (@binary_files > 0) {
        print "write list of binary files for revision $rev as '$file.idx'...\n";
        open IDX, ">$file.idx" || die "[svn-diffs] Can't write $file.idx: $!\n";
        foreach my $f (@binary_files) {
            print IDX "$f\n";
        }
        close IDX;
    }
}
close F;

