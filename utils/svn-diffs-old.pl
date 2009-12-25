#!/usr/bin/perl -w
use strict;
use warnings;
use Cwd;
use Encode;
use File::Spec;

######################
my $diff_cmd = 'diff';
my $diff_args = '-u';
#####################

die "Usage: perl svn-diffs.pl URL [fromREV] [toREV]\n" if @ARGV < 1;

my $URL = $ARGV[0];
my $fromREV = $ARGV[1] || 1;
my $toREV = $ARGV[2];

# set en_US locale to make parsing easy
$ENV{LANG} = 'en_US.UTF-8';
$ENV{LC_ALL} = 'en_US.UTF-8';


# test svn whether has been installed
if (!qx/svn --version/) {
    die "Can't execute svn: $!\n";
}


# get max revision number
my ($line, @lines);
open(F, "svn info $URL |") || die "Failed to execute 'svn info': $!\n";
@lines = <F>;
close(F);

($line) = grep /Changed Rev:/, @lines;
($line) = $line =~/(\d+)/;
$toREV = $line if (!defined $toREV) || ($toREV > $line);

print "generate diffs for $URL [$fromREV, $toREV]\n";


# get logs and create patches
my $count = 0;
my ($rev, $author, $date, $n);
my $filename;

open(F, "svn log -r $fromREV:$toREV $URL |") || die "Failed to execute 'svn log': $!\n";
if (<F> !~ /-{72}/) {
    close(F);
    die "No revisions found in this range.\n";
}

while (<F>) {
    ++$count;
    ($rev, $author, $date, $n) = split / \| /;
    $rev = substr $rev, 1;
    ($n) = $n =~ /(\d+)/;
    
    print "diff $count: rev $rev\n";

    # write log
    $filename = "$fromREV-${toREV}_$count.log";
    open(F2, ">$filename") || die "Can't write to $filename: $!\n";
    print F2 $_;    # header
    ++$n;           # include blank line after header
    do {
        print F2 scalar(<F>);
    } while (--$n > 0);
    close(F2);
    scalar(<F>);           # skip '--------------' separator

    # write diff
    $filename = "$fromREV-${toREV}_$count.diff";
    system("svn diff -r " . ($rev - 1) . ":$rev $URL > $filename");
}

close(F);

