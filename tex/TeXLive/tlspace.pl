#!/usr/bin/perl
# calculate disk space needed for TeXLive packages
#
# Usage:
#   perl tlspace.pl texlive.tlpdb arch package...
#
#   eg. perl tlspace.pl texlive.tlpdb i386-linux scheme-basic
#
#   You can download texlive.tlpdb from CTAN:
#       /systems/texlive/tlnet/tldev/texlive.tlpdb
#
#   TexLive::* modules is in TeXLive's installer (install-tl.tar.gz)
#
use strict;
use warnings;
use TeXLive::TLPDB;

my $tlpdb = new TeXLive::TLPDB;
$tlpdb->from_file(shift);

my $arch = shift;
my @archs = $tlpdb->available_architectures();
die "available arch: @archs\n" if 0 == grep {$arch eq $_} @archs;

my @pkgs = @ARGV;
my %a = ();

my $docsize = 0;
my $srcsize = 0;
my $runsize = 0;
my $binsize = 0;

while (my $pkg = shift @pkgs) {
    my $pobj = $tlpdb->get_package($pkg);
    if (! defined $pobj) {
        warn "can't find $pkg\n";
        next;
    }

    my @depends = $pobj->depends();
    for my $dep (@depends) {
        $dep =~ s/ARCH$/$arch/;
        if (! exists $a{$dep}) {
            $a{$dep} = 1;
            push @pkgs, $dep;
        }
    }
    $docsize += $pobj->docsize;
    $srcsize += $pobj->srcsize;
    $runsize += $pobj->runsize;
    my $s = $pobj->binsize;
    $binsize += $s->{$arch} if exists $s->{$arch};
}

printf "srcsize: %.2f MB\n", $srcsize * 4 / 1024;
printf "docsize: %.2f MB\n", $docsize * 4 / 1024;
printf "runsize: %.2f MB\n", $runsize * 4 / 1024;
printf "binsize: %.2f MB\n", $binsize * 4 / 1024;

my $size = $srcsize + $docsize + $runsize + $binsize;
printf "total size: %.2f MB\n", $size * 4 / 1024;

