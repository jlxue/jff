#!/usr/bin/perl -w
#
# Usage:
# fc-list :lang=zh | cut -d, -f1 | cut -d: -f1 | sort | perl fontshow2.pl a.png 16 hello world 中国人民你好
# eog a.png
#
use strict;
use warnings;
use File::Slurp;
use GD;
use constant IMAGE_WIDTH => 800;

my $image_file = shift;
my $font_size = shift;

die "Read comment in this script for usage information!" if !defined $font_size;

my $string = join '', @ARGV;
die "Read comment in this script for usage information!" if length($string) == 0;

my @fontnames = <STDIN>;
chomp(@fontnames);
die "Read comment in this script for usage information!" if @fontnames == 0;

my $im = new GD::Image(IMAGE_WIDTH, @fontnames * 3 * $font_size);
my $white = $im->colorAllocate(255,255,255);
my $black = $im->colorAllocate(0,0,0);
#$im->transparent($white);

my $hasfontconfig = $im->useFontConfig(1);
print STDERR $hasfontconfig ? "support font-config\n" : "don't support font-config\n";


my $y = $font_size * 2;
for (@fontnames) {
    my @bounds = $im->stringFT($black, $_, $font_size, 0, 10, $y, "$_:$font_size $string");
    if (@bounds == 0) { warn "Can't draw with $_ at $font_size: $@\n"; next }
    $im->rectangle($bounds[6], $bounds[7], $bounds[2], $bounds[3], $black);
    warn "Too long string: $_ at $font_size\n" if $bounds[2] - $bounds[6] > IMAGE_WIDTH;
    $ y += $bounds[1] - $bounds[5] + $font_size;
}

write_file($image_file, {binmode => ':raw'}, $im->png);

