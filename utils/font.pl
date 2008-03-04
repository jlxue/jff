#!/usr/bin/perl -w
# 写了个脚本查找匹配的中英文字体，对于 bitstream vera sans mono 14 和它
# 匹配的是 simsun 17（simsun 18 也匹配的，大小一样），这样能保证 Emacs
# 中英文混杂时能对齐。
# 
# 使用办法：
# perl font.pl "Bitstream Vera Sans Mono"  14
# 查看这个字体输出的字母 "ll" 宽度，第二个参数默认是 14.
# 
# perl font.pl "Bitstream Vera Sans Mono" 14 SimSun
# 查找与 Bitstream Vera Sans Mono 14 最匹配的 SimSun 字体大小，范围是英文
# 字体大小加减 5.
# 
# 字体名可以是 ttf 字体文件所在路径名，或者 fc-list 列出的字体名，详情
# 见 perldoc GD，脚本如下，Debian 系下需要安装 libgd-gd2-perl。
#
# 只适用于 TrueType 字体和支持 freetype 和 xft 的 Emacs unicode-2 分支。
#
use strict;
use warnings;
#use File::Slurp;
use GD;

my $im = new GD::Image(100, 100);
my $white = $im->colorAllocate(255,255,255);
my $black = $im->colorAllocate(0,0,0);
$im->transparent($white);

my $hasfontconfig = $im->useFontConfig(1);
print STDERR $hasfontconfig ? "support font-config\n" : "don't support font-config\n";

my $enfont = shift;
my $ensize = shift;
my $zhfont = shift;
$ensize ||= 14;
$ensize = 6 if $ensize < 6;
die "Usage: perl font.pl <en_font> [en_fontsize] [zh_font]\n"if !$enfont;

my @bounds = $im->stringFT($black, $enfont, $ensize, 0, 10, 50, 'll');
die "Can't draw with this font: $enfont, $ensize: $@\n" if @bounds == 0;
my $enwidth = $bounds[2] - $bounds[6];
print "$enfont, size=$ensize, width('ll')=$enwidth\n";

exit 0 if !$zhfont;

my ($zhsize, $zhwidth, $size, $width, $bias);
$zhsize = $zhwidth = 0;
$bias = 100;
for $size ($ensize - 5 .. $ensize + 5) {
    @bounds = $im->stringFT($black, $zhfont, $size, 0, 10, 50, 'll');
    next if @bounds == 0;
    $width = $bounds[2] - $bounds[6];
    if ($bias > abs($width - $enwidth)) {
        $bias = abs($width - $enwidth);
        $zhsize = $size;
        $zhwidth = $width;
    }
}

print "$zhfont, size=$zhsize, width('ll')=$zhwidth\n";

#$im->rectangle($bounds[6], $bounds[7], $bounds[2], $bounds[3], $black);
#write_file("testfont.png", {binmode => ':raw'}, $im->png);

