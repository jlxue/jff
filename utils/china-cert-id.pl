#!/usr/bin/perl
#
# 计算身份证号码的最后一位校验码
#
# http://luy.li/2011/01/01/sfzhm/
# http://zh.wikipedia.org/zh/中华人民共和国公民身份号码

use strict;
use warnings;

my ($addr_code, $birth_code, $seq_code) = @ARGV;
usage() if !defined $seq_code;

my @a = reverse split //, $addr_code . $birth_code . $seq_code . 'X';
usage() if @a != 18;

my $sum = 0;

for (my $i = 1; $i < 18; ++$i) {
    $sum += $a[$i] * ( 2**$i % 11 );
}

$a[0] = 12 - ($sum % 11) % 11;
$a[0] = 'X' if $a[0] == 10;

print "Gender: ", $seq_code % 2 ? "male" : "female", ", ID: ", join('', reverse @a), "\n";

#########################################################
sub usage {
    print STDERR "$0 addr-code birth-code seq-code\n";
    print STDERR "  Example: $0 110102 19900101 001\n";
    exit -1;
}

