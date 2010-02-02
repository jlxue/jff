#!/usr/bin/perl
# Purpose:
#   decode xprop's output like this to readable string:
#   _NET_WM_NAME(UTF8_STRING) = 0x44, 0x65, 0x62, 0x69
#
# Usage:
#   xprop | ./decode-xprop.pl
#
# Author:
#   Liu Yubao <yubao.liu@gmail.com>
#
# License:
#   BSDL
#
# ChangeLog:
#   2010-02-02  first release, v1.0
#
use strict;
use warnings;

while (<>) {
    if (/^([^=]+= )((?:0x[a-f0-9]{2}, )*0x[a-f0-9]{2})$/) {
        my ($key, $value) = ($1, $2);
        $value =~ s/0x([a-f0-9]{2})/$1/g;
        chomp $value;
        print $key, pack("(H*)*", (split (/, /, $value))), "\n";
    } else {
        print;
    }
}

