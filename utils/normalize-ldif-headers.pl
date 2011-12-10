#!/usr/bin/perl
# Purpose:
#
# Convert
# header: value1
#  value2
# to
# header: value1value2
#
# Reference:
#   /usr/share/doc/ldap-utils/README.Debian
#
use strict;
use warnings;

local $/ = ' ';
while (<>) {
    s/\n //;
    print;
}

