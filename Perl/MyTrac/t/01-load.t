#!/usr/bin/perl

# Test to see if the module loads correctly.
use warnings;
use strict;
use Test::More tests => 1;

BEGIN {

    use_ok('MyTrac');

}

diag(

    "Testing MyTrac $MyTrac::VERSION, Perl $], $^X\n",

);
