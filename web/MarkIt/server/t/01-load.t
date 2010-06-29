#!/usr/bin/perl

# Test to see if the module loads correctly.
use warnings;
use strict;
use Test::More tests => 1;

BEGIN {

    use_ok('MarkIt');

}

diag(

    "Testing MarkIt $MarkIt::VERSION, Perl $], $^X\n",

);
