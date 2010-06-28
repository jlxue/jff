#!/usr/bin/perl

# Test to see if the module loads correctly.
use warnings;
use strict;
use Test::More tests => 1;

BEGIN {

    use_ok('MarkItServer');

}

diag(

    "Testing MarkItServer $MarkItServer::VERSION, Perl $], $^X\n",

);
