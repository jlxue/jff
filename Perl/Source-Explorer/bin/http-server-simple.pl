#!/usr/bin/perl
use HTTP::Server::Simple;
use strict;
use warnings;

my $server = new HTTP::Server::Simple;

$server->run();

