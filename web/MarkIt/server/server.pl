#!/usr/bin/perl
use warnings;
use strict;
use CGI::Application::Server;
use lib 'lib';
use MarkIt;

my $server = CGI::Application::Server->new();
$server->document_root('./t/www');
$server->entry_points({
    '/' => 'MarkIt',
});

$server->run;

