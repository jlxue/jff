#!/usr/bin/perl
use warnings;
use strict;
use CGI::Application::Server;
use lib 'lib';
use MarkItServer;

my $server = CGI::Application::Server->new();
$server->document_root('./t/www');
$server->entry_points({
    '/markit.min.js'         => MarkItServer->new(PARAMS => {}),
});

$server->run;

