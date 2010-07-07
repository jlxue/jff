#!/usr/bin/perl
use warnings;
use strict;
use CGI::Application::Server;
use lib 'lib';
use MarkIt;
use MarkIt::Configuration;

*{CGI::Application::Server::valid_http_method} = sub {
    my $self   = shift;
    my $method = shift or return 0;
    return $method =~ /^(?:GET|POST|OPTIONS|HEAD|PUT|DELETE)$/;
};

my $server = CGI::Application::Server->new();
$server->document_root(MarkIt::Configuration::ROOT_DIR);
$server->entry_points({
    '/' => 'MarkIt',
});

$server->run;

