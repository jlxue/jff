#!/usr/bin/perl
use strict;
use warnings;
use constant NET_SERVER => 'Net::Server::PreForkSimple';

my $server = new MyServer();
$server->run();
exit(0);

#=========================================================
package MyServer;
use base qw(HTTP::Server::Simple::CGI);
use HTTP::Server::Simple::Static;

sub handle_request {
    my ($self, $cgi) = @_;
    return $self->serve_static($cgi, '/');
}


sub net_server {
    return ::NET_SERVER;
}

