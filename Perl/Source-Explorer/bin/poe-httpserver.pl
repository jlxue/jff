#!/usr/bin/perl
#
# See perldoc POE::Kernel 'Using POE with Other Event Loops'.
#
# POE_EVENT_LOOP=POE::Loop::EV ./poe-httpserver.pl
#

BEGIN {
    eval {
        require POE::XS::Queue::Array;
        POE::XS::Queue::Array->import();
    };
}

use POE qw/Component::Server::HTTPServer/;

my $server = new POE::Component::Server::HTTPServer(
    port        => 8080,
    handlers    => ['/' => new_handler('StaticHandler', '/',
                                       auto_index => 1)],
    log_file    => '/dev/null',
);

my $svc = $server->create_server();
POE::Kernel->run();
exit 0;

