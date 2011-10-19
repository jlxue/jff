#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;
use Getopt::Long;
use HTTP::Daemon;
use HTTP::Response;
use HTTP::Status qw(:constants :is status_message);
use Socket qw/unpack_sockaddr_in/;

my $port = 80;

GetOptions("port=i" => \$port);

my $d = HTTP::Daemon->new(LocalPort => $port) || die;
print "Please contact me at: <URL:", $d->url, ">\n";

while (my ($c, $peeraddr) = $d->accept) {
    next if fork();

    my ($port, $addr) = unpack_sockaddr_in($peeraddr);
    $addr = inet_ntoa($addr);
    print "Got connection from $addr:$port\n";

    while (my $r = $c->get_request) {
        print STDERR Dumper($r);

        my $resp = HTTP::Response->new(HTTP_OK);
        $resp->content("\n" . scalar(localtime) . "\n");
        $c->send_response($resp);
    }

    $c->close;
    undef($c);
}

