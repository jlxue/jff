#!/usr/bin/perl
use strict;
use warnings;
use AnyEvent;
use Getopt::Long;
use IO::Socket;

my $opt_port = 9999;
GetOptions('port=i'     => \$opt_port);
die "Bad port $opt_port!" if $opt_port < 0 || $opt_port >= 65535;

my $listen_sock = IO::Socket::INET->new(
    Blocking    => 1,
    Listen      => 10,
    LocalPort   => $opt_port,
    Proto       => 'tcp',
) or die "Can't create socket: $!";

$listen_sock->listen or die "Can't listen socket: $!";

while (my $sock = $listen_sock->accept) {
    print "$sock\n";
    sleep 2;
}

