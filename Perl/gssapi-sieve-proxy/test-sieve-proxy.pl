#!/usr/bin/perl
#
# Usage:
#   Test against Sieve server:
#   $ kinit dieken
#   $ ./test-sieve-proxy.pl -s smtp.corp.example.com -p 4190 -u dieken
#
#   Test against Sieve proxy without authentication to the proxy:
#   $ kinit dieken
#   $ ./gssapi-sieve-proxy.pl -s smtp.corp.example.com -p 4190 -l 41900  -u dieken &
#   $ ./test-sieve-proxy.pl -s localhost -p 41900
#
use strict;
use warnings;
use Authen::SASL;
use Data::Dumper;
use Getopt::Long;
use Net::ManageSieve;

my $g_sieve_server = "localhost";
my $g_sieve_port   = 41900;
my $g_user = $ENV{USER} || "dieken";

GetOptions(
    "server=s"  => \$g_sieve_server,
    "port=i"    => \$g_sieve_port,
    "user=s"    => \$g_user,
);

my $sieve = Net::ManageSieve->new(
    Host => $g_sieve_server,
    Port => $g_sieve_port,
    Debug => 1
);

if ($g_sieve_server ne 'localhost') {
    my $sasl = Authen::SASL->new(
        mechanism   => "GSSAPI",
        callback    => { authname => $g_user },
        debug       => 8 | 4 | 1);

    my $caps = $sieve->capabilities;
    if (exists $caps->{starttls}) {
        $sieve->starttls();
    }

    my $saslclient = $sasl->client_new("sieve", $g_sieve_server, "noanonymous noplaintext");

    # why the "authname" callback not work?
    $sieve->auth($saslclient, '', $g_user);
}

my @scripts = $sieve->listscripts;
print Dumper(\@scripts);

