#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../lib";

use Event::Plumber;
use JSON;
use LWP::UserAgent;
use Test::More tests => 1;

use Test::Deep;             # for cmp_deeply()
use Test::Differences;      # for eq_or_diff()

{
    my $ua = LWP::UserAgent->new();
    my $plumber = Event::Plumber->new();
    my $request = $plumber->createExecRequest("echo", 111);
    my $response = $plumber->parseResponse($ua->request($request));

    my $expected_response = ["ok", ["ok", 111]];
    #is_deeply($response, $expected_response, "echo 111");
    #cmp_deeply($response, $expected_response, "echo 111");
    eq_or_diff($response, $expected_response, "echo 111");
}

