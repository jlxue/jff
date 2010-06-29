#!/usr/bin/perl

# Test application functionality
use strict;
use warnings;
use Test::More tests => 1;
use Test::WWW::Mechanize::CGIApp;
use MarkIt;

my $mech = Test::WWW::Mechanize::CGIApp->new;

$mech->app(
    sub {
        my $app = MarkIt->new(PARAMS => {

        });
        $app->run();
    }
);

$mech->get_ok(q{/});
