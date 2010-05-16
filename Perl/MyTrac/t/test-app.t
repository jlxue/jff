#!/usr/bin/perl

# Test application functionality
use strict;
use warnings;
use Test::More tests => 1;
use Test::WWW::Mechanize::CGIApp;
use MyTrac;

my $mech = Test::WWW::Mechanize::CGIApp->new;

$mech->app(
    sub {
        my $app = MyTrac->new(PARAMS => {

        });
        $app->run();
    }
);

$mech->get_ok(q{/});
