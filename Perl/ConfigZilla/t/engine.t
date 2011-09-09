#!/usr/bin/perl
use strict;
use warnings;
use Test::More tests => 2;

use Config::Zilla::Engine;
use Log::Log4perl qw(:easy);

Log::Log4perl->easy_init({
        level   => $DEBUG,
        file    => ">engine.log",
        layout  => "[%d] %-5p (%P) %F{1}:%-4L - %m%n",
    });

my $engine = Config::Zilla::Engine->new();

is(keys %{$engine->ruleset}, 0, " empty engine has zero rule");
ok($engine->run(), " empty engine runs successfully");

