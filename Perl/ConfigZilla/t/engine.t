#!/usr/bin/perl
use strict;
use warnings;
use Test::More tests => 5;

use Config::Zilla::Engine;
use Config::Zilla::Rule;
use Log::Log4perl qw(:easy);

Log::Log4perl->easy_init({
        level   => $DEBUG,
        file    => ">engine.log",
        layout  => "[%d] %-5p (%P) %F{1}:%-4L - %m%n",
    });


{
    my $engine = Config::Zilla::Engine->new();

    is(keys %{$engine->ruleset}, 0, "empty engine has zero rule");
    ok($engine->run(), "empty engine runs");
}


{
    my $engine = Config::Zilla::Engine->new();
    ok($engine->run(), "another empty engine runs");
}


{
    my $engine = Config::Zilla::Engine->new();
    $engine->addRule(Config::Zilla::Rule->new(name => "rule1",
                                              shortdesc => "this is rule1"));
    ok($engine->run(), "runs one rule");
}

{
    my $engine = Config::Zilla::Engine->new();
    $engine->addRule(Config::Zilla::Rule->new(name => "rule1",
                                              shortdesc => "this is rule1"));
    $engine->addRule(Config::Zilla::Rule->new(name => "rule2",
                                              shortdesc => "this is rule2"));
    ok($engine->run(), "runs two rule");
}

