package Config::Zilla;
use strict;
use warnings;
use Config::Zilla::Engine;
use Config::Zilla::Rule;
use Config::Zilla::FileRule;
use Config::Zilla::PackageRule;

my $czil = Config::Zilla::Engine->new();

sub Rule {
    my $rule = Config::Zilla::Rule->new(@_);
    $rule->validate();
    $czil->addRule($rule);
}

sub FileRule {
    my $rule = Config::Zilla::FileRule->new(@_);
    $rule->validate();
    $czil->addRule($rule);
}

sub PackageRule {
    my $rule = Config::Zilla::PackageRule->new(@_);
    $rule->validate();
    $czil->addRule($rule);
}

1;

