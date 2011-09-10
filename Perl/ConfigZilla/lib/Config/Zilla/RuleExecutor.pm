package Config::Zilla::RuleExecutor;
use strict;
use warnings;
use utf8;
use Any::Moose;

use Config::Zilla::Rule;

has 'rule'      => (is => 'ro', isa => 'Config::Zilla::Rule', required => 1);
has 'state'     => (is => 'ro', isa => 'HashRef', default => sub { {} });

sub prepare {
}

sub execute {
}

sub rollback {
}

no Any::Moose;
__PACKAGE__->meta->make_immutable();
