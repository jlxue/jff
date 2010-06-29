package MarkIt::Mark;

use base 'MarkIt::Base';

our $VERSION = '0.01';

sub setup {
    my ($c) = @_;

    $c->start_mode('view');
    $c->run_modes([qw/view/]);
}

sub view {
    my ($c) = @_;

    return "hello world";
}

1;

