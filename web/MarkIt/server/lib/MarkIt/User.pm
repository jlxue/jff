package MarkIt::User;
use MarkIt::Configuration;
use base 'MarkIt::Base';

our $VERSION = '0.01';


sub setup {
    my ($c) = @_;

    $c->start_mode('login');
    $c->run_modes([qw/login register key/]);
}


sub login {
    my ($c) = @_;
}

sub register {
}

sub key {
}

1;

