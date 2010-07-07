package MarkIt::User;
use MarkIt::Configuration;
use base 'MarkIt::Base';

our $VERSION = '0.01';


sub setup {
    my ($c) = @_;

    $c->start_mode('login');
    $c->run_modes([qw/login/]);
}


sub login {
    my ($c) = @_;
}


1;

