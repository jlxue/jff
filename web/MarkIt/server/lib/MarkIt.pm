package MarkIt;
use base 'CGI::Application::Dispatch';
use strict;
use warnings;

sub dispatch_args {
    return {
        prefix  => 'MarkIt',

        args_to_new => {
            TMPL_PATH   => 'templates',
            PARAMS  => {
            },
        },

        table   => [
            'mark/:rm?' => { app => 'Mark' },
            ':path?'    => { app => 'StaticFile' },
        ],
    };
}

1;

