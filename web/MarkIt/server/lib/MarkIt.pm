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
            ''          => { app => 'StaticFile' },
            'mark/:rm'  => { app => 'Mark' },
            '*'         => { app => 'StaticFile', '*' => 'path' },
        ],
    };
}

1;

