package MarkIt;
use base 'CGI::Application::Dispatch';
use MarkIt::Configuration;
use strict;
use warnings;


sub dispatch_args {
    return {
        prefix  => 'MarkIt',

        args_to_new => {
            TMPL_PATH   => MarkIt::Configuration::TEMPLATE_DIR,
            PARAMS  => {
            },
        },

        table   => [
            ''          => { app => 'StaticFile' },
            'mark/:rm'  => { app => 'Mark' },
            'captcha'   => { app => 'Captcha', rm => 'create' },
            '*'         => { app => 'StaticFile', '*' => 'path' },
        ],
    };
}

1;

