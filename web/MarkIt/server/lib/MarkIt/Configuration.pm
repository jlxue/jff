package MarkIt::Configuration;

use strict;
use warnings;

use constant {
    ROOT_DIR            => "t/www",
    TEMPLATE_DIR        => "lib/MarkIt/templates",
    DB_SOURCE           => "dbi:SQLite:dbname=./markit.db",
    DB_USERNAME         => "",
    DB_PASSWORD         => "",
};


1;

