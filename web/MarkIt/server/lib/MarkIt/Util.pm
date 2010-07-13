package MarkIt::Util;

use base 'Exporter';
use strict;
use warnings;

our $VERSION = '0.01';

our @EXPORT_OK = qw/trim/;


sub trim {
    for (@_) {
        s/^\s+|\s+$//g if defined;
    }
}

1;

