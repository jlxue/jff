package MarkIt::Base;

use warnings;
use strict;
use base 'Titanium';
use Carp qw( croak );
use Log::Dispatch::File;

our $VERSION = '0.01';

sub cgiapp_prerun {
    my ($c) = @_;
}

sub cgiapp_postrun {
    my ($c, $output_ref) = @_;

    $c->header_add('-Access_Control_Allow_Origin' => '*');
}

1;

