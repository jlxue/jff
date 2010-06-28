package MarkItServer;

use warnings;
use strict;
use base 'Titanium';
use Carp qw( croak );
use Log::Dispatch::File;

our $VERSION = '0.01';

sub setup {
    my ($c) = @_;

    $c->start_mode('start');
    $c->run_modes([qw/start/]);
    return;
}

sub start {
    my ($c) = @_;
    #my $file = $c->query->path_info();
    #$c->log->info("got $file");
    my $file = './t/www/markit.min.js';

    $c->header_add('-Access_Control_Allow_Origin' => '*',
                   '-type'  => 'application/javascript');
    $c->stream_file($file);
}

1;

