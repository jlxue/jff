package MarkIt::StaticFile;

use base 'MarkIt::Base';

our $VERSION = '0.01';

sub setup {
    my ($c) = @_;

    $c->start_mode('view');
    $c->run_modes([qw/view/]);
}

sub view {
    my ($c) = @_;

    my $path = $c->param('path');

    $c->log->info("got [$path]\n");
    my $file = './t/www/markit.min.js';

    if ($file =~ /\.js$/) {
        $c->header_add('-type'  => 'application/javascript');
    }

    $c->stream_file($file);
}

1;

