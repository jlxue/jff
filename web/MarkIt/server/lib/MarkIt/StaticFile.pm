package MarkIt::StaticFile;
use Cwd qw/abs_path/;
use base 'MarkIt::Base';
use constant ROOT   => abs_path('t/www');

our $VERSION = '0.01';

sub setup {
    my ($c) = @_;

    $c->start_mode('view');
    $c->run_modes([qw/view/]);
}

sub view {
    my ($c) = @_;

    my $path = $c->param('path');

    $c->log->info("request static file: [$path]\n");
    if (! defined($path) || $path eq '/' || length($path) == 0) {
        $path = ROOT . '/index.html';
    } else {
        my $p= abs_path(ROOT . '/' . $path);
        if (index($p, ROOT) != 0) {
            $c->error(title => "Stop!", msg => "Can't access " . $p);
            return;
        }

        $path = $p;
    }

    $c->log->info("response static file: [$path]\n");
    if ($path =~ /\.js$/) {
        $c->header_add('-type'  => 'application/javascript');
    }

    $c->stream_file($path);
}

1;

