#!/usr/bin/perl
# A preforked http server.
#
# Reference:
#   http://www.stonehenge.com/merlyn/WebTechniques/col34.html

use Fcntl qw/:flock/;
use HTTP::Daemon;
use HTTP::Status;
use Parallel::Prefork;
use strict;
use warnings;

my $pm = Parallel::Prefork->new({
        max_workers     => 10,
        trap_signals    => {
            HUP     => 'TERM',
            TERM    => 'TERM',
            USR1    => undef,
        }
    });

my $master = new HTTP::Daemon(
        Listen      => SOMAXCONN,
        LocalAddr   => 'localhost',
        LocalPort   => 9999) || die "$!\n";

print "Please contact me at: <URL:", $master->url(), ">\n";

while ($pm->signal_received() ne 'TERM') {
    $pm->start() and next;

    eval {
        worker_loop($master);
    };

    print $@ if $@;
    $pm->finish();
}

$pm->wait_all_children();

1;

#------------------------------------
sub worker_loop {
    my $d = shift;

    while (1) {
        flock($d, LOCK_EX) || die "$!\n";
        my $c = $d->accept();
        flock($d, LOCK_UN) || die "$!\n";

        next if !defined $c;

        while (my $r = $c->get_request) {
            if ($r->method eq 'GET' && -f $r->uri->path) {
                $c->send_file_response($r->uri->path);
            } else {
                $c->send_error(RC_FORBIDDEN);
            }
        }

        $c->close();
        undef $c;
    }
}

