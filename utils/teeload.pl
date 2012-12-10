#!/usr/local/bin/perl
#
# Purpose:
#   A HTTP proxy server that response client empty 200 OK and passes
#   incoming traffic to backend servers.
#
# Options and their default values:
#   --debug
#   --max_requests  2000
#   --max_workers 100
#   --keep_host
#   --keep_alive 100
#   --port 4080
#   --server host:port[,host:port]
#   --timeout 1000  # milliseconds
#   --match REGEX
#
#  Dependency:
#   local_perl_compat yprocctl perl-Parallel-Prefork perl-libwww-perl
#
#  Example:
#  $ exmon -e /tmp/teeload.err-%Y%m%d-%H -E /tmp/teeload.err \
#       -o /tmp/teeload.out.%Y%m%d-%H -O /tmp/teeload.out \
#       -i -l /tmp -n teeload -p 200 \
#       ./teeload.pl --port 8888  --debug  --keep_host \
#       --server www.google.com \
#       --server localhost:12345,localhost:9998

use Fcntl qw/:flock/;
use Getopt::Long;
use HTTP::Daemon;
use HTTP::Status;
use LWP::UserAgent;
use Parallel::Prefork;
use strict;
use warnings;

$| = 1;

my @g_backend_servers;
my $g_debug = 0;
my $g_max_requests = 2000;
my $g_max_workers = 100;
my $g_timeout = 1000;   # milliseconds
my $g_port = 4080;
my $g_keep_host = 0;
my $g_keep_alive = 0;
my $g_match;
my $g_ua;

GetOptions("server=s"       => \@g_backend_servers,
           "port=i"         => \$g_port,
           "timeout=i"      => \$g_timeout,
           "max_requests=i" => \$g_max_requests,
           "max_workers=i"  => \$g_max_workers,
           "keep_host!"     => \$g_keep_host,
           "keep_alive:100" => \$g_keep_alive,
           "match=s"        => \$g_match,
           "debug!"         => \$g_debug);
@g_backend_servers = split(/[,; ]/, join(',', @g_backend_servers));
@g_backend_servers = read_servers(@g_backend_servers);

my $pm = Parallel::Prefork->new({
        max_workers     => $g_max_workers,
        trap_signals    => {
            HUP     => 'TERM',
            TERM    => 'TERM',
            USR1    => undef,
        }
    });

my $master = HTTP::Daemon->new(
        Listen      => SOMAXCONN,
        LocalPort   => $g_port,
        ReuseAddr   => 1) || die "$!\n";

print "Please contact me at: <URL:", $master->url(), ">\n";

while ($pm->signal_received() ne 'TERM') {
    $pm->start() and next;

    eval {
        worker_loop($master);
    };

    print STDERR $@ if $@;
    $pm->finish();
}

$pm->wait_all_children();

1;

#------------------------------------
sub worker_loop {
    my $d = shift;

    print "Worker $$ enters, max_requests=$g_max_requests\n" if $g_debug;

    if ($g_keep_alive) {
        $g_ua = LWP::UserAgent->new("keep_alive" => $g_keep_alive);
    } else {
        $g_ua = LWP::UserAgent->new;
    }
    $g_ua->timeout($g_timeout > 1000 ? $g_timeout / 1000 : 1);

    while ($g_max_requests-- > 0) {
        flock($d, LOCK_EX) || die "$!\n";
        my ($c, $peeraddr) = $d->accept();
        flock($d, LOCK_UN) || die "$!\n";

        print "After accept(): $c\n" if $g_debug;

        next if !defined $c;

        my ($port, $addr) = unpack_sockaddr_in($peeraddr);
        $addr = inet_ntoa($addr);

        while (my $r = $c->get_request) {
            print "Got request from $addr:$port [[\n", $r->as_string, "]]\n" if $g_debug;

            my $response = HTTP::Response->new;

            $response->header("Content-Type" => "application/json; charset=UTF-8");
            $response->content("{}\n");

            $c->send_response($response);

            next if $g_match && $r->uri->path !~ /$g_match/;

            $r->header("x-teeload-client", "$addr:$port") unless $r->header("x-teeload-client");
            $r->header("x-teeload-host", $r->header("Host")) unless $r->header("x-teeload-host");
            if ($r->header("x-teeload-path")) {
                $r->header("x-teeload-path", $r->header("x-teeload-path") . ", $addr:$port");
            } else {
                $r->header("x-teeload-path", "$addr:$port");
            }

            reverse_proxy($r);
        }

        $c->close();
        undef $c;

        print "$addr:$port disconnects.\n" if $g_debug;
    }

    print "Worker $$ exits.\n" if $g_debug;
}

sub reverse_proxy {
    my $request = shift;

    my %logged;

    for my $host_port (@g_backend_servers) {
        my $uri = $request->uri;

        $uri->scheme("http") unless $uri->scheme;
        $uri->host_port($host_port);
        $request->uri($uri);

        unless ($g_keep_host) {
            $request->header("Host", $host_port);
        }

        $logged{$host_port} = 1;

        print "Forward request: [[\n", $request->as_string, "]]\n" if $g_debug && $logged{$host_port};

        my $response = $g_ua->simple_request($request);

        print "Got response: [[\n", $response->as_string, "]]\n" if $g_debug && $logged{$host_port};
    }
}

sub read_servers {
    my @servers = ();

    for my $server (@_) {
        if (-f $server) {
            open my $fh, $server || die "Can't open $server: $!\n";
            while (<$fh>) {
                next if /^\s*#/;
                s/^\s+|\s+$//g;
                push @servers, $_;
            }
            close $fh;
        } else {
            if ($server =~ /:/) {
                push @servers, $server;
            } else {
                push @servers, "$server:80";
            }
        }
    }

    return @servers;
}

