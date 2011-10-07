#!/usr/bin/perl
use strict;
use warnings;
use AnyEvent::Loop;     # Must before "use AnyEvent;"
use AnyEvent;
use AnyEvent::Handle;
use AnyEvent::Log;
use AnyEvent::Socket;
use Getopt::Long;
use Socket;
use Try::Tiny;


my $g_port = 9999;

my %g_action_table = (
    run     => \&action_run,
    report  => \&action_report,
    stop    => \&action_stop,
);

my %g_contexts;


GetOptions('port=i'     => \$g_port);
die "Bad port $g_port!" if $g_port < 0 || $g_port >= 65535;


my $guard = tcp_server undef, $g_port, \&on_accept, \&on_prepare;

AnyEvent::Loop::run();


######################################################################
sub on_accept {
    my ($fh, $host, $port) = @_;

    my $handle = AnyEvent::Handle->new(
        fh          => $fh,
        linger      => 10,              # 10 seconds
        rbuf_max    => 2 * 1024 * 1024, # 2 MB
        timeout     => 10,              # 10 seconds
        on_eof      => sub {
            my ($hd) = @_;

            AE::log info => "got eof\n";
            close_connection($hd);
        },
        on_error    => sub {
            my ($hd, $fatal, $msg) = @_;

            AE::log warn => "got error $msg\n";
            close_connection($hd);
        },
        on_timeout  => sub {
            my ($hd) = @_;

            AE::log warn => "got timeout\n";
            close_connection($hd);
        }
    );

    $handle->push_read(json => \&dispatch);

    $g_contexts{$handle} = { handle => $handle };
}


sub on_prepare {
    my ($fh, $host, $port) = @_;

    AE::log info => "Plumber bound to $host, port $port";

    $ENV{PLUMBER_PORT} = $port;
}


sub close_connection {
    my ($hd) = @_;

    delete $g_contexts{$hd};
    $hd->destroy;
}


sub dispatch {
    my ($hd, $ref) = @_;

    if (! $ref || ref($ref) ne 'ARRAY' || @$ref == 0) {
        AE::log warn => "bad request";
        close_connection($hd);
        return;
    }

    my $action_name = shift @$ref;

    try {
        if (! exists $g_action_table{$action_name}) {
            AE::log warn => "Unknown action: $action_name";
        } else {
            AE::log info => "Got action: $action_name";
            $g_action_table{$action_name}($hd, @$ref);
        }
    } catch {
        AE::log warn => "Caught exception when run action \"$action_name\": $_";
    };

    close_connection($hd);
}


sub action_run {
    my ($hd, @args) = @_;

    $hd->push_write(json => \@args);
}


sub action_report {
    my ($hd, @args) = @_;

    $hd->push_write(json => \@args);
}

sub action_stop {
    undef $guard;
}

