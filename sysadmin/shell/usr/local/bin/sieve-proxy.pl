#!/usr/bin/env perl
use strict;
use warnings;
use AnyEvent;
use AnyEvent::Handle;
use AnyEvent::Socket;
use Authen::SASL;
use Getopt::Long;
use MIME::Base64;
use Socket qw(:DEFAULT :crlf);

# Reference: sieve-connect libnet-managesieve-perl libnet-sieve-perl

my $g_sieve_server = 'localhost';
my $g_sieve_port = 4190;
my $g_listen_port = 44190;

parse_options();

tcp_server "127.0.0.1", $g_listen_port, \&accept_cb;

enter_loop();

########################################################################
sub parse_options {
    GetOptions(
        "server=s"   => \$g_sieve_server,
        "port=i"     => \$g_sieve_port,
        "listen=i"   => \$g_listen_port,
    );

    die "ERROR: sieve server name is empty!\n" unless $g_sieve_server;
    die "ERROR: sieve server port must be greater than 0 and not greater than 65535!\n"
        unless $g_sieve_port > 0 && $g_sieve_port <= 65535;
    die "ERROR: proxy listening port must be greater than 0 and not greater than 65535!\n"
        unless $g_listen_port > 0 && $g_listen_port <= 65535;

    AE::log info => "Listen on $g_listen_port and connect to $g_sieve_server:$g_sieve_port...";
}


sub enter_loop {
    my $w = AnyEvent->condvar;
    $w->recv;
}


sub accept_cb {
    my ($sock, $host, $port) = @_;

    AE::log info => "Got connection from $host:$port";

    my ($proxyclient, $sieveclient);

    $proxyclient = AnyEvent::Handle->new(
        fh          => $sock,

        on_error    => sub {
            my ($hdl, $fatal, $msg) = @_;

            AE::log error => "[proxy client] got error $msg\n";
            destroy_handles($hdl, $sieveclient);
        },

        on_eof      => sub {
            my ($hdl) = @_;

            AE::log error => "[proxy client] peer closed\n";
            destroy_handles($hdl, $sieveclient);
        },
    );

    $sieveclient = AnyEvent::Handle->new(
        connect     => [ $g_sieve_server, $g_sieve_port ],

        on_connect  => sub {
            my ($handle, $host, $port) = @_;
            AE::log info => "Connected to $host:$port.";

            $proxyclient->on_read(sub {
                    my ($hdl) = @_;

                    AE::log debug => "[proxy client] $hdl->{rbuf}";

                    $sieveclient->push_write($hdl->{rbuf});
                    $hdl->{rbuf} = '';
                });

            my $expect_end_of_server_announce;
            $expect_end_of_server_announce = sub {
                my ($handle, $line) = @_;

                AE::log debug => "[sieve client] $line";

                if ($line =~ /^OK\s/) {
                    $handle->stop_read();

                    my $sasl = Authen::SASL->new(
                        mechansim   => "GSSAPI",
                        debug       => 15,
                    );
                    die "SASL object creation failed: $!\n" unless defined $sasl;

                    my $saslclient = $sasl->client_new("sieve", $g_sieve_server, "noanonymous noplaintext");
                    die "SASL client object creation failed: $!\n" unless defined $saslclient;

                    #$saslclient->property(realm => "corp.example.com");
                    #$saslclient->property(externalssf => 56);

                    my $sasl_tosend = $saslclient->client_start();
                    if ($saslclient->code()) {
                        die "SASL error: " . $saslclient->error();
                    }
                } else {
                    if ($line eq '"STARTTLS"') {
                        AE::log info => "[sieve client] discard STARTTLS from sieve server";
                    } else {
                        $proxyclient->push_write($line . CRLF);
                    }

                    $handle->push_read(line => $expect_end_of_server_announce);
                }
            };

            $handle->push_read(line => $expect_end_of_server_announce);
        },

        on_connect_error    => sub {
            my ($hdl, $msg) = @_;

            AE::log error => "Can't connect to $g_sieve_server:$g_sieve_port: $msg";
            destroy_handles($hdl, $proxyclient);
        },

        on_error    => sub {
            my ($hdl, $fatal, $msg) = @_;

            AE::log error => "[sieve client] got error $msg\n";
            destroy_handles($hdl, $proxyclient);
        },

        on_read     => sub {
            my ($hdl) = @_;

            AE::log debug => "[sieve client] $hdl->{rbuf}";

            $proxyclient->push_write($hdl->{rbuf});
            $hdl->{rbuf} = '';
        },
    );
}

sub destroy_handles {
    for (@_) {
        $_->destroy if defined $_;
    }
}

