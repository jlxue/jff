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

my $g_sieve_server = 'smtp.corp.example.com';
my $g_sieve_port = 4190;
my $g_listen_port = 41900;
my $g_user = 'dieken';

parse_options();

tcp_server "127.0.0.1", $g_listen_port, \&accept_cb;

enter_loop();

########################################################################
sub parse_options {
    GetOptions(
        "server=s"   => \$g_sieve_server,
        "port=i"     => \$g_sieve_port,
        "listen=i"   => \$g_listen_port,
        "user=s"     => \$g_user,
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
            my ($handle, $fatal, $msg) = @_;

            AE::log error => "[proxy client] got error $msg\n";
            destroy_handles($handle, $sieveclient);
        },

        on_eof      => sub {
            my ($handle) = @_;

            AE::log error => "[proxy client] peer closed\n";
            destroy_handles($handle, $sieveclient);
        },

        on_read     => sub {
            my ($handle) = @_;

            AE::log debug => "[proxy client] $handle->{rbuf}";

            $sieveclient->push_write($handle->{rbuf});
            $handle->{rbuf} = '';
        },
    );

    $sieveclient = AnyEvent::Handle->new(
        connect     => [ $g_sieve_server, $g_sieve_port ],

        on_connect  => sub {
            my ($handle, $host, $port) = @_;
            AE::log info => "Connected to $host:$port.";

            $handle->push_read(line => create_sieve_auth_callback($proxyclient));
        },

        on_connect_error    => sub {
            my ($handle, $msg) = @_;

            AE::log error => "Can't connect to $g_sieve_server:$g_sieve_port: $msg";
            destroy_handles($handle, $proxyclient);
        },

        on_error    => sub {
            my ($handle, $fatal, $msg) = @_;

            AE::log error => "[sieve client] got error $msg\n";
            destroy_handles($handle, $proxyclient);
        },

        on_read     => sub {
            my ($handle) = @_;

            AE::log debug => "[sieve client] $handle->{rbuf}";

            $proxyclient->push_write($handle->{rbuf});
            $handle->{rbuf} = '';
        },
    );
}


sub destroy_handles {
    for (@_) {
        $_->destroy if defined $_;
    }
}


sub create_sieve_auth_callback {
    my ($proxy_client) = @_;
    my $saslclient;

    my $auth_cb;
    $auth_cb = sub {
        my ($handle, $line) = @_;

        AE::log debug => "[sieve client] $line";

        if (! defined($saslclient)) {
            if ($line eq '"STARTTLS"') {
                AE::log debug => "[sieve client] -- discard STARTTLS from sieve server";

            } elsif ($line =~ /^OK\s/) {
                my $sasl = Authen::SASL->new(
                    mechanism   => "GSSAPI",
                    callback    => { authname => $g_user },
                    debug       => 8 | 4 | 1);
                die "SASL object creation failed: $!\n" unless defined $sasl;

                $saslclient = $sasl->client_new("sieve", $g_sieve_server, "noanonymous noplaintext");
                die "SASL client object creation failed: $!\n" unless defined $saslclient;

                #$saslclient->property(realm => "corp.example.com");
                #$saslclient->property(externalssf => 56);

                my $msg = $saslclient->client_start();
                if ($saslclient->code()) {
                    die "SASL error: " . $saslclient->error();
                }

                $msg = 'Authenticate "GSSAPI" "' . encode_base64($msg, "") .  '"';
                AE::log debug => "[sieve client] >> $msg";

                $handle->push_write($msg . CRLF);
            } else {
                $proxy_client->push_write($line . CRLF);
            }
        } else {
            if ($line =~ /^"/) {
                $line =~ s/^"|"$//g;
                my $msg = $saslclient->client_step(decode_base64($line));
                $msg = "" unless defined $msg;

                $msg = '"' . encode_base64($msg, "") . '"';
                AE::log debug => "[sieve client] >> $msg";
                $handle->push_write($msg . CRLF);
            } else {
                $proxy_client->push_write($line . CRLF);

                if ($line !~ /^OK\s/) {
                    destroy_handles($handle, $proxy_client);
                }

                return;
            }
        }

        $handle->push_read(line => $auth_cb);
    };

    return $auth_cb;
}

