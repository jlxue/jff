#!/usr/bin/env perl
use strict;
use warnings;
use AnyEvent;
use AnyEvent::Handle;
use AnyEvent::Log;
use AnyEvent::Socket;
use Authen::SASL;
use Getopt::Long;
use MIME::Base64;
use Socket qw(:DEFAULT :crlf);

# Reference: sieve-connect libnet-managesieve-perl libnet-sieve-perl

my $g_conf;
my $g_sieve_server;
my $g_sieve_port;
my $g_listen_port;
my $g_user;
my $g_password;
my $g_debug = 0;

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
        "conf=s"     => \$g_conf,
        "debug!"     => \$g_debug,
    );

    if (defined $g_conf) {
        my %conf;

        open my $fh, $g_conf or die "Can't open $g_conf: $!\n";
        while (<$fh>) {
            next if /^\s*#/;

            s/^\s+|\s+$//g;
            my @a = split /\W+/, $_, 2;
            $conf{lc($a[0])} = $a[1];
        }
        close $fh;

        $g_sieve_server ||= $conf{server};
        $g_sieve_port   ||= $conf{port};
        $g_listen_port  ||= $conf{listen};
        $g_user         ||= $conf{user};
        $g_password     ||= $conf{password};
    }

    $g_sieve_server ||= "smtp.corp.example.com";
    $g_sieve_port   ||= 4190;
    $g_listen_port  ||= 41900;
    $g_user         ||= $ENV{USER};

    if ($g_debug) {
        $AnyEvent::Log::FILTER->level("trace");
    } else {
        $AnyEvent::Log::FILTER->level("info");
    }

    die "ERROR: sieve server domain name is empty!\n" unless $g_sieve_server;
    die "ERROR: sieve server port must be greater than 0 and not greater than 65535!\n"
        unless $g_sieve_port > 0 && $g_sieve_port <= 65535;
    die "ERROR: proxy listening port must be greater than 0 and not greater than 65535!\n"
        unless $g_listen_port > 0 && $g_listen_port <= 65535;
    die "ERROR: user name for sieve server isn't specified!\n" unless $g_user;

    AE::log info => "Listen on $g_listen_port and will connect to $g_user\@$g_sieve_server:$g_sieve_port...";

    if (! $g_password) {
        AE::log warn => "Password isn't specified, this proxy isn't protected!";
    }
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

        on_eof      => sub {
            my ($handle) = @_;

            AE::log error => "[sieve client] peer closed\n";
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


# Logic:
#   * buffer messages from Sieve server, until got "OK ...", this means
#     Sieve server is ready now.
#   * if STARTTLS is found, send "STARTTLS" to Sieve server, expect
#     it returns "OK ...".
#      * Upgrade socket to SSL enabled socket, goto step 1;
#   * Do GSSAPI authentication against Sieve server
#   * Output buffered messages to proxy client
#   * Do Plain authentication against proxy client
#
sub create_sieve_auth_callback {
    my ($proxy_client) = @_;
    my $buffer = "";
    my $saslclient;

    my $auth_cb;
    $auth_cb = sub {
        my ($handle, $line) = @_;

        AE::log debug => "[sieve client] $line";

        if (! defined($saslclient)) {
            if ($line eq '"STARTTLS"') {
                AE::log debug => "[sieve client] -- discard STARTTLS from sieve server";

                $buffer = "";

                my $msg = 'STARTTLS';
                AE::log debug => "[sieve client] >> $msg";
                $handle->push_write($msg . CRLF);

                $handle->push_read(line => sub {
                        my ($hdl, $line) = @_;

                        AE::log debug => "[sieve client] $line";

                        if ($line !~ /^OK\s/) {
                            die "Failed to starttls: $line\n";
                        }

                        $hdl->{rbuf} = "";
                        $hdl->starttls("connect");
                    });

            } elsif ($line =~ /^"SASL"\s/) {
                AE::log debug => "[sieve client] -- discard SASL from sieve server";

                $buffer .= '"SASL" "PLAIN"' . CRLF;

            } elsif ($line =~ /^OK\s/) {
                AE::log info => "[sieve client] -- Sieve server ready, begin GSSAPI authentication";

                $buffer .= $line . CRLF;

                my $sasl = Authen::SASL->new(
                    mechanism   => "GSSAPI",
                    callback    => { authname => $g_user },
                    debug       => $g_debug ? (8 | 4 | 1) : 0);
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
                $buffer .= $line . CRLF;
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
                if ($line !~ /^OK\s/) {
                    die "Failed to authenticate against sieve server: $line";
                }

                $proxy_client->push_write($buffer);
                return;
            }
        }

        $handle->push_read(line => $auth_cb);
    };

    return $auth_cb;
}

