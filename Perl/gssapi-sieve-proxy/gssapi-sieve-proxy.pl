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
use constant FAILED_LOGIN_LIMIT_PER_MIN     => 5;

# Reference: sieve-connect libnet-managesieve-perl libnet-sieve-perl

my $g_conf;
my $g_sieve_server;
my $g_sieve_port;
my $g_listen_port;
my $g_user;
my $g_password;
my $g_debug = 0;
my $g_failed_logins = 0;

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

            AE::log error => "[C] got error $msg\n";
            destroy_handles($handle, $sieveclient);
        },

        on_eof      => sub {
            my ($handle) = @_;

            AE::log info => "[C] peer closed\n";
            destroy_handles($handle, $sieveclient);
        },

        on_read     => sub {
            my ($handle) = @_;

            AE::log debug => "[P<C] $handle->{rbuf}";

            $sieveclient->push_write($handle->{rbuf});
            $handle->{rbuf} = '';
        },
    );

    $sieveclient = AnyEvent::Handle->new(
        connect     => [ $g_sieve_server, $g_sieve_port ],

        on_connect  => sub {
            my ($handle, $host, $port) = @_;
            AE::log info => "Connected to $host:$port.";

            $handle->push_read(line => create_sieve_connect_callback($proxyclient));
        },

        on_connect_error    => sub {
            my ($handle, $msg) = @_;

            AE::log error => "Can't connect to $g_sieve_server:$g_sieve_port: $msg";
            destroy_handles($handle, $proxyclient);
        },

        on_error    => sub {
            my ($handle, $fatal, $msg) = @_;

            AE::log error => "[S] got error $msg\n";
            destroy_handles($handle, $proxyclient);
        },

        on_eof      => sub {
            my ($handle) = @_;

            AE::log info => "[S] peer closed\n";
            destroy_handles($handle, $proxyclient);
        },

        on_read     => sub {
            my ($handle) = @_;

            AE::log debug => "[S>P] $handle->{rbuf}";

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
sub create_sieve_connect_callback {
    my ($proxy_client) = @_;
    my $buffer = "";
    my $starttls = 0;

    my $cb;
    $cb = sub {
        my ($handle, $line) = @_;

        AE::log debug => "[S>P] $line";

        if ($line eq '"STARTTLS"' && ! exists $handle->{tls}) {
            $starttls = 1;

        } elsif ($line =~ /^"SASL"\s/) {
            AE::log debug => "discard SASL from sieve server";

            $buffer .= '"SASL" "PLAIN"' . CRLF;

        } elsif ($line =~ /^OK\s/) {
            AE::log info => "Sieve server ready";

            if ($starttls) {
                $buffer = "";
                $starttls = 0;

                starttls($handle);
            } else {
                AE::log info => "Sieve server ready, begin GSSAPI authentication";

                $buffer .= $line . CRLF;

                my $saslclient = create_sasl_client();
                my $msg = $saslclient->client_start();
                if ($saslclient->code()) {
                    die "SASL error: " . $saslclient->error();
                }

                $msg = 'Authenticate "GSSAPI" "' . encode_base64($msg, "") .  '"';
                AE::log debug => "[S<P] $msg";
                $handle->push_write($msg . CRLF);

                $handle->push_read(line => create_sieve_auth_callback($proxy_client, $buffer, $saslclient));
                return;
            }

        } else {
            $buffer .= $line . CRLF;
        }

        $handle->push_read(line => $cb);
    };

    return $cb;
}


sub create_sieve_auth_callback {
    my ($proxy_client, $buffer, $saslclient) = @_;

    my $cb;
    $cb = sub {
        my ($handle, $line) = @_;

        AE::log debug => "[S>P] $line";

        if ($line =~ /^"/) {
            $line =~ s/^"|"$//g;
            my $msg = $saslclient->client_step(decode_base64($line));
            $msg = "" unless defined $msg;

            $msg = '"' . encode_base64($msg, "") . '"';
            AE::log debug => "[S<P] $msg";
            $handle->push_write($msg . CRLF);
        } else {
            if ($line !~ /^OK\s/) {
                die "Failed to authenticate against sieve server: $line";
            }

            AE::log debug => "[P>C] $buffer";
            $proxy_client->push_write($buffer);

            if ($g_password) {
                authenticate_proxy_client($proxy_client, $handle);
            }

            return;
        }

        $handle->push_read(line => $cb);
    };

    return $cb;
}


sub starttls {
    my ($handle) = @_;

    my $msg = 'STARTTLS';
    AE::log debug => "[S<P] $msg";
    $handle->push_write($msg . CRLF);

    $handle->push_read(line => sub {
            my ($hdl, $line) = @_;

            AE::log debug => "[S>P] $line";

            if ($line !~ /^OK\s/) {
                die "Failed to starttls: $line\n";
            }

            $hdl->{rbuf} = "";
            $hdl->starttls("connect");
        });
}


sub create_sasl_client {
    my $sasl = Authen::SASL->new(
        mechanism   => "GSSAPI",
        callback    => { authname => $g_user },
        debug       => $g_debug ? (8 | 4 | 1) : 0);
    die "SASL object creation failed: $!\n" unless defined $sasl;

    my $saslclient = $sasl->client_new("sieve", $g_sieve_server, "noanonymous noplaintext");
    die "SASL client object creation failed: $!\n" unless defined $saslclient;

    #$saslclient->property(realm => "corp.example.com");
    #$saslclient->property(externalssf => 56);

    return $saslclient;
}


sub authenticate_proxy_client {
    my ($proxy_client, $sieve_client) = @_;

    AE::log info => "begin authenticate proxy client";

    $proxy_client->push_read(line => sub {
            my ($handle, $line) = @_;

            AE::log debug => "[P<C] $line";

            if ($line =~ /^AUTHENTICATE\s+"PLAIN"\s+"([^"]+)"\s*$/) {
                my @a = split /\000/, decode_base64($1);

                if ($a[2] && $g_password eq $a[2]) {
                    my $msg = 'OK "Logged in."';
                    AE::log debug => "[P>C] $msg";
                    $handle->push_write($msg .  CRLF);
                    return;
                } else {
                    my $msg = 'NO "Bad credential."';
                    AE::log debug => "[P>C] $msg";
                    $handle->push_write($msg .  CRLF);
                }
            }

            AE::log warn => "Failed to authenticate proxy client: $line";
            destroy_handles($handle, $sieve_client);
        });
}

