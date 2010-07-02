package MarkIt::Base;

use warnings;
use strict;
use base 'Titanium';
use Carp qw( croak );
use Log::Dispatch::File;

our $VERSION = '0.01';

sub cgiapp_init {
    my ($c) = @_;

    my $data_source = "dbi:SQLite:dbname=./markit.db";
    my $username = "";
    my $password = "";
    my %attr = ();
    $c->dbh_config($data_source, $username, $password, \%attr);

    $c->run_modes([qw/preflight/]);
}


sub cgiapp_prerun {
    my ($c) = @_;

    $c->header_add(-charset => "UTF-8");

    my $q = $c->query;
    my $hasAccessControl = 0;

    $c->log->info("cgiapp_prerun: request_method=" . $q->request_method . "\n");

    # we'd better use $q->http("Access-Control-Request-Method").
    if (exists $ENV{"HTTP_ACCESS_CONTROL_REQUEST_METHOD"}) {
        $c->header_add(-Access_Control_Request_Methods =>
                "POST, GET, PUT, HEAD, DELETE, OPTIONS");
        $hasAccessControl = 1;
    }

    if (exists $ENV{"HTTP_ACCESS_CONTROL_REQUEST_HEADERS"}) {
        $c->header_add(-Access_Control_Request_Headers =>
                $ENV{"HTTP_ACCESS_CONTROL_REQUEST_HEADERS"});
        $hasAccessControl = 1;
    }

    if (exists $ENV{"HTTP_ORIGIN"}) {
        $c->header_add('-Access_Control_Allow_Origin' => '*');
        $hasAccessControl = 1;
    }

    if ($hasAccessControl) {
        $c->header_add(-Access_Control_Max_Age => 172800);
    }

    if ($q->request_method eq 'OPTIONS') {
        $c->prerun_mode("preflight");
    }
}

#sub cgiapp_postrun {
#    my ($c, $output_ref) = @_;
#}

sub preflight {
    my ($c) = @_;

    $c->log->info("preflighted!\n");

    return "";
}

1;

