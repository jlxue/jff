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
    $c->dbh_config($data_source, $username, $passwd, \%attr);
}

sub cgiapp_prerun {
    my ($c) = @_;

    $c->header_add(-charset => "UTF-8");
}

sub cgiapp_postrun {
    my ($c, $output_ref) = @_;

    $c->header_add('-Access_Control_Allow_Origin' => '*');
}

1;

