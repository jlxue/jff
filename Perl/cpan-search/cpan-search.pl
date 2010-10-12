#!/usr/bin/perl
# Purpose:
#   A more powerful cpan searcher than search.cpan.org
#
# Usage:
#   Run `perl cpan-search.pl' for details.
#
# License:
#   GPL v3
#
# Author:
#   Yubao Liu <yubao.liu@gmail.com>
#
# ChangeLog
#   2010-09-26  Yubao Liu
#       * initial skeleton, implemented mirror_files()
#
#   2010-10-08  Yubao Liu
#       * refactored with App::Cmd
use strict;
use warnings;

########################################################################
{
package CPANSearchApp;
use strict;
use warnings;
use AppConfig;
use App::Cmd::Setup -app;


sub config {
    my $app = shift;

    if (! exists $app->{config}) {
        my $config = AppConfig->new();
        $app->{config} = $config;
    }

    return $app->{config};
}

1;
}

########################################################################
{
package CPANSearchApp::Command;
use strict;
use warnings;
use App::Cmd::Setup -command;

sub opt_spec {
    my ( $class, $app ) = @_;
    return (
        [ 'help' => "This usage screen" ],
        $class->options($app),
    )
}

sub validate_args {
    my ( $self, $opt, $args ) = @_;
    die $self->_usage_text if $opt->{help};
    $self->validate( $opt, $args );
}

1;
}

########################################################################
{
package CPANSearchApp::Command::update;
use strict;
use warnings;
use File::Basename;
use LWP::UserAgent;
#use CPANSearchApp -command;
CPANSearchApp->import(qw(-command));


sub abstract {
    "update CPAN related data files"
}

sub execute {
    my ($self, $opt, $args) = @_;

    print "Updating...\n";
}

sub mirror_files($@) {
    my ($site, @files) = @_;
    my $i = 1;
    my $count = @files;
    my $ua = LWP::UserAgent->new;
    $ua->timeout(10);
    $ua->env_proxy();
    $ua->show_progress(1);

    $site .= '/' if $site !~ m|/$|;

    for my $file (@files) {
        my ($dir) = $file =~ m|([^/]+)/|;
        mkdir $dir if defined($dir) && ! -e $dir;

        my $remote_file = $site . $file;

        print "[$i/$count] Mirroring $remote_file to $file...\n";
        ++$i;

        my $response = $ua->mirror($remote_file, $file);
        die "Failed to mirror $remote_file: " . $response->status_line if $response->is_error;
    }
}

######## MAIN ENTRY ##################################
my @MIRROR_FILES = qw(
    authors/00whois.html
    authors/00whois.xml
    authors/01mailrc.txt.gz
    indices/du-k.gz
    indices/find-ls.gz
    indices/ls-lR.gz
    modules/00modlist.long.html
    modules/01modules.index.html
    modules/01modules.mtime.html
    modules/01modules.mtime.rss
    modules/02packages.details.txt.gz
    modules/03modlist.data.gz
);

my $MIRROR_SITE = 'http://cpan.wenzk.com/';

#mirror_files($MIRROR_SITE, @MIRROR_FILES);

#my ($meta_files, $author_id_map, $dists) = filter_find_ls($ARGV[0]);
#print Dumper($author_id_map);
1;
}

########################################################################
{
package CPANSearchApp::Command::build;
use strict;
use warnings;
use IO::Uncompress::AnyUncompress qw(anyuncompress $AnyUncompressError);
#use CPANSearchApp -command;
CPANSearchApp->import(qw(-command));


sub abstract {
    "some abstract"
}

sub execute {
    my ($self, $opt, $args) = @_;
}

sub filter_find_ls($) {
    my $z = IO::Uncompress::AnyUncompress->new($_[0]) or die "anyuncompress failed: $AnyUncompressError\n";
    my (@meta_files, %author_id_map, @dists);

    while (<$z>) {
        next if ! /\d authors\//;

        my @a = split;
        if ($a[2] eq "l") {
            if ($a[8] !~ m|^authors/id/|) {
                $author_id_map{ substr($a[8], 8) } = substr($a[9], 3);
            }
        } elsif ($a[2] =~ /^-/) {
            if ($a[8] =~ /\.meta$/) {
                push @meta_files, [ @a[6..8] ];
            } elsif ($a[8] =~ /[\.\-_](?:gz|bz2|zip|tgz|tbz2|tar|Z|rar)$/i) {
                push @dists, [ @a[6..8] ];
            }
        }
    }

    $z->close;

    return (\@meta_files, \%author_id_map, \@dists);
}

1;
}

########################################################################
{
package CPANSearchApp::Command::search;
use strict;
use warnings;
#use CPANSearchApp -command;
CPANSearchApp->import(qw(-command));


sub abstract {
    "some abstract"
}

sub execute {
    my ($self, $opt, $args) = @_;
}

1;
}

########################################################################
#   {
#   package CPANSearchApp::Command::foo;
#   use strict;
#   use warnings;
#   #use CPANSearchApp -command;
#   CPANSearchApp->import(qw(-command));
#
#   
#   sub abstract {
#       "some abstract"
#   }
#   
#   sub execute {
#       my ($self, $opt, $args) = @_;
#   }
#   
#   1;
#   }

########################################################################
{
package main;
use strict;
use warnings;
#use CPANSearchApp;
CPANSearchApp->import;


CPANSearchApp->run;
}

