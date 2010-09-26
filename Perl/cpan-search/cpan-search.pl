#!/usr/bin/perl
use strict;
use warnings;
use File::Basename;
use LWP::UserAgent;

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

mirror_files($MIRROR_SITE, @MIRROR_FILES);


sub mirror_files {
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

