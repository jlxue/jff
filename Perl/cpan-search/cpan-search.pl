#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;
use File::Basename;
use IO::Uncompress::AnyUncompress qw(anyuncompress $AnyUncompressError);
use LWP::UserAgent;

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

my ($meta_files, $author_id_map, $dists) = filter_find_ls($ARGV[0]);
print Dumper($author_id_map);

