#!/usr/bin/perl -w
# grab_web.pl - extract hyper links from a url
#
use strict;
use warnings;
use File::Slurp;
use HTML::TreeBuilder;
use LWP::Simple;

die "Usage: perl a.pl url\n" if @ARGV == 0;

my $content = get($ARGV[0]);
die "Couldn’t get it: $ARGV[0]!\n" unless defined $content;

my $root = HTML::TreeBuilder->new_from_content($content);
my $links = $root->extract_links('a');
my ($link, $element, $attr, $tag);
my $filename = 1;

for (@{$links}) {
    ($link, $element, $attr, $tag) = @$_;
    
    $link = $ARGV[0] . $link if (index($link, "http") < 0);
    print "dump $link to [$filename]...\n";
    write_file($filename++, {binmode => ':raw'}, `w3m -dump $link`);
}

