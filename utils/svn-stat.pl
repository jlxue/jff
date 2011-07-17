#!/usr/bin/perl

#
# Purpose:
#   count lines produced or touched by specified authors
#
# Usage:
#   svn checkout http://..../xxx
#   svn log -r '{2009-01-01}:HEAD' -v --xml xxx > xxx.log
#   ./svn-stat.pl xxx.log xxx authorA authorB
#
#   Run "./svn-stat.pl" without arguments to see its help.
#
use strict;
use warnings;
use Data::Dumper;
use Getopt::Long;
use XML::Simple;
use constant {
    E_DELETE  => -1,
    E_DIR     => -2,
    E_FAIL    => -3,
};

my $g_opt_skip_merge = 0;
my @g_opt_include_patterns;
my @g_opt_exclude_patterns;
my ($log_file, $wc_dir, @authors);
my ($include_pattern, $exclude_pattern, $author_pattern);
my ($total_lines, $author_paths, $path_lines, $author_lines);


parse_command_line();

$author_paths = read_and_parse_svn_log($log_file, $wc_dir,
        $include_pattern, $exclude_pattern, $author_pattern);
($total_lines, $author_lines, $path_lines) = count_paths_lines($author_paths);

print "Total\t$total_lines\n";
for my $author (sort keys %$author_lines) {
    print "$author\t$author_lines->{$author}\n";
}

########################################################################
sub usage {
    print "$0 [--skip-merge] [--include|-I pattern]... [--exclude|-X pattern]... log-file wc-dir [author...]\n";
    exit 0;
}

sub svn_root_dir {
    my $info = `svn info $wc_dir`;
    my ($url) = $info =~ /URL:\s*(\S+)/;
    my ($root) = $info =~ /Root:\s*(\S+)/;

    die "Can't obtain svn info on $wc_dir\n" unless defined $url && defined $root;
    return substr $url, length($root);
}

sub count_lines {
    my $path = shift;

    return E_DELETE unless -e $path;
    return E_DIR if -d $path;

    open my $fh, $path or warn "Can't read $path: $!\n";;
    return E_FAIL unless defined $path;

    my $n = 0;
    while (<$fh>) {
        ++$n;
    }

    close $fh;
    return $n;
}

sub build_patterns {
    my ($includes, $excludes, $authors) = @_;
    my @patterns;
    my $s;

    if (@$includes > 0) {
        $s = "(?:" . join("|", @$includes) . ")";
        push @patterns, qr/$s/i;
    } else {
        push @patterns, qr/./;
    }

    if (@$excludes > 0) {
        $s = "(?:" . join("|", @$excludes) . ")";
        push @patterns, qr/$s/i;
    } else {
        push @patterns, qr/^$/;
    }

    if (@$authors > 0) {
        $s = "^(?:" . join("|", @$authors) . ")\$";
        push @patterns, qr/$s/i;
    } else {
        push @patterns, qr/./;
    }

    return @patterns;
}

sub read_svn_log {
    my $log_file = shift;
    my $logs = XMLin $log_file;
    die "Can't parse $log_file:$ !\n" unless defined $logs;
    $logs = $logs->{logentry};

    return $logs;
}

sub parse_command_line {
    GetOptions("skip-merge!" => \$g_opt_skip_merge,
               "include|I=s" => \@g_opt_include_patterns,
               "exclude|X=s" => \@g_opt_exclude_patterns);

    usage() if @ARGV < 2;

    ($log_file, $wc_dir, @authors) = @ARGV;
    die "$log_file isn't a file!\n" unless -f $log_file;
    die "$wc_dir isn't a svn working copy!\n"
            unless -d $wc_dir && -d "$wc_dir/.svn";

    ($include_pattern, $exclude_pattern, $author_pattern) =
            build_patterns(\@g_opt_include_patterns, \@g_opt_exclude_patterns, \@authors);
}

sub read_and_parse_svn_log {
    my ($log_file, $wc_dir, $include_pattern, $exclude_pattern, $author_pattern) = @_;

    my $logs = read_svn_log($log_file);
    my $root = svn_root_dir($wc_dir);
    my %author_paths;

    for my $log (@$logs) {
        my $author = $log->{author};

        if ($log->{msg} =~ /\bmerg/i) {
            print STDERR "Found merge: rev $log->{revision} by $author\n";
            next if $g_opt_skip_merge;
        }

        next unless $author =~ $author_pattern;

        my $paths = $log->{paths}{path};
        my $paths2;

        if (exists $author_paths{$author}) {
            $paths2 = $author_paths{$author};
        } else {
            $paths2 = $author_paths{$author} = {};
        }

        if (ref($paths) ne 'ARRAY') {   # only one file is changed in this revision
            my $path = $paths;
            my $content = $path->{content};

            next if $content =~ $exclude_pattern || $content !~ $include_pattern;

            $paths2->{ "$wc_dir/" . substr($content, length($root)+1) } = 1 if length($content)+1 > length($root);
        } else {
            for my $path (@$paths) {
                my $content = $path->{content};

                next if $content =~ $exclude_pattern || $content !~ $include_pattern;

                $paths2->{ "$wc_dir/" . substr($content, length($root)+1) } = 1 if length($content)+1 > length($root);
            }
        }
    }

    return \%author_paths;
}

sub count_paths_lines {
    my ($author_paths) = @_;
    my ($total_lines, %author_lines, %path_lines) = 0;

    for my $author (sort keys %$author_paths) {
        my $paths = $author_paths->{$author};

        for my $path (sort keys %$paths) {
            #print "$author\t$path\n";

            unless (exists $path_lines{$path}) {
                $path_lines{$path} = count_lines($path);
                $total_lines += $path_lines{$path} if $path_lines{$path} > 0;
            }

            $author_lines{$author} += $path_lines{$path} if $path_lines{$path} > 0;
        }
    }

    return ($total_lines, \%author_lines, \%path_lines);
}
