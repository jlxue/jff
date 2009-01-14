#!/usr/bin/perl -w
#
#  newsmth-notifier.pl - poll newsmth.net for new messages
#
#  Usage:
#    if run it for the first time, create a file named "board.list"
#    with content like this(no leading '#' characters):
#       Perl
#       LinuxApp
#
#    run the command below to start the poll:
#       perl newsmth-notifier.pl
#
#   Note: this script must be in GBK encoding if you add Chinese
#   characters in it, for example use Chinese characters directly
#   in regular expressions.
#         (这个脚本必须是 GBK 编码)
#
#  ChangeLog:
#       2007-10-03      
#           * can poll newsmth for new messages
#           * release 1.0
#       2007-10-05
#           * sleep more to make newsmth server happier
#           (suggested by EOF@newsmth)
#           * use different terminal encoding according to OS and locale
#           (reported by sspin@newsmth)
#
#       2009-01-14
#           * fix GBK/UTF-8 terminal encoding problem
#
#  Dieken at newsmth.net
#
#  This script is released under GNU GPL v3.
#
#  TODO: put it into system tray
#
use strict;
use warnings;
use Encode;
use File::Slurp;
use LWP::Simple;
use POSIX qw/setsid strftime/;
#use Proc::Daemon;

$| = 1;
my $terminal_encoding;
if ($^O eq "MSWin32" || 
        (exists($ENV{'LC_MESSAGES'}) && $ENV{'LC_MESSAGES'} =~ /\.GB(?:2312|K|18030)/i) ||
        (exists($ENV{'LANG'})        && $ENV{'LANG'}        =~ /\.GB(?:2312|K|18030)/i) ||
        (exists($ENV{'LC_CTYPE'})    && $ENV{'LC_CTYPE'}    =~ /\.GB(?:2312|K|18030)/i)) {
    $terminal_encoding = 'GBK';
} else {
    $terminal_encoding = 'UTF-8';
}

my %lastPostIds;
my $got_sig_hup = 0;

load_board_list();

$SIG{HUP}  = sub { ++$got_sig_hup; };

# daemonize();


$SIG{INT}  = \&save_board_list;
$SIG{KILL} = \&save_board_list;
$SIG{QUIT} = \&save_board_list;

while (1) {
    if ($got_sig_hup > 0) {
        load_board_list();
        --$got_sig_hup;
    }
    print "# checking...........................\n";
    for my $board (sort(keys %lastPostIds)) {
        my $content = get('http://www.newsmth.net/bbsdoc.php?board=' . $board);
        next unless defined $content;

        my @posts = parse_page($content);
        
        for my $post (@posts) {
            if (exists $lastPostIds{$board}) {
                if ($lastPostIds{$board} < $post->[0]) {
                    $lastPostIds{$board} = $post->[0];
                    print $board, ': ', format_post($post), "\n";
                }
            } else {
                $lastPostIds{$board} = $post->[0];
            }
        }

        sleep rand 5;
    }
    sleep 60;

    save_board_list();
}


END {
    save_board_list();
}

############################################################
#
sub parse_page {
    my $content = shift;
    #my ($postId, $topicId, $author, $flags, $time, $title, $bytes, $n);
    my (@a, @result);
    my ($begin, $end) = (0, 0);
    while (($begin = index($content, 'c.o(', $begin)) >= 0) {
        $end = index($content, ");\n", $begin);
        last if $end <= $begin;
        @a = substr($content, $begin + 4, $end - $begin) =~
                /(\d+),(\d+),'(.*)','(.*)',(\d+),'(.*)',(\d+),(\d+)/;
        push @result, [@a];
        $begin = $end;
    }

    return @result;
}


sub format_post {
    my $post = shift;
    my $result = sprintf "[%10s] %6d (%-12s) | %s",
                         strftime("%m-%d %H:%M", localtime($post->[4])), # time
                         $post->[0],    # postId
                         $post->[2],    # author
                         $post->[5];    # title

    if ('GBK' ne $terminal_encoding) {
        Encode::from_to($result, 'GBK', $terminal_encoding);
    }

    return $result;
}


sub daemonize {
    # http://www.newsmth.net/bbscon.php?bid=226&id=15888
    defined(my $pid = fork) or die "Can't fork: $!\n";
    exit if $pid;
    open STDIN, '/dev/null' or die "Can't read /dev/null: $!\n";
    open STDOUT, '>/dev/null' or die "Can't write stdout to /dev/null: $!\n";
    open STDERR, '>/dev/null' or die "Can't write stderr to /dev/null: $!\n";
    chdir '/' or die "Can't chdir to /: $!\n";
    umask 0;
    setsid or die "Can't start a new session: $!\n";
}


sub load_board_list {
    open (my $fh, 'board.list') || die "Can't read board list: $!\n";
    while (<$fh>) {
        chomp;
        next if /^\s*#/;
        s/^\s+//;
        my ($board, $id) = split /\s+/;
        $lastPostIds{$board} = ($id || 0) if defined $board;
    }
    close $fh;
}


sub save_board_list {
    my $sig = shift;
    local $SIG{$sig} = 'IGNORE' if defined $sig;

    my @a;
    push @a, sprintf("%-32s        %s\n", "# board name", "last post id");
    while (my ($board, $id) = each %lastPostIds) {
        push @a, sprintf("%-32s        %d\n", $board, $id);
    }
    write_file('board.list', {binmode => ':utf8', atomic => 1}, \@a);

    exit 0 if defined $sig;
}

