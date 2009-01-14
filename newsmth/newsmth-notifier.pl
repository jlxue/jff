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
#           * support system tray icon for Windows
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
use threads;
use threads::shared;
use Thread::Queue;
use Win32::GUI ();
use constant WM_NOTIFYICON => 40000;

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

my $q = new Thread::Queue;
my $thread = threads->create(\&systray_thread);
$thread->detach();
$q->dequeue();      # When systray_thread is ready it will give me a message.

while (1) {
    if ($got_sig_hup > 0) {
        load_board_list();
        --$got_sig_hup;
    }

    print "# checking...........................\n";
    my $message = "";

    for my $board (sort(keys %lastPostIds)) {
        my $count = 0;

        my $content = get('http://www.newsmth.net/bbsdoc.php?board=' . $board);
        next unless defined $content;

        my @posts = parse_page($content);
        
        for my $post (@posts) {
            if (exists $lastPostIds{$board}) {
                if ($lastPostIds{$board} < $post->[0]) {
                    $lastPostIds{$board} = $post->[0];
                    print $board, ': ', format_post($post), "\n";
                    ++$count;
                }
            } else {
                $lastPostIds{$board} = $post->[0];
            }
        }

        $message .= sprintf("%16s %d\n", $board, $count) if $count > 0;

        sleep rand 5;
    }

    if (length($message) > 0) {
        $q->enqueue($message);
        sendmessage();
    }

    save_board_list();
    sleep 60;
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


#########################################################
## system tray notification
#
my $main;

sub systray_thread {

    $main = Win32::GUI::Window->new(
        -name => 'NewSMTH_Notifier',
        -text => 'NewSMTH Notifier',
        -width => 200,
        -height => 200
    );
    $main->AddLabel(-text => "I'm NewSMTH Notifier");
    $main->Hook(WM_NOTIFYICON, \&notify_new_articles);

    my $icon = new Win32::GUI::Icon('GUIPERL.ICO');
    my $ni = $main->AddNotifyIcon(
        -name => "NI",
        -icon => $icon,
        -tip => "Checking...",
        -balloon => 1,
        -balloon_tip => "Checking...",
        -balloon_title => "NewSMTH Notifier",
        -balloon_icon => "info",
        -balloon_timeout => 10000   # 10 s
    );
    $ni->SetBehaviour(1);

    $q->enqueue("ready");

    Win32::GUI::Dialog();
}

sub NewSMTH_Notifier_Terminate {
    $main->NI->Remove();
    return -1;
}

sub NewSMTH_Notifier_Minimize {
    $main->Disable();
    $main->Hide();
    return 1;
}

sub NI_Click {
    $main->Enable();
    $main->Show();
    return 1;
}

sub sendmessage {
    #my $win = Win32::GUI::FindWindow('PerlWin32GUI_STD', 'NewSMTH Notifier');
    my $win = Win32::GUI::FindWindow('', 'NewSMTH Notifier');
    Win32::GUI::SendMessage($win, WM_NOTIFYICON, 0, 0);
}

sub notify_new_articles {
    my $msg = $q->dequeue_nb();

    if (defined $msg) {
        $main->NI->Change(-balloon_tip => $msg, -tip => $msg);
        $main->NI->ShowBalloon(0);
        $main->NI->ShowBalloon(1);
    }

    return 1;
}

