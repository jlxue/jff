#!/usr/bin/perl
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
#       2009-01-14
#           * fix GBK/UTF-8 terminal encoding problem
#           * support system tray icon for Windows, release 2.0
#       2010-02-01
#           * cleanup code, support desktop notification for Linux
#             release 3.0
#       2010-02-02
#           * add some options, release 3.1
#
#  Dieken at newsmth.net
#
#  This script is released under GNU GPL v3.
#
#  TODO: cleanup code, port to Linux
#

#########################################################
## system tray notification

{ # begin package WindowsNotify
package WindowsNotify;

use threads;
use threads::shared;
use Thread::Queue;
use if $^O eq "MSWin32", Win32::GUI => ();
use constant WM_NOTIFYICON => 40000;
use strict;
use warnings;

use Exporter 'import';
our @EXPORT = qw(notify_start notify_alive notify_send notify_stop);
our @EXPORT_OK = qw(notify_start notify_alive notify_send notify_stop);

my ($main, $q, $thread);

sub notify_start {
    $q = new Thread::Queue;
    $thread = threads->create(\&systray_thread);
    $thread->detach();
    $q->dequeue();      # When systray_thread is ready it will give me a message.
}

sub notify_alive {
    $thread->is_running();
}

sub notify_send {
    $q->enqueue($_[0]);
    sendmessage();
}

sub notify_stop {
}

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
        if (length($msg) > 0) {
            $main->NI->Change(-balloon_tip => $msg, -tip => $msg);
            $main->NI->ShowBalloon(0);
            $main->NI->ShowBalloon(1);
        } else {
            $main->NI->Change(-balloon_tip => 'Checking...', -tip => 'Checking...');
            $main->NI->ShowBalloon(0);
        }
    }

    return 1;
}
} # end package WindowsNotify


{ # begin package LinuxNotify
package LinuxNotify;

use if $^O ne 'MSWin32', "Desktop::Notify";        # require to install notification-daemon
use strict;
use warnings;

use Exporter 'import';
our @EXPORT = qw(notify_start notify_alive notify_send notify_stop);
our @EXPORT_OK = qw(notify_start notify_alive notify_send notify_stop);

my ($notify, $notification);

sub notify_start {
    $notify = Desktop::Notify->new();
    $notification = $notify->create(summary => 'NewSMTH Notifier',
                                    body    => "I'm ready, go!",
                                    timeout => 5000);
    $notification->show();
}

sub notify_alive {
    1;
}

sub notify_send {
    $notification->body($_[0]);
    $notification->show();
}

sub notify_stop {
    $notification->close() if defined $notification;
    undef $notification;
}

} # end package LinuxNotify

{ # begin package DummyNotify
package DummyNotify;

use strict;
use warnings;

use Exporter 'import';
our @EXPORT = qw(notify_start notify_alive notify_send notify_stop);
our @EXPORT_OK = qw(notify_start notify_alive notify_send notify_stop);

sub notify_start {
}

sub notify_alive {
    1;
}

sub notify_send {
}

sub notify_stop {
}

} # end package DummyNotify

#########################################################
{   # begin package main
use Encode;
use File::Slurp;
use Getopt::Long;
use LWP::Simple;
use POSIX qw/setsid strftime/;
#use Proc::Daemon;
use strict;
use warnings;

my $opt_use_gui = 1;
my @opt_boards = ('Perl');
my $opt_save = 1;
my $opt_board_list = 'board.list';
my $opt_help = 0;

GetOptions(
    "gui!"      => \$opt_use_gui,
    "board=s"   => \@opt_boards,
    "save!"     => \$opt_save,
    "list=s"    => \$opt_board_list,
    "help"      => \$opt_help,
);
@opt_boards = split(/[;:,\s]/,join(',',@opt_boards));

if (! $opt_use_gui) {
    DummyNotify->import();
} elsif ($^O eq 'MSWin') {
    WindowsNotify->import();
} elsif ($^O eq 'linux') {
    LinuxNotify->import();
} else {
    warn "Use dummy notification implementation!\n";
    DummyNotify->import();
}


$| = 1;
my $terminal_encoding;
if ($^O eq "MSWin32" ||
        (exists($ENV{'LC_MESSAGES'}) && $ENV{'LC_MESSAGES'} =~ /\.GB(?:2312|K|18030)/i) ||
        (exists($ENV{'LANG'})        && $ENV{'LANG'}        =~ /\.GB(?:2312|K|18030)/i) ||
        (exists($ENV{'LC_CTYPE'})    && $ENV{'LC_CTYPE'}    =~ /\.GB(?:2312|K|18030)/i)) {
    $terminal_encoding = 'GBK';
} else {
    $terminal_encoding = 'UTF-8';
    binmode STDOUT, ":utf8";
    binmode STDERR, ":utf8";
}

my %lastPostIds;
my $got_sig_hup = 0;

usage() if $opt_help;

load_board_list();

$SIG{HUP}  = sub { ++$got_sig_hup; };

# daemonize();


$SIG{INT}  = \&save_board_list;
$SIG{KILL} = \&save_board_list;
$SIG{QUIT} = \&save_board_list;

notify_start();

while (notify_alive()) {
    if ($got_sig_hup > 0) {
        load_board_list();
        --$got_sig_hup;
    }

    print "# checking...........................\n";
    my $message = "";

    for my $board (sort(keys %lastPostIds)) {
        my $count = 0;

        # At least in in LWP v5.827  get() returns decoded text, so $context is
        # encoded in Perl's internal encoding(UTF-8).   2010-02-10 lyb
        my $content;
        eval {
            $content = get('http://www.newsmth.net/bbsdoc.php?board=' . $board);
        };
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

    notify_send($message);

    save_board_list();
    sleep 60 if notify_alive();
}


END {
    notify_stop();
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

    if ('GBK' eq $terminal_encoding) {
        eval {
            $result =  Encode::encode("GBK", $result);
        };
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
    if (-r $opt_board_list) {
        open (my $fh, $opt_board_list) || die "Can't read $opt_board_list: $!\n";
        while (<$fh>) {
            chomp;
            next if /^\s*#/;
            s/^\s+//;
            my ($board, $id) = split /\s+/;
            $lastPostIds{$board} = ($id || 0) if defined $board;
        }
        close $fh;
    }

    for my $board (@opt_boards) {
        $lastPostIds{$board} = 0 if !exists $lastPostIds{$board};
    }
}


sub save_board_list {
    my $sig = shift;
    local $SIG{$sig} = 'IGNORE' if defined $sig;

    if (keys %lastPostIds > 0 && $opt_save) {
        my @a;
        push @a, sprintf("%-32s        %s\n", "# board name", "last post id");
        while (my ($board, $id) = each %lastPostIds) {
            push @a, sprintf("%-32s        %d\n", $board, $id);
        }
        write_file($opt_board_list, {binmode => ':utf8', atomic => 1}, \@a);
    }

    if (defined $sig) {
        notify_stop();
        exit 0;
    }
}

sub usage {
    print <<END;
NewSMTH-Notifier v3.1
    --gui, --no-gui     whether to use notification. (default yes)
    --board "XXX,YYY"   specify board list in addition to a list file
    --save, --no-save   whether to save notifier state to board list
                        file (default yes)
    --list FILE         specify a board list file (default board.list)
    --help              show this help and quit
END
    exit 0;
}

} # end package main

