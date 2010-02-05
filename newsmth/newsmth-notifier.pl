#!/usr/bin/perl
#
#  newsmth-notifier.pl - poll newsmth.net for new messages
#
#  Usage:
#    See `perl newsmth-notifier.pl --help`.
#
#    if run it for the first time, create a file named "board.list"
#    with content like this(no leading '#' characters):
#       Perl
#       LinuxApp
#
#    run the command below to start the poll:
#       perl newsmth-notifier.pl
#
# Requirement:
#   Windows - install ActiveState Perl (tested with 5.10.0 and 5.10.1)
#             run `ppm` in cmd, select View -> All packages,
#             install Tk, Win32-GUI, File-Slurp
#
#   Debian/Ubuntu - aptitude install perl-tk libdesktop-notify-perl \
#                       notification-daemon libfile-slurp-perl
#
# Author:
#   Dieken at newsmth.net (Liu Yubao <yubao.liu@gmail.com>)
#
# License:
#   GPL v3
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
#       2010-02-01
#           * add some options, release 3.1
#       2010-02-02
#           * support Tk preview window, release 4.0
#       2010-02-02
#           * avoid deleting unread posts
#           * delete old posts automatically to spare memory
#           * release 4.1
#       2010-02-02
#           * delete more posts when there are too many posts in GUI window
#           * adjust GUI_MAX_POSTS to 1000
#       2010-02-03
#           * add --filter-console and --title-filter options
#       2010-02-04
#           * inline missed GUIPerl.ICO
#           * fix crash problem with fork + exec on ActiveState Perl
#           * use correct notification implementation on Windows
#           * fix long waiting in sleep(60) even interrupted on Windows
#           * use sensible font for post list in GUI window
#           * destroy tray icon on exit (XXX: not stable)
#           * release 4.2
#       2010-02-05
#           * fix garbled output with ActivePerl-5.10.0.1005-MSWin32-x86-290470.msi
#           * release 4.3
#
# TODO:
#   * how to quit WindowsNotify's UI thread reliably and safely?
#   * how to send message from main thread to Tk thread without polling
#     with repeat()? Tk::send? Tk::event?

#########################################################
## system tray notification

{ # begin package WindowsNotify ------------------------
package WindowsNotify;

use File::Spec;
use threads;
use threads::shared;
use Thread::Queue;
use if $^O eq "MSWin32", Win32::GUI => qw(WM_CLOSE WM_DESTROY WM_QUIT);
use if $^O eq "MSWin32", "Win32::GUI::BitmapInline";
use constant WM_NOTIFYICON => 40000;
use constant WM_QUITSELF   => 40001;
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
    my $win = Win32::GUI::FindWindow('', 'NewSMTH Notifier');
    Win32::GUI::SendMessage($win, WM_QUITSELF, 0, 0);
    # All don't work....
    #Win32::GUI::SendMessage($win, WM_CLOSE, 0, 0);
    #Win32::GUI::PostQuitMessage($win);
    #Win32::GUI::CloseWindow($win);
}

sub systray_thread {

    $main = Win32::GUI::Window->new(
        -name => 'Main',
        -text => 'NewSMTH Notifier',
        -width => 200,
        -height => 200
    );
    $main->AddLabel(-text => "I'm NewSMTH Notifier");
    $main->Hook(WM_NOTIFYICON, \&notify_new_articles);
    $main->Hook(WM_QUITSELF, \&quit_self);

    my $icon = new Win32::GUI::Icon('GUIPERL.ICO');
    $icon = get_defaulticon() if ! defined $icon;
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

    $main->Hide();
    undef $main;
    exit 0;
}

sub quit_self {
    Win32::GUI::CloseWindow($main);
    Win32::GUI::DestroyWindow($main);
    Win32::GUI::PostQuitMessage($main);
}

sub Main_Close {
    print "Main_Close\n";
    #Win32::GUI::DestroyWindow($main);
    return 1;
}

sub Main_Destroy {
    print "Main_Destroy\n";
    #Win32::GUI::PostQuitMessage($main);
    return 1;
}

sub Main_Quit {
    print "Main_Quit\n";
    return 1;
}

sub Main_Terminate {
    print "Main_Terminate\n";
    $main->NI->Remove();
    return -1;
}

sub Main_Minimize {
    #$main->Disable();
    #$main->Hide();
    return 1;
}

sub NI_Click {
    #$main->Enable();
    #$main->Show();
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

# stolen from Win32::GUI demos NotifyIcon.pl
sub get_defaulticon
{
    return newIcon Win32::GUI::BitmapInline( q(
AAABAAIAICAQAAAAAADoAgAAJgAAACAgAAAAAAAAqAgAAA4DAAAoAAAAIAAAAEAAAAABAAQAAAAA
AIACAAAAAAAAAAAAABAAAAAAAAAAAAAAAAAAgAAAgAAAAICAAIAAAACAAIAAgIAAAMDAwACAgIAA
AAD/AAD/AAAA//8A/wAAAP8A/wD//wAA////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAIgAd3AAiIAAAAAAAAAAAAgHgIcACAiAAAAAAAAAAAAI
CIiAAAAIAAAAAAAAAAAAAAiIAAAAAAAAAAAAAAAAAIgHiAAAAAAAAAAAAAAAAACIB3cACAAAAAAA
AAAAAAAACIB3gAAAAAgAAAAAAAAAAAAIgIiAAAAAAAAAAAAAAAAAAAAAiAiAgAAAAAAAAAAAAAAA
AAeIiAAAAAAAAAAAAAAIgIAAiAiAAAAAAAAAAAAAAIdwAACAAAAAAAAAAAAAgAAABwAAAAAAAAAA
AAAAAAAICHcAAAAAAAAAAAAAAAAACAB3cHAACIgAAAAAAAAIAAiId3gIAAgHAAAAAAAAAAiAB3d4
AIAABwAAAAAAAId3d3eIiAAAAAgAAAAAh3eHd3dwAAiAAACAAAAACAh3d3d3AAAHeAAAAAAAAAAA
iAh3dwCIAAgIeAAAAAAACIiHAHAAh3gAgHAAAAAAAAgACIAACAeIgICAAAAAAAAAAAAAAACIcAiA
AAAAAAAAAAAAAAAAAAAIiAAAAAAAAAAAAAAAAIAACIAAAAAAAAAAAAAAAAAIgIAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA/////////////////wAAf/8AAH//AAB//wAAf/8A
AH//AAB//wAAP/8AAD//AAA//4AAH/+AAB//wAAf/8AAH//gAB//4AAP/4AAD/gAAA/4AAAP8AAA
H+AAAD/wAAA/+AAAP/iAAH//+AD///4A////Af///4f///////////8oAAAAIAAAAEAAAAABAAgA
AAAAAIAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAgAAAAICAAIAAAACAAIAAgIAAAMDAwADA
3MAA8MqmANTw/wCx4v8AjtT/AGvG/wBIuP8AJar/AACq/wAAktwAAHq5AABilgAASnMAADJQANTj
/wCxx/8Ajqv/AGuP/wBIc/8AJVf/AABV/wAASdwAAD25AAAxlgAAJXMAABlQANTU/wCxsf8Ajo7/
AGtr/wBISP8AJSX/AAAA/gAAANwAAAC5AAAAlgAAAHMAAABQAOPU/wDHsf8Aq47/AI9r/wBzSP8A
VyX/AFUA/wBJANwAPQC5ADEAlgAlAHMAGQBQAPDU/wDisf8A1I7/AMZr/wC4SP8AqiX/AKoA/wCS
ANwAegC5AGIAlgBKAHMAMgBQAP/U/wD/sf8A/47/AP9r/wD/SP8A/yX/AP4A/gDcANwAuQC5AJYA
lgBzAHMAUABQAP/U8AD/seIA/47UAP9rxgD/SLgA/yWqAP8AqgDcAJIAuQB6AJYAYgBzAEoAUAAy
AP/U4wD/sccA/46rAP9rjwD/SHMA/yVXAP8AVQDcAEkAuQA9AJYAMQBzACUAUAAZAP/U1AD/sbEA
/46OAP9rawD/SEgA/yUlAP4AAADcAAAAuQAAAJYAAABzAAAAUAAAAP/j1AD/x7EA/6uOAP+PawD/
c0gA/1clAP9VAADcSQAAuT0AAJYxAABzJQAAUBkAAP/w1AD/4rEA/9SOAP/GawD/uEgA/6olAP+q
AADckgAAuXoAAJZiAABzSgAAUDIAAP//1AD//7EA//+OAP//awD//0gA//8lAP7+AADc3AAAubkA
AJaWAABzcwAAUFAAAPD/1ADi/7EA1P+OAMb/awC4/0gAqv8lAKr/AACS3AAAerkAAGKWAABKcwAA
MlAAAOP/1ADH/7EAq/+OAI//awBz/0gAV/8lAFX/AABJ3AAAPbkAADGWAAAlcwAAGVAAANT/1ACx
/7EAjv+OAGv/awBI/0gAJf8lAAD+AAAA3AAAALkAAACWAAAAcwAAAFAAANT/4wCx/8cAjv+rAGv/
jwBI/3MAJf9XAAD/VQAA3EkAALk9AACWMQAAcyUAAFAZANT/8ACx/+IAjv/UAGv/xgBI/7gAJf+q
AAD/qgAA3JIAALl6AACWYgAAc0oAAFAyANT//wCx//8Ajv//AGv//wBI//8AJf//AAD+/gAA3NwA
ALm5AACWlgAAc3MAAFBQAPLy8gDm5uYA2traAM7OzgDCwsIAtra2AKqqqgCenp4AkpKSAIaGhgB6
enoAbm5uAGJiYgBWVlYASkpKAD4+PgAyMjIAJiYmABoaGgAODg4A8Pv/AKSgoACAgIAAAAD/AAD/
AAAA//8A/wAAAP8A/wD//wAA////AOnp6enp6enp6enp6enp6enp6enp6enp6enp6enp6enr5+T/
//////8AAAAA6+sAAAcHBwAAAOvr6///////5Ovn5P///////wAAAOsAB+sA6wcAAADrAOvr////
///k6+fk////////AAAA6wDr6+vrAAAAAAAA6wD//////+Tr5+T///////8AAAAAAOvr6wAAAAAA
AAAAAP//////5Ovn5P///////wAA6+sAB+vrAAAAAAAAAAAA///////k6+fk////////AADr6wAH
BwcAAADrAAAAAAD//////+Tr5+T///////8AAADr6wAHB+sAAAAAAAAAAOv/////5Ovn5P//////
/wAAAAAA6+sA6+vrAAAAAAAAAP/////k6+fk////////AAAAAAAAAAAAAOvrAOvrAOsA/////+Tr
5+T/////////AAAAAAAAAAAAAAfr6+vrAAAA////5Ovn5P////////8AAAAA6+sA6wAAAOvrAOvr
AAD////k6+fk//////////8AAAAA6wcHAAAAAADrAAAAAP///+Tr5+T//////////+sAAAAAAAAH
AAAAAAAAAAAA////5Ovn5P///////////wAA6wDrBwcAAAAAAAAAAAD////k6+fk////////////
AADrAAAHBwcABwAAAADr6+v//+Tr5+T/////////6wAAAOvr6wcHB+sA6wAAAOsAB///5Ovn5P//
/wAAAAAAAOvrAAAHBwcH6wAA6wAAAAAH///k6+fk////AAAA6wcHBwcHBwfr6+vrAAAAAAAAAOv/
/+Tr5+T//+sHBwfrBwcHBwcHAAAAAOvrAAAAAADr////5Ovn5P/rAOsHBwcHBwcHBwAAAAAABwfr
AAAAAP/////k6+fk//8AAOvrAOsHBwcHAADr6wAAAOsA6wfr/////+Tr5+T////r6+vrBwAABwAA
AOsHB+sAAOsABwD/////5Ovn5P///+sAAP/r6wAAAADrAAfr6+sA6wDr///////k6+fk////////
//////8AAADr6wcAAOvrAP///////+Tr5+T/////////////////AAAAAAAA6+vr////////5Ovn
5P//////////////////6wAAAADr6//////////k6+fn5+fn5+fn5+fn5+fn5+fn6+sA6+fn5+fn
5+fn5wfr5wcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHB+vnZxFnZ2dnZ2dnZ2dnZ2dnZ2dn
Z2dnZ2dn6+vr6+tn6+dnDmdnZ2dnZ2dnZ2dnZ2dnZ2dnZ2dnZ2cH6gfqB2fr5+vr6+vr6+vr6+vr
6+vr6+vr6+vr6+vr6+vr6+vr6+sAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==
) );
}

} # end package WindowsNotify ------------------------------------------


{ # begin package LinuxNotify ------------------------------------------
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

} # end package LinuxNotify -------------------------------------------

{ # begin package DummyNotify -----------------------------------------
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

} # end package DummyNotify -------------------------------------------

#######################################################################
{   # begin package main
use Class::Struct Post => [
                          boardId   => '$',
                          id        => '$',
                          topicId   => '$',
                          author    => '$',
                          flags     => '$',
                          time      => '$',
                          title     => '$',
                          length    => '$',
                          n         => '$',
                         ];
use Encode;
use File::Slurp;
use Getopt::Long;
use LWP::Simple;
use POSIX qw/setsid strftime/;
#use Proc::Daemon;
use threads;
use Thread::Queue;
use strict;
use warnings;

use constant {
    GUI_POLL_INTERVAL       =>  5000,       # 5s
    GUI_MAX_POSTS           =>  1000,
    GUI_CLEAR_AVOID_TIME    =>  5,          # 5s
};

my $opt_use_gui = 1;
my $opt_show_link = 0;
my $opt_use_notify = 1;
my @opt_boards = ();
my $opt_save = 1;
my $opt_board_list = 'board.list';
my $opt_help = 0;
my $opt_title_filter;
my $opt_filter_console = 0;

my $terminal_encoding;
my %lastPostIds;
my $got_sig_hup = 0;
my $tk_thread;
my $g_queue;
my $g_tkappname;

GetOptions(
    "board=s"   => \@opt_boards,
    "list=s"    => \$opt_board_list,
    "gui!"      => \$opt_use_gui,
    "notify!"   => \$opt_use_notify,
    "link!"     => \$opt_show_link,
    "save!"     => \$opt_save,
    "help"      => \$opt_help,
    "filter-console!"   => \$opt_filter_console,
    "title-filter=s"     => \$opt_title_filter,
);
usage() if $opt_help;

@opt_boards = split(/[;:,\s]/,join(',',@opt_boards));

select_notify_mechanism();

initialize_terminal();
$opt_title_filter = Encode::decode($terminal_encoding, $opt_title_filter)
        if defined $opt_title_filter;

load_board_list();
die "No board list specified, please specify it with --board or --list option.\n" .
    "Run `perl $0 --help` for details.\n" if @opt_boards == 0 && keys %lastPostIds == 0;

setup_signal_handlers();

# daemonize();

notify_start();

if ($opt_use_gui) {
    $g_queue = new Thread::Queue;
    $tk_thread = start_tk_thread($g_queue);
    $tk_thread->detach;

    die "Bad greeting from Tk thread!\n" if "ready" ne $g_queue->dequeue;
    $g_tkappname = $g_queue->dequeue;
    print "Tk thread's appname: $g_tkappname\n";
}

poll_newsmth_loop();

notify_stop();
save_board_list();

############################################################
#
sub parse_page {
    my $content = shift;
    # @a: ($postId, $topicId, $author, $flags, $time, $title, $bytes, $n)
    my (@a, @result);
    my ($begin, $end) = (0, 0);

    $begin = index($content, 'docWriter(');
    my ($board_id) = substr($content, $begin) =~ /(\d+)/;

    while (($begin = index($content, 'c.o(', $begin)) >= 0) {
        $end = index($content, ");\n", $begin);
        last if $end <= $begin;
        @a = substr($content, $begin + 4, $end - $begin) =~
                /(\d+),(\d+),'(.*)','(.*)',(\d+),'(.*)',(\d+),(\d+)/;

        my $post = new Post(boardId => $board_id,
                           id      => $a[0],
                           topicId => $a[1],
                           author  => $a[2],
                           flags   => $a[3],
                           time    => $a[4],
                           title   => $a[5],
                           length  => $a[6],
                           n       => $a[7],
                           );

        push @result, $post;
        $begin = $end;
    }

    return @result;
}


sub format_post {
    my $post = shift;
    my $result = sprintf "[%10s] %6d (%-12s) | %s",
                         strftime("%m-%d %H:%M", localtime($post->time)),   # time
                         $post->id,         # postId
                         $post->author,     # author
                         $post->title;      # title

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

    print STDERR "Got signal $sig\n" if defined $sig;

    if (keys %lastPostIds > 0 && $opt_save) {
        my @a;
        push @a, sprintf("%-32s        %s\n", "# board name", "last post id");

        my @boards = sort keys %lastPostIds;
        for my $board (@boards) {
            push @a, sprintf("%-32s        %d\n", $board, $lastPostIds{$board});
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
NewSMTH-Notifier v4.3
    --board "XXX,YYY"       specify board list in addition to a list file
    --list FILE             specify a board list file (default board.list)
    --gui, --no-gui         whether to use GUI. (default yes)
    --notify, --no-notify   whether to use notification. (default yes)
    --link, --no-link       whether to show links on console (default no)
    --save, --no-save       whether to save notifier state to board list
                            file (default yes)
    --help                  show this help and quit
    --filter-console, --no-filter-console
                            whether apply title filter for console output
                            (default no for convenience with GUI)
    --title-filter REGEX    set a regex to filter output by post's title
END
    exit 0;
}

sub select_notify_mechanism {
    if (! $opt_use_notify) {
        DummyNotify->import();
    } elsif ($^O eq 'MSWin32') {
        WindowsNotify->import();
    } elsif ($^O eq 'linux') {
        LinuxNotify->import();
    } else {
        warn "Use dummy notification implementation!\n";
        DummyNotify->import();
    }
}

sub initialize_terminal {
    $| = 1;
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
}

sub setup_signal_handlers {
    $SIG{HUP}  = sub { ++$got_sig_hup; };
    $SIG{INT}  = \&save_board_list;
    $SIG{KILL} = \&save_board_list;
    $SIG{QUIT} = \&save_board_list;
}

sub poll_newsmth_loop {
    while (notify_alive() && tk_thread_alive()) {
        if ($got_sig_hup > 0) {
            load_board_list();
            --$got_sig_hup;
        }

        print "# checking...........................\n";
        my $message = "";
        my $should_hurry = 0;

        my @boards = sort keys %lastPostIds;
        for my $board (@boards) {
            # At least in in LWP v5.827  get() returns decoded text, so $context is
            # encoded in Perl's internal encoding(UTF-8).   2010-02-10 lyb
            my $content;
            eval {
                $content = get('http://www.newsmth.net/bbsdoc.php?board=' . $board);
                $content = Encode::decode('GBK', $content) if ! Encode::is_utf8($content);
            };
            next unless defined $content;

            my @posts = parse_page($content);

            # Have we missed something?
            my $missed = 0;
            if (@posts > 0 && exists $lastPostIds{$board} && $lastPostIds{$board} > 0 &&
                    $posts[0]->id > $lastPostIds{$board}) {
                $missed = $posts[0]->id - $lastPostIds{$board};
                print "** WARN **: missed $missed posts at $board\n";
                $should_hurry = 1;
            }

            # Have we gotten something new?
            my @new = ();
            for my $post (@posts) {
                if (exists $lastPostIds{$board}) {
                    if ($lastPostIds{$board} < $post->id) {
                        $lastPostIds{$board} = $post->id;

                        if (defined $opt_title_filter) {
                            if ($post->title =~ /$opt_title_filter/io) {
                                push @new, $post;
                            } else {
                                next if $opt_filter_console;
                            }
                        } else {
                            push @new, $post;
                        }

                        if ($opt_show_link) {
                            printf "%-12s %-70s%s\n", $board, format_post($post), get_post_link($post);
                        } else {
                            printf "%-12s %s\n", $board, format_post($post);
                        }
                    }
                } else {
                    $lastPostIds{$board} = $post->id;
                }
            }

            if (@new > 0) {
                $message .= sprintf("%16s %d", $board, scalar(@new));
                if ($missed > 0) {
                    $message .= " ($missed missed)\n";
                } else {
                    $message .= "\n";
                }

                $g_queue->enqueue($board, $missed, \@new) if $opt_use_gui;
            }

            interruptable_sleep(rand 5);
        }

        notify_send($message) if length($message) > 0;

        # don't save, to avoid race condition with user edition on board list file.
        #save_board_list();
        interruptable_sleep(60) if ! $should_hurry && notify_alive() && tk_thread_alive();
    }
}

sub get_post_link {
    my $post = shift;
    return  "http://www.newsmth.net/bbscon.php?bid=" . $post->boardId . "&id=" . $post->id;
}

sub start_tk_thread {
    my $thread = threads->create(\&tk_thread_entry, $_[0]);
    return $thread;
}

sub tk_thread_entry {
    my $queue = shift;

    require Tk;
    Tk->import();

    use if $^O eq 'MSWin32', "Win32::Process";
    use if $^O eq 'MSWin32', "Win32";

    my $flag_autoscroll = 1;
    my $last_update_time = 0;

    my $mw = new MainWindow();
    my $toolbar = $mw->Frame()->pack(-expand => 1, -fill => "x");

    my $label_update_time = $toolbar->Label(
            -text => strftime("Last updated: %Y-%m-%d %H:%M:%S", localtime()),
        )->pack(-side => "left", -anchor => "e");

    my $hlist = $mw->Scrolled(qw/
        HList
        -header 1
        -columns    8
        -width      120
        -height     30
        -scrollbars ose
    /)->pack(qw/-expand 1 -fill both/);

    $hlist->configure(-font => ["system", 9]) if $^O eq 'MSWin32';

    $hlist->configure(
        -command    => sub {
            my $url = $hlist->info('data', $_[0]);
            return if ! (defined $url && length($url) > 0);

            if ($^O eq 'MSWin32') {
                # XXX: fork + exec crashes under ActivePerl-5.10.1.1007-MSWin32-x86-291969
                no strict "subs";   # XXX: avoid warnings about NORMAL_PRIORITY_CLASS and DETACHED_PROCESS

                my $windir = $ENV{windir};
                $windir = $ENV{SystemRoot} if ! defined $windir;
                $windir = "C:\\Windows" if ! defined $windir;

                my $child;
                Win32::Process::Create($child,
                    "$windir\\explorer.exe",
                    "explorer \"$url\"",
                    0,
                    NORMAL_PRIORITY_CLASS | DETACHED_PROCESS,
                    '.') || print STDERR Win32::FormatMessage(Win32::GetLastError());
            } else {
                my $pid = fork();
                if (defined $pid && $pid == 0) {
                    # child process
                    exec "x-www-browser", $url or 1;
                    exec "firefox", $url or 1;

                    exit(0);
                }
            }
        }
    );

    my $button = $toolbar->Button(
        -text       => "Clear",
        -command    => sub {
            # avoid deleting unread posts
            if (time() - $last_update_time > GUI_CLEAR_AVOID_TIME) {
                $hlist->delete("all");
            } else {
                my $oldtime = strftime("%H:%M:%S", localtime($last_update_time));
                $last_update_time = 0;
                $mw->messageBox(
                    -icon   => "warning",
                    -title  => "Avoid clearing unread posts!",
                    -type   => "Ok",
                    -message => "New posts have come since $oldtime that's in less than 5 seconds." .
                                "You can click \"Clear\" button immediately again to clear all posts " .
                                "if no *newer* post has come.",
                );
            }
        },
    )->pack(-side => "left", -anchor => "center", -expand => 1, -fill => "x");

    my $checkbtn = $toolbar->Checkbutton(
        -text       => "Auto scroll?",
        -variable   => \$flag_autoscroll,
    )->pack(-side => "left", -anchor => 'w');

    my $i = 0;
    for my $header (qw/Board Time PostId TopicId Author Flags Title Length/) {
        $hlist->header('create', $i++, -text => $header);
    }

    # stupid and inefficient way...
    $hlist->repeat(GUI_POLL_INTERVAL, sub {
            if ($queue->pending() >= 3) {
                $last_update_time = time();
                $label_update_time->configure(-text =>
                        strftime("Last updated: %Y-%m-%d %H:%M:%S", localtime($last_update_time)));

                do {
                    my ($board, $missed, $posts) = $queue->dequeue(3);
                    for my $post (@$posts) {
                        if ($flag_autoscroll) {
                            $hlist->see(tk_thread_addPost($hlist, $board, $post));
                        } else {
                            tk_thread_addPost($hlist, $board, $post);
                        }
                    }
                } while ($queue->pending() >= 3);

                my @children = $hlist->info('children');
                if (@children > GUI_MAX_POSTS) {
                    my $todeleted = int(GUI_MAX_POSTS / 10);
                    $todeleted = 1 if $todeleted == 0;

                    for (my $i = 0; $i < $todeleted; ++$i) {
                        $hlist->delete('entry', $children[$i]);
                    }
                }
            }

        });


    $queue->enqueue("ready");
    $queue->enqueue($mw->appname);
    MainLoop();
}


sub tk_thread_addPost {
    my ($hlist, $board, $post) = @_;

    my $child = $hlist->addchild('',
        -data   => get_post_link($post),
    );

    my @values = ($board, strftime("%m-%d %H:%M", localtime($post->time)),
                  $post->id, $post->topicId,
                  $post->author, $post->flags, $post->title, $post->length);

    for (my $i = 0; $i < @values; ++$i) {
        $hlist->itemCreate($child, $i, -text => $values[$i]);
    }

    return $child;
}

sub tk_thread_alive {
    if (defined $tk_thread) {
        return $tk_thread->is_running;
    } else {
        return 1;
    }
}

# avoid deferred signal delay on Windows
sub interruptable_sleep {
    if ($^O eq 'MSWin32') {
        my $seconds = shift;
        my $count = 2;

        if ($seconds <= 2) {
            sleep $seconds;
        } else {
            do {
                sleep $count;
                $seconds -= $count;
            } while ($seconds > 0);
        }
    } else {
        sleep $_[0];
    }
}

} # end package main

