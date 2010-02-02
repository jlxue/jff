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
#       2010-02-01
#           * add some options, release 3.1
#       2010-02-02
#           * support Tk preview window, release 4.0
#
#  Dieken at newsmth.net
#
#  This script is released under GNU GPL v3.
#
#  TODO: cleanup code, port to Linux
#

#########################################################
## system tray notification

{ # begin package WindowsNotify ------------------------
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

my $opt_use_gui = 1;
my $opt_show_link = 0;
my $opt_use_notify = 1;
my @opt_boards = ('Perl');
my $opt_save = 1;
my $opt_board_list = 'board.list';
my $opt_help = 0;

my $terminal_encoding;
my %lastPostIds;
my $got_sig_hup = 0;
my $tk_thread;
my $g_queue;
my $g_tkappname;

GetOptions(
    "board=s"   => \@opt_boards,
    "gui!"      => \$opt_use_gui,
    "help"      => \$opt_help,
    "link!"     => \$opt_show_link,
    "list=s"    => \$opt_board_list,
    "notify!"      => \$opt_use_notify,
    "save!"     => \$opt_save,
);
usage() if $opt_help;

@opt_boards = split(/[;:,\s]/,join(',',@opt_boards));

select_notify_mechanism();

initialize_terminal();

load_board_list();

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
NewSMTH-Notifier v4.0
    --board "XXX,YYY"       specify board list in addition to a list file
    --gui, --no-gui         whether to use GUI. (default yes)
    --help                  show this help and quit
    --link, --no-link       whether to show links on console (default yes)
    --list FILE             specify a board list file (default board.list)
    --notify, --no-notify   whether to use notification. (default yes)
    --save, --no-save       whether to save notifier state to board list
                            file (default yes)
END
    exit 0;
}

sub select_notify_mechanism {
    if (! $opt_use_notify) {
        DummyNotify->import();
    } elsif ($^O eq 'MSWin') {
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
                        push @new, $post;
                        if ($opt_show_link) {
                            printf "%-90s%s\n",  $board . ': ' . format_post($post),
                                                  get_post_link($post);
                        } else {
                            print $board, ': ', format_post($post), "\n";
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

            sleep rand 5;
        }

        notify_send($message) if length($message) > 0;

        # don't save, to avoid race condition with user edition on board list file.
        #save_board_list();
        sleep 60 if ! $should_hurry && notify_alive() && tk_thread_alive();
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

    my $flag_autoscroll = 1;

    my $mw = new MainWindow();

    my $checkbtn = $mw->Checkbutton(
        -text       => "Auto scroll?",
        -variable   => \$flag_autoscroll,
    )->pack();

    my $hlist = $mw->Scrolled(qw/
        HList
        -header 1
        -columns    8
        -width      120
        -height     30
        -scrollbars ose
    /)->pack(qw/-expand 1 -fill both/);

    my $button = $mw->Button(
        -text       => "Clear",
        -command    => sub {
            $hlist->delete("all");
        },
    )->pack();

    my $i = 0;
    for my $header (qw/Board Time PostId TopicId Author Flags Title Length/) {
        $hlist->header('create', $i++, -text => $header);
    }

    # stupid and inefficient way...
    $hlist->repeat(5000, sub {
            while ($queue->pending() >= 3) {
                my ($board, $missed, $posts) = $queue->dequeue(3);
                for my $post (@$posts) {
                    if ($flag_autoscroll) {
                        $hlist->see(tk_thread_addPost($hlist, $board, $post));
                    } else {
                        tk_thread_addPost($hlist, $board, $post);
                    }
                }
            }
        });

    #$hlist->bind(<<newposts>>, sub {...});

    $queue->enqueue("ready");
    $queue->enqueue($mw->appname);
    MainLoop();
}


sub tk_thread_addPost {
    my ($hlist, $board, $post) = @_;

    my $child = $hlist->addchild('');
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

} # end package main

