#!/usr/bin/perl
#
# Purpose:
#   Example to demonstrate how to use thread and
#   user defined message in Win32::GUI.
#
# Liu Yubao <yubao.liu@gmail.com>   2011-01-02
#
use strict;
use warnings;
use threads;
use Win32::GUI ();
use Win32::GUI::Constants qw/WM_USER/;
use constant MY_EVENT => WM_USER + 1;

my ($DOShwnd, $DOSinstance) = Win32::GUI::GetPerlWindow();
Win32::GUI::Hide($DOShwnd);
END {
    Win32::GUI::Show($DOShwnd);
}


my $mw = Win32::GUI::Window->new(
    -name   => 'Main',
    -width  => 300,
    -height => 200,
);

my $label = $mw->AddLabel(
    -text => 'Hello, world!',
    -width => 300,
);

$mw->Hook(MY_EVENT, \&onMyMessage);
$mw->Show();

my $thread = threads->create(\&thread_func, $mw);

Win32::GUI::Dialog();
$thread->kill('KILL')->detach();

##########################################################
sub Main_Terminate {
    -1;
}

sub onMyMessage {
    my ($win, $wParam, $lParam, $type, $msg) = @_;
    if ($type != 0 || $msg != MY_EVENT) {
        warn "Bad Message: @_\n";
        return 1;
    }

    $label->Text("Message from thread $wParam at " . localtime($lParam));
    return 1;
}

sub thread_func {
    my ($mw) = @_;
    $SIG{'KILL'} = sub { threads->exit() };

    while (1) {
        sleep(2);
        $mw->PostMessage(MY_EVENT, threads->tid(), time());
    }
}

