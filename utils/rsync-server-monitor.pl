#!/usr/bin/perl
#
# monitor transfering progress information on the rsync server side
#
# Usage:
#   perl rsync-server-monitor.pl RSYNC-SERVER-PID
#
#   This script gets information from /proc/PID/fd/3 and /proc/PID/fdinfo/3.
#
use File::Basename;
#use Smart::Comments;
use strict;
use warnings;

my $pid = shift;
die "Usage: $0 RSYNC-SERVER-PID\n" if !defined $pid;

die "PID $pid has quited or isn't rsync!\n" if ! go_on($pid);
for ("fd", "fdinfo") {
    my $d = "/proc/$pid/$_";
    die "$d doesn't exist!\n" if ! -d $d;
}


my ($file, $dev, $ino, $size, $oldpos, $curpos);
$dev = $ino = $size = $oldpos = 0;

for (;;) {
    $file = get_filename($pid);
    last if !defined $file;
    ### $file

    my @st = get_stat($pid);
    last if @st == 0;
    ### @st

    $curpos = get_pos($pid);
    last if !defined $curpos;
    ### $curpos

    if ($st[0] == $dev && $st[1] == $ino && $size >= $curpos) {
        my $rate = $curpos - $oldpos;
        $rate = 1 if $rate == 0;
        $rate = $rate / 1024;

        printf "%s   size=%.1f KB pos=%.1f KB %.0f%% %.1f KB/s (ETA %.1f min)\t\n",
               $file, $size / 1024, $curpos / 1024,
               $size > 0 ? $curpos / $size * 100 : 100,
               $rate,
              ($size - $curpos) / 1024 / $rate / 60;

        $oldpos = $curpos;
    } else {
        # avoid race condition, the transfering file has changed.
        $oldpos = 0;

        printf "%s   size=%.1f KB pos=%.1f KB %.0f%% -- KB/s (ETA -- min)\t\n",
               $file, $size / 1024, $curpos / 1024,
               $size > 0 ? $curpos / $size * 100 : 100;
    }

    $dev = $st[0];
    $ino = $st[1];
    $size = $st[7];

    sleep 1;
}

##############################################################
sub go_on {
    my $pid = shift;

    my $exe = readlink "/proc/$pid/exe";
    if (defined $exe && $exe =~ /rsync$/) {
        return 1;
    } else {
        return 0;
    }
}


sub get_filename {
    my $pid = shift;
    my $filename;

    while (go_on($pid)) {
        $filename = readlink "/proc/$pid/fd/3";
        if (! defined $filename) {
            sleep 1;
            next;
        }

        last;
    }

    return $filename;
}


sub get_pos {
    my $pid = shift;
    my $pos;

    while (go_on($pid)) {
        my $fh;
        if (! open $fh, "/proc/$pid/fdinfo/3") {
            sleep 1;
            next;
        }

        ($pos) = scalar(<$fh>) =~ /(\d+)/;
        close $fh;
        last;
    }

    return $pos;
}


sub get_stat {
    my $pid = shift;

    while (go_on($pid)) {
        my @st = stat "/proc/$pid/fd/3";
        if (@st == 0) {
            sleep 1;
            next;
        }

        return @st;
    }

    return ();
}

