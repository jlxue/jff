#!/usr/bin/perl
use strict;
use warnings;
use IO::Handle;

my @fhs;
my $n = shift;
tee($n);
for (0 .. 99) {
    print { $fhs[$_ % $n] } "$_\n"; # 父进程分发数据
}
close $_ for @fhs;

sub tee
{
    my $num = shift;
    while ($num--) {
        my $pid = open my $fh, "|-";
        die "cannot fork: $!" unless defined $pid;
        if ($pid) {
            autoflush $fh 1;
            push @fhs, $fh;
        } else {
            close $_ for @fhs;  # 否则父进程 close 不会导致文件真的关闭，从而子进程 <STDIN> 一直阻塞

            while (<STDIN>) {
                print "$$ $_"; # 子进程处理数据
            }
            exit;
        }
    }
}

