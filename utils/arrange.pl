#!/usr/bin/perl
use Data::Dumper;
use Storable;
use strict;
use warnings;

my @persons = ('name1', 'name2');

my $arrangement = {};

if (arrange(1, $arrangement)) {
    print Dumper($arrangement);
}

# 参数：$n 表示班次：1..14 表示 7 天白天、晚上 14 个班次
#       $h_ref 一个 hash 的引用，表示事件安排，格式为：
#           {
#               班次 => { name1 => 1, name2 => 1},
#               name1 => { 班次1 => 1, 班次2 => 1},
#           }
sub arrange {
    my ($n, $arrangement) = @_;

    return 1 if $n > 14;    # 已经排完了

    for my $person (@persons) {
        # 复制一份安排表，失败回溯时从原来状态开始
        my $h = dclone($arrangement);

        if (can_arrange($n, $person, $h)) {
            $h->{$person}{$n} = 1;
            $h->{$n}{$person} = 1;

            if (arrange($n + 1, $h)) {
                %$arrangement = %$h;
                return 1;
            }
        }
    }

    return 0;
}

sub can_arrange {
    my ($n, $person, $h) = @_;

    return 1;
}

