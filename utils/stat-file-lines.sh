#!/bin/sh
# 统计文件行数分布情况，第一列是行数上限，第二列是文件个数，
# 比如： find . -type f -name "*.java" | ./stat-file-lines.sh

xargs -l wc -l | perl -wane '$a[$F[0]/200]++; END {
    for (my $i=0; $i<@a; ++$i) {
        printf "%10d %d\n", ($i+1)*200, $a[$i] if defined $a[$i];
    }}'

