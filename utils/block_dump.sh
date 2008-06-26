#!/bin/bash
#
# From:
# http://blog.csdn.net/guoguo1980/archive/2008/04/27/2333817.aspx
#

/etc/init.d/syslog stop || exit 1

echo 1 > /proc/sys/vm/block_dump

sleep 60

dmesg | awk '/(READ|WRITE|dirtied)/ {process[$1]++} END {for (x in process) \
    print process[x],x}' |sort -nr |awk '{print $2 " " $1}' | \
    head -n 10

echo 0 > /proc/sys/vm/block_dump

/etc/init.d/syslog start

