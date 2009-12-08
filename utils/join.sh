#!/bin/bash

# input file1:
# aaa bbb
# bbb ccc
#
# input file2:
# aaa
#
# output:
# aaa bbb
#
# Usage: join.sh file1 file2

join -1 2 <(cat -n "$1" | sort -k 2,2) <(sort "$2") |
    sort -k 2,2 | cut -d " " -f 1,3-

