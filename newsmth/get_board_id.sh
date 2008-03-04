#!/bin/bash
# usage:
#   ./get_board_id.sh < tech_boards.txt > idlist
#

prefix='http://www.newsmth.net/bbsdoc.php?board='
while read board; do
    wget -O - "$prefix$board" | grep docWriter
done

