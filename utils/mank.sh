#!/bin/bash
#
# A manual viewer for UNIX-like systems.
#
# Requirements:
#   bash, dialog or whiptail, apropos, mandb
#   (Usually they have been installed on your system.)
#
# Usage:
#   ./mank                      # let mank select which tool
#   DIALOG=whiptail ./mank      # prefer whiptail
#   ./mank -s 2                 # only search in section 2.
#
# Author:
#   Liu Yubao <yubao.liu@gmail.com>
#
# Licence:
#   GPLv3.
#
# ChangeLog:
#   2008-04-29    Liu Yubao
#       * initial version v0.1
#

: ${DIALOG:=dialog}

{
    which $DIALOG || {
        DIALOG=dialog
        which $DIALOG || {
            DIALOG=whiptail
            which $DIALOG || DIALOG=""
        }
    }
} >/dev/null 2>&1

if [ -z "$DIALOG" ]; then
    echo Please install dialog or whiptail first. >&2
    exit 1
fi

which apropos >/dev/null 2>&1 || {
    echo "Can't find \`apropos\` command, will switch to \`man -k\`." >&2
    APROPOS_CMD="man -k"
}


while [ -n "$1" ]; do
    if [ "-s" = $1 ]; then
        SECTION=$2
        break
    fi
    shift
done
[ -z "$APROPOS_CMD" ] && {
    [ -z "$SECTION" ] && APROPOS_CMD="apropos" ||
        APROPOS_CMD="apropos -s $SECTION"
}


tempfile=`tempfile 2>/dev/null` || tempfile=/tmp/mank$$
trap "rm -f $tempfile" 0 1 2 5 15

shopt -s -q extglob


while $DIALOG --inputbox "$APROPOS_CMD" 16 51 2>$tempfile ; do
    $APROPOS_CMD `cat $tempfile` >$tempfile 2>/dev/null

    if [ ! -s $tempfile ]; then
        $DIALOG --msgbox "nothing appropriate." 16 51
        continue
    fi

    if grep ": nothing appropriate." $tempfile >/dev/null; then
        $DIALOG --msgbox "`cat $tempfile`" 16 51
    else
        n=0
        while read item; do
            items[$((n++))]=${item%%+( )-*}
            items[$((n++))]=${item#*+( )- }
        done < $tempfile

        item=${items[0]}
        while $DIALOG --default-item "$tag"    \
                --menu "Select an item to see its manual."  \
                0 0 0 "${items[@]}" 2>$tempfile ; do
            item=`cat $tempfile`
            page=${item%% *}
            section=${item#*\(}
            section=${section%)*}
            man "$section" "$page"
        done
    fi
done

