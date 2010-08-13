#!/bin/bash
#
# Run `perldoc ./jump.pl` for document.
#
# Author:
#   Liu Yubao <yubao.liu at gmail.com>
#
# License:
#   GPL v3
#
# ChangeLog:
#   2010-04-15  Liu Yubao
#       * initial release, v0.1


j () {
    declare -a candidates
    declare -i i=0
    local from
    local __JUMP_PL=jump.pl

    [ "$1" = "-" -o -d "$1" ] && {
        from=$PWD
        cd "$1" && "$__JUMP_PL" -a add -f $from $PWD
        return
    }

    while read && [ "$REPLY" ]; do
        candidates[$((i++))]=$REPLY
    done < <("$__JUMP_PL" -a search "$1")

    [ ${#candidates[@]} -eq 0 ] && {
        echo "No candidate!" >&2
        return
    }

    [ ${#candidates[@]} -eq 1 ] && {
        from=$PWD
        cd ${candidates[0]} && "$__JUMP_PL" -a add -f $from $PWD
        return
    }

    while :; do
        for ((i=0; i < ${#candidates[@]}; ++i)); do
            printf "%4s  %s\n" $((i+1)) ${candidates[$i]}
        done | less -FRX

        printf "Which directory? [0 to quit, 1 ~ ${#candidates[@]} to select, or view again] "
        read

        # 0 to quit
        [ "0" = "$REPLY" ] && return

        # invalid number
        [ 0 -eq $(expr "$REPLY" : "[0-9]\+$") ] && continue

        i=$REPLY
        [ 0 -eq $((i <= 0 || i > ${#candidates[@]})) ] && {
            from=$PWD
            cd ${candidates[$((i-1))]} && "$__JUMP_PL" -a add -f $from $PWD
            return
        }
    done
}

