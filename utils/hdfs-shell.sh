#!/bin/bash

CMDS=" $(hadoop fs 2>&1 | fgrep '[-' | sed -e 's/^\s*\[-//' -e 's/\W.*//' | perl -e 'chomp(@a=<STDIN>); print qq{@a}') "
D="/user/`id -un`"

while read -p "D=$D PWD=$PWD$ " -e cmd args; do
    if [ "${cmd:0:1}" = "!" ]; then
        eval ${cmd:1} $args
    elif [[ "$CMDS" =~ " $cmd " ]]; then
        eval hadoop fs -$cmd $args
    elif [ "$cmd" = cd ]; then
        if [ "${args}" ]; then
            if [ "${args:0:1}" = "/" ]; then
                D="$args"
            elif [ "$args" = "~" ]; then
                D="/user/`id -un`"
            elif [ "${args:0:1}" = "~" ]; then
                D="/user/${args:1}"
            else
                D="$D/$args"
            fi
        else
            D="/user/`id -un`"
        fi
    else
        [ "$cmd" ] && eval $cmd $args
    fi

    D=$(perl -le '$_=$ARGV[0]; do { $d=$_; s@/+@/@g; s@(.)/$@\1@; $_="/" if $_ eq "/."; s@/\.(/|$)@\1@g; $_="/" if $_ eq "/.."; s@/[^/]+/\.\.(/|$)@\1@; } while ($_ ne $d); print' "$D")
done

