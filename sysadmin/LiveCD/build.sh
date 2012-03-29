#!/bin/bash

set -e


ROOT="$1"


bootstrap() {
    [ ! -e "$ROOT" ] || [ -d "$ROOT" ]
    [ "`ls $ROOT`" ] && {
        echo "$ROOT isn't empty!" >&2
        exit 1
    }

    sudo cdebootstrap -a i386 --debug -v -f minimal sid ./sid http://mirrors.163.com/debian
}

