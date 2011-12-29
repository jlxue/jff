#!/bin/sh

set -e

ROOT="$1"

[ -d "$ROOT" ] || {
    echo "Argument [$ROOT] isn't directory!" >&2
    exit 1
}

while read f; do
    [ "$f" ] || continue

    [ -e "$ROOT/$f" ] || {
        echo "$ROOT/$f not found!" >&2
        continue
    }

    sudo mkdir -p ./`dirname $f`
    if [ -d "$ROOT/$f" ]; then
        sudo rsync -avr --delete "$ROOT/$f/" ./$f
    else
        sudo rsync -avr --delete "$ROOT/$f" ./$f
    fi
done

find etc | while read f; do [ -r $f ] || echo $f; done

