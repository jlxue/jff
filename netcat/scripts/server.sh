#!/bin/bash

PORT="$1"
PROGRAM="$2"

[ -z "$PROGRAM" ] && {
    echo "Usage: $0 PORT PROGRAM" >&2
    exit 1
}

[ -x "$PROGRAM" ] || {
    echo "$PROGRAM isn't executable!" >&2
    exit 1
}

socat TCP-LISTEN:$PORT,backlog=10,fork,reuseaddr EXEC:$PROGRAM,nofork &

