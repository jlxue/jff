#!/bin/sh

ulimit -d 768000
ulimit -m 512000
ulimit -u 100
ulimit -v 768000

ulimit -a

echo
echo "$@"
"$@"

