#!/bin/sh

set -e

grep 'sync_file\|sync_dir\|overwrite_file\|overwrite_dir' *.sh |
    fgrep '$SCRIPT_DIR' |
    sed -e 's/.*\$SCRIPT_DIR\(\S\+\).*/\1/' |
    sort -u

