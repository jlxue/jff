#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


ensure_mode_user_group /etc/mailman     755 root list

[ "`pidof mailman`" ] || service mailman start

