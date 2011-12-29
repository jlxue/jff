#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


# stolen from /etc/init.d/mailman
SITE_LIST=$( sed -rne "s/^[[:space:]]*MAILMAN_SITE_LIST[[:space:]]*=[[:space:]]*(['\"])([^'\"]+)\\1/\\2/p" /etc/mailman/mm_cfg.py )
[ -n "$SITE_LIST" ] || SITE_LIST='mailman'
/var/lib/mailman/bin/list_lists -b | grep -q "^${SITE_LIST}$" || {
    echo "You must run 'newlist $SITE_LIST' as root first!" >&2
    exit 1
}

ensure_mode_user_group /etc/mailman     755 root list

[ "`pidof mailman`" ] || service mailman start

