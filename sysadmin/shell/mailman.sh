#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


# stolen from /etc/init.d/mailman
SITE_LIST=$( sed -rne "s/^[[:space:]]*MAILMAN_SITE_LIST[[:space:]]*=[[:space:]]*(['\"])([^'\"]+)\\1/\\2/p" /etc/mailman/mm_cfg.py )
[ -n "$SITE_LIST" ] || SITE_LIST='mailman'
/var/lib/mailman/bin/list_lists -b | grep -q "^${SITE_LIST}$" || {
    echo "You must run 'newlist $SITE_LIST' as root first!" >&2
    echo "Notice don't add the mailman aliases to /etc/aliases," >&2
    echo "because they are processed specially in /etc/exim4/conf.d/." >&22
    exit 1
}

cmp_dir $SCRIPT_DIR/etc/mailman /etc/mailman --exclude en --exclude zh_CN || {
    overwrite_dir_ignore_extra $SCRIPT_DIR/etc/mailman /etc/mailman
    /var/lib/mailman/bin/genaliases
    service mailman restart
}


ensure_mode_user_group /etc/mailman     755 root list

[ "`pgrep mailmanctl`" ] || service mailman start

