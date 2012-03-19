#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

mkdir -p -m 0755 /srv/www/list

cmp_dir $SCRIPT_DIR/etc/mailman /etc/mailman --exclude en --exclude zh_CN || {
    overwrite_dir_ignore_extra $SCRIPT_DIR/etc/mailman /etc/mailman
    /var/lib/mailman/bin/genaliases
    CONF_CHANGED=1
}


ensure_mode_user_group /etc/mailman     755 root list
ensure_mode_user_group /srv/www/list    750 root www-data

[ -z "$CONF_CHANGED" ] || {
    service mailman restart
    service apache2 restart
}

[ "`pgrep mailmanctl`" ] || service mailman start
ensure_service_started apache2 apache2

