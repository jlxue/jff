#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

ensure_service_started postgresql postgres

su postgres -c 'psql -c "" davical' 2>/dev/null || {
    su postgres -c /usr/share/davical/dba/create-database.sh
}

cmp_dir $SCRIPT_DIR/etc/davical /etc/davical || {
    overwrite_dir $SCRIPT_DIR/etc/davical /etc/davical
    CONF_CHANGED=1
}

ensure_mode_user_group /etc/davical/administration.yml      640 root www-data
ensure_mode_user_group /etc/davical/config.php              640 root www-data

[ -z "$CONF_CHANGED" ] || service apache2 restart

