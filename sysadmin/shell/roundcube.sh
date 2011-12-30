#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

#mkdir -p -m 0755 /srv/www/mail

cmp_file $SCRIPT_DIR/etc/dbconfig-common/roundcube.conf /etc/dbconfig-common/roundcube.conf || {
    overwrite_file $SCRIPT_DIR/etc/dbconfig-common/roundcube.conf /etc/dbconfig-common/roundcube.conf
    CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/roundcube /etc/roundcube || {
    overwrite_dir $SCRIPT_DIR/etc/roundcube /etc/roundcube
    CONF_CHANGED=1
}

ensure_mode_user_group /etc/dbconfig-common                 755 root root
ensure_mode_user_group /etc/dbconfig-common/config          600 root root
ensure_mode_user_group /etc/dbconfig-common/roundcube.conf  600 root root
ensure_mode_user_group /etc/roundcube                       755 root root
ensure_mode_user_group /etc/roundcube/debian-db.php         640 root www-data
ensure_mode_user_group /etc/roundcube/main.inc.php          640 root www-data
#ensure_mode_user_group /srv/www                             755 root root
#ensure_mode_user_group /srv/www/mail                        755 root root


[ -z "$CONF_CHANGED" ] || service apache2 restart

[ "`pidof apache2`" ] || service apache2 start

