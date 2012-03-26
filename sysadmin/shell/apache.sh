#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


add_system_user_group "Trac account" /srv/trac trac trac svn
add_system_user_group "MoinMoin account" /srv/www/moin moin moin


mkdir -p -m 0755 /srv/www

cmp_dir $SCRIPT_DIR/etc/apache2 /etc/apache2 || {
    overwrite_dir $SCRIPT_DIR/etc/apache2 /etc/apache2
    CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/modsecurity /etc/modsecurity || {
    overwrite_dir $SCRIPT_DIR/etc/modsecurity /etc/modsecurity
    CONF_CHANGED=1
}


#cmp_dir $SCRIPT_DIR/var/www /var/www || {
#    overwrite_dir_ignore_extra $SCRIPT_DIR/var/www /var/www
#    CONF_CHANGED=1
#}

grep -q '^\s*date\.timezone\s*=' /etc/php5/cgi/php.ini || {
    timezone=$(cat /etc/timezone)
    [ "$timezone" ] && perl -i -ne "
        if (/^\\s*;\\s*date\\.timezone\\s*=\\s*\\S/) {
            s/^\\s*;\\s*//;
        } elsif (/^\\s*;\\s*date\\.timezone\\s*=/) {
            print \"date.timezone = $timezone\n\";
            next;
        }

        print;
        " /etc/php5/cgi/php.ini

    CONF_CHANGED=1
}

ensure_mode_user_group /srv/www         755 root root

[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

