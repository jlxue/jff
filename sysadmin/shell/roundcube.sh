#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

#mkdir -p -m 0755 /srv/www/mail

######################################################################################
## install http_auth_autologin plugin
cmp_dir $SCRIPT_DIR/usr/share/roundcube/plugins/http_auth_autologin /usr/share/roundcube/plugins/http_auth_autologin || {
    overwrite_dir $SCRIPT_DIR/usr/share/roundcube/plugins/http_auth_autologin /usr/share/roundcube/plugins/http_auth_autologin
    CONF_CHANGED=1
}

[ -L /var/lib/roundcube/plugins/http_auth_autologin ] &&
    [ "`readlink /var/lib/roundcube/plugins/http_auth_autologin`" = /usr/share/roundcube/plugins/http_auth_autologin ] || {
        rm -rf /var/lib/roundcube/plugins/http_auth_autologin
        ln -s /usr/share/roundcube/plugins/http_auth_autologin /var/lib/roundcube/plugins/http_auth_autologin
        CONF_CHANGED=1
}

mkdir -p -m 0755 /etc/roundcube/plugins/http_auth_autologin
f=/etc/roundcube/plugins/http_auth_autologin/config.inc.php
tmpl=/usr/share/roundcube/plugins/http_auth_autologin/config.inc.php.dist
master_user=$(grep imap_auth_master_username $tmpl | sed -e "s/.*=\s*['\"]//; s/['\"].*//")
dummy=$(grep imap_auth_master_password $tmpl | sed -e "s/.*=\s*['\"]//; s/['\"].*//")
set +x
[ ! -e $f ] || {
    passwd=$(grep imap_auth_master_password $f | sed -e "s/.*=\s*['\"]//; s/['\"].*//")
    [ "$passwd" != "$dummy" ] || passwd=
}
[ "$passwd" ] || passwd=`pwgen -cnys 24 1`
sed -e "s/$dummy/$passwd/" $tmpl | diff -q $f - 2>/dev/null || {
    sed -e "s/$dummy/$passwd/" $tmpl > $f
    chmod 640 $f
    chown root:www-data $f
    CONF_CHANGED=1
}

credential=$(doveadm pw -s CRAM-MD5 -p "$passwd")
masterdb=/etc/dovecot/master-users
grep -q "^$master_user:$credential$" $masterdb 2>/dev/null || {
    sed -i -e "s/^$master_user:.*/$master_user:$credential/" $masterdb ||
        echo "$master_user:$credential" >> $masterdb
    chmod 640 $masterdb
    chown root:dovecot $masterdb
    service dovecot restart
}
set -x
######################################################################################


cmp_file $SCRIPT_DIR/etc/dbconfig-common/roundcube.conf /etc/dbconfig-common/roundcube.conf || {
    overwrite_file $SCRIPT_DIR/etc/dbconfig-common/roundcube.conf /etc/dbconfig-common/roundcube.conf
    CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/roundcube /etc/roundcube --exclude http_auth_autologin || {
    overwrite_dir $SCRIPT_DIR/etc/roundcube /etc/roundcube --exclude http_auth_autologin
    CONF_CHANGED=1
}

ensure_mode_user_group /etc/dbconfig-common                 755 root root
ensure_mode_user_group /etc/dbconfig-common/config          600 root root
ensure_mode_user_group /etc/dbconfig-common/roundcube.conf  600 root root
ensure_mode_user_group /etc/roundcube                       755 root root
ensure_mode_user_group /etc/roundcube/debian-db.php         640 root www-data
ensure_mode_user_group /etc/roundcube/main.inc.php          640 root www-data
ensure_mode_user_group /etc/roundcube/plugins/http_auth_autologin/config.inc.php 640 root www-data
ensure_mode_user_group /etc/dovecot/master-users            640 root dovecot
#ensure_mode_user_group /srv/www                             755 root root
#ensure_mode_user_group /srv/www/mail                        755 root root


[ -z "$CONF_CHANGED" ] || service apache2 restart

[ "`pidof apache2`" ] || service apache2 start

