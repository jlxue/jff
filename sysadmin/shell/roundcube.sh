#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

#mkdir -p -m 0755 /srv/www/mail

######################################################################################
## install http_auth_autologin plugin
##
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
dummy='@@IMAP_AUTH_MASTER_PASSWORD@@'
set +x
parse_password_by_pattern "imap_auth_master_password['\"]\\]\\s*=\\s*['\"]([^'\"]+)" $f $dummy imap_passwd
substitude_template "$tmpl" "$f" 640 root:www-data CONF_CHANGED -e "s/$dummy/$imap_passwd/"
set -x
######################################################################################


######################################################################################
## setup Dovecot master user account
##
set +x
master_user=webmail
credential=$(doveadm pw -s DIGEST-MD5 -u webmail@corp.example.com -p "$imap_passwd")
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


######################################################################################
## setup managesieve plugin
##
f=/etc/roundcube/plugins/managesieve/config.inc.php
tmpl=$SCRIPT_DIR$f
dummy='@@MANAGESIEVE_AUTH_PW@@'
set +x
substitude_template "$tmpl" "$f" 640 root:www-data CONF_CHANGED -e "s/$dummy/$imap_passwd/"
set -x
######################################################################################


######################################################################################
## setup sieverules plugin
##
f=/etc/roundcube/plugins/sieverules/config.inc.php
tmpl=$SCRIPT_DIR$f
dummy='@@SIEVERULES_AUTH_PW@@'
set +x
substitude_template "$tmpl" "$f" 640 root:www-data CONF_CHANGED -e "s/$dummy/$imap_passwd/"
set -x
######################################################################################


######################################################################################
## setup Exim user account for Roundcube
##
sasldb=/etc/exim4/sasldb2
smtp_passwd=
set +x
sasldblistusers2 -f $sasldb 2>/dev/null | grep -q '^webmail@corp.example.com:' && {
    smtp_passwd=$(perl -we 'use DB_File; tie %h, "DB_File", $ARGV[0], O_RDONLY or die "$!"; print $h{"webmail\0corp.example.com\0userPassword"}, "\n"' $sasldb)
    [ -n "$smtp_passwd" ]
} || {
    smtp_passwd=`pwgen -cnys 24 1`
    echo "$smtp_passwd" | saslpasswd2 -p -c -u corp.example.com -f $sasldb webmail
    chmod 640 $sasldb
    chown root:Debian-exim $sasldb
}
f=/etc/roundcube/main.inc.php
tmpl=$SCRIPT_DIR$f
dummy='@@SMTP_PASS@@'
dummy2='@@IMAP_AUTH_PW@@'
substitude_template "$tmpl" "$f" 640 root:www-data CONF_CHANGED -e "s/$dummy/$smtp_passwd/" -e "s/$dummy2/$imap_passwd/"
set -x
######################################################################################


ensure_service_started postgresql postgres

f=/etc/dbconfig-common/roundcube.conf
tmpl=$SCRIPT_DIR$f
dummy='@@ROUNDCUBE_DB_PASSWORD@@'
isnew=
set +x
parse_password_by_pattern "dbc_dbpass\\s*=\\s*['\"]([^'\"]+)" $f $dummy db_passwd isnew
[ ! "$isnew" ] || pg_set_role_password roundcube "$db_passwd"

substitude_template "$tmpl" "$f" 600 root:root CONF_CHANGED -e "s/$dummy/$db_passwd/"

f=/etc/roundcube/debian-db.php
tmpl=$SCRIPT_DIR$f
dummy='@@ROUNDCUBE_DB_PASSWORD@@'
substitude_template "$tmpl" "$f" 640 root:www-data CONF_CHANGED -e "s/$dummy/$db_passwd/"
set -x


######################################################################################
cmp_dir $SCRIPT_DIR/etc/roundcube /etc/roundcube --exclude http_auth_autologin \
        --exclude managesieve --exclude sieverules --exclude main.inc.php \
        --exclude debian-db.php || {
    overwrite_dir $SCRIPT_DIR/etc/roundcube /etc/roundcube \
        --exclude http_auth_autologin --exclude managesieve --exclude sieverules \
        --exclude main.inc.php --exclude debian-db.php
    CONF_CHANGED=1
}

ensure_mode_user_group /etc/dbconfig-common                 755 root root
ensure_mode_user_group /etc/dbconfig-common/config          600 root root
ensure_mode_user_group /etc/dbconfig-common/roundcube.conf  600 root root
ensure_mode_user_group /etc/roundcube                       755 root root
ensure_mode_user_group /etc/roundcube/debian-db.php         640 root www-data
ensure_mode_user_group /etc/roundcube/main.inc.php          640 root www-data
ensure_mode_user_group /etc/roundcube/plugins/http_auth_autologin/config.inc.php 640 root www-data
ensure_mode_user_group /etc/roundcube/plugins/managesieve/config.inc.php    640 root www-data
ensure_mode_user_group /etc/roundcube/plugins/sieverules/config.inc.php     640 root www-data
ensure_mode_user_group /etc/dovecot/master-users            640 root dovecot
ensure_mode_user_group /etc/exim4/sasldb2                   640 root Debian-exim
#ensure_mode_user_group /srv/www/mail                        755 root root

[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

