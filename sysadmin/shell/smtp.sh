#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


cmp_file $SCRIPT_DIR/etc/default/spamassassin /etc/default/spamassassin || {
    overwrite_file $SCRIPT_DIR/etc/default/spamassassin /etc/default/spamassassin
    service spamassassin restart
}

sync_file $SCRIPT_DIR/etc/aliases /etc/aliases
sync_file $SCRIPT_DIR/etc/email-addresses /etc/email-addresses

cmp_file $SCRIPT_DIR/etc/mailname /etc/mailname || {
    overwrite_file $SCRIPT_DIR/etc/mailname /etc/mailname
    CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/exim4 /etc/exim4 --exclude passwd.client {
    overwrite_dir $SCRIPT_DIR/etc/exim4 /etc/exim4 --exclude passwd.client
    CONF_CHANGED=1
}


[ -z "$CONF_CHANGED" ] || {
    /usr/sbin/update-exim4.conf --verbose
    service exim4 reload
}


ensure_mode_user_group /etc/default/spamassassin    644 root root
ensure_mode_user_group /etc/exim4               755 root root
ensure_mode_user_group /etc/exim4/conf.d        755 root root
ensure_mode_user_group /etc/exim4/update-exim4.conf.conf    644 root root
ensure_mode_user_group /etc/exim4/exim4.conf.template       644 root root
ensure_mode_user_group /etc/exim4/passwd.client 640 root Debian-exim
ensure_mode_user_group /etc/mailname            644 root root
ensure_mode_user_group /etc/aliases             644 root root
ensure_mode_user_group /etc/email-addresses     644 root root


[ "`pidof clamd`" ] || service clamav-daemon start

[ "`pidof freshclam`" ] || service clamav-freshclam start

[ "`pgrep spamd`" ] || service spamassassin start

[ "`pidof exim4`" ] || service exim4 start

