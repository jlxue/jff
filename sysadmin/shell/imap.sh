#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


[ -d /srv/mail ] || mkdir -m 1777 /srv/mail


cmp_dir $SCRIPT_DIR/etc/dovecot /etc/dovecot --exclude master-users || {
    overwrite_dir $SCRIPT_DIR/etc/dovecot /etc/dovecot --exclude master-users
    service dovecot reload
}


ensure_mode_user_group /srv/mail                1777 root root
ensure_mode_user_group /etc/dovecot             755 root root
ensure_mode_user_group /etc/dovecot/conf.d      755 root root
ensure_mode_user_group /etc/dovecot/dovecot.conf            644 root root
ensure_mode_user_group /etc/dovecot/dovecot-db.conf.ext     640 root dovecot
ensure_mode_user_group /etc/dovecot/dovecot-dict-sql.conf.ext   640 root dovecot
ensure_mode_user_group /etc/dovecot/dovecot-sql.conf.ext    640 root dovecot
ensure_mode_user_group /etc/dovecot/dovecot-ldap.conf.ext   600 root root
ensure_mode_user_group /etc/dovecot/master-users            640 root dovecot


[ "`pidof dovecot`" ] || service dovecot start

