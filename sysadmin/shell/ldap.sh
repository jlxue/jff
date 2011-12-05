#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

# Reference:
#   http://techpubs.spinlocksolutions.com/dklar/ldap.html
#   Debian GNU: Setting up OpenLDAP
#   Davor Ocelic
#   SPINLOCK — advanced GNU/Linux and Unix solutions for commercial and education sectors.


sync_file $SCRIPT_DIR/etc/ldap/ldap.conf /etc/ldap/ldap.conf
sync_file $SCRIPT_DIR/etc/nsswitch.conf /etc/nsswitch.conf
cmp_file $SCRIPT_DIR/etc/nslcd.conf /etc/nslcd.conf || {
    overwrite_file $SCRIPT_DIR/etc/nslcd.conf /etc/nslcd.conf
    service nslcd restart
}

ensure_mode_user_group /etc/ldap/ldap.conf  644 root root
ensure_mode_user_group /etc/nsswitch.conf   644 root root
ensure_mode_user_group /etc/nslcd.conf      640 root nslcd

# depends on krb5 principals setup in kerberos.sh
[ "`pidof slapd`" ] || service slapd start
[ "`pidof nscd`" ] || service nscd start
[ "`pidof nslcd`" ] || service nslcd start

