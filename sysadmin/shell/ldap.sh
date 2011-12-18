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


# XXX: libsasl2-module-gssapi-mit <= 2.1.25.dfsg1-2 doesn't respect SASL
# "keytab" option, so we have to set KRB5_KTNAME environment in /etc/default/slapd.
# See http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=651308 .
cmp_file $SCRIPT_DIR/etc/ldap/sasl2/slapd.conf /etc/ldap/sasl2/slapd.conf || {
    overwrite_file $SCRIPT_DIR/etc/ldap/sasl2/slapd.conf /etc/ldap/sasl2/slapd.conf
    SLAPD_RESTART=1
}

cmp_file $SCRIPT_DIR/etc/default/slapd /etc/default/slapd || {
    overwrite_file $SCRIPT_DIR/etc/default/slapd /etc/default/slapd
    SLAPD_RESTART=1
}

pidfile=`slapcat -a cn=config -b cn=config | sed -ne 's/^olcPidFile\s*:\s*\([^[:space:]]\+\)\s*/\1/p'`
file_newer "$pidfile" /etc/slapd.keytab || {
    SLAPD_RESTART=1
}

[ -z "$SLAPD_RESTART" ] || service slapd restart


sync_file $SCRIPT_DIR/etc/cron.hourly/nss-updatedb /etc/cron.hourly/nss-updatedb


ensure_mode_user_group /etc/default/slapd   644 root root
ensure_mode_user_group /etc/ldap/sasl2/slapd.conf   644 root root
ensure_mode_user_group /etc/ldap/ldap.conf  644 root root
ensure_mode_user_group /etc/nsswitch.conf   644 root root
ensure_mode_user_group /etc/nslcd.conf      640 root nslcd
ensure_mode_user_group /etc/cron.hourly/nss-updatedb    755 root root

# depends on krb5 principals setup in kerberos.sh
[ "`pidof slapd`" ] || service slapd start
[ "`pidof nscd`" ] || service nscd start
[ "`pidof nslcd`" ] || service nslcd start

