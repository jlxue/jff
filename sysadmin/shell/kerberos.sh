#!/bin/sh

set -e -x

SCRIPT_DIR=`dirname $0`
. $SCRIPT_DIR/lib.sh

# Reference:
#   http://techpubs.spinlocksolutions.com/dklar/kerberos.html
#   Debian GNU: Setting up MIT Kerberos 5
#   Davor Ocelic
#   SPINLOCK - advanced GNU/Linux and Unix solutions for commercial and education sectors.

grep -q "kerberos" /etc/hosts || {
    sed -i -e '$a \
127.0.0.1	kerberos.corp.example.com kerberos krb\
::1		kerberos.corp.example.com kerberos krb\
' /etc/hosts

    HOSTS_CHANGED=1
}

cmp_file $SCRIPT_DIR/etc/krb5.conf /etc/krb5.conf || {
    overwrite_file $SCRIPT_DIR/etc/krb5.conf /etc/krb5.conf
    KRB5CONF_CHANGED=1
}

cmp_file $SCRIPT_DIR/etc/krb5kdc/kdc.conf /etc/krb5kdc/kdc.conf || {
    overwrite_file $SCRIPT_DIR/etc/krb5kdc/kdc.conf /etc/krb5kdc/kdc.conf
    KDCCONF_CHANGED=1
}

cmp_file $SCRIPT_DIR/etc/krb5kdc/kadm5.acl /etc/krb5kdc/kadm5.acl || {
    overwrite_file $SCRIPT_DIR/etc/krb5kdc/kadm5.acl /etc/krb5kdc/kadm5.acl
    KADM5ACL_CHANGED=1
}


krb5kdc_db=/var/lib/krb5kdc/principal
[ -e $krb5kdc_db ] || {
    set +x      # don't record $pw on shell trace

    krb_realm=CORP.EXAMPLE.COM
    kdc_db_pw=/etc/krb5kdc/kdc_master_db_pw
    pw=`pwgen -cnys 24 1`

    ( date; echo $pw; echo; ) >> $kdc_db_pw
    chmod 600 $kdc_db_pw

    ( echo $pw; echo $pw; ) | kdb5_util -r $krb_realm -d $krb5kdc_db create -s

    set -x

    KDCDB_CHANGED=1
}


kadmin.local -q "get_policy -terse admin" | grep -q '^\s*"\?admin"\?\s' || {
    echo 'add_policy -minlength 8 -minclasses 3 admin' | kadmin.local
    KDCDB_CHANGED=1
}

kadmin.local -q "get_policy -terse host" | grep -q '^\s*"\?host"\?\s' || {
    echo 'add_policy -minlength 8 -minclasses 4 host' | kadmin.local
    KDCDB_CHANGED=1
}

kadmin.local -q "get_policy -terse service" | grep -q '^\s*"\?service"\?\s' || {
    echo 'add_policy -minlength 8 -minclasses 4 service' | kadmin.local
    KDCDB_CHANGED=1
}

kadmin.local -q "get_policy -terse user" | grep -q '^\s*"\?user"\?\s' || {
    echo 'add_policy -minlength 8 -minclasses 2 user' | kadmin.local
    KDCDB_CHANGED=1
}


[ -z "$KRB5CONF_CHANGED$KDCCONF_CHANGED$KDCDB_CHANGED" ] ||
    service krb5-kdc restart

[ -z "$KRB5CONF_CHANGED$KDCCONF_CHANGED$KDCDB_CHANGED$KADM5ACL_CHANGED" ] ||
    service krb5-admin-server restart

[ "`pidof krb5kdc`" ] || service krb5-kdc start
[ "`pidof kadmind`" ] || service krb5-admin-server start


ensure_mode_user_group /etc/hosts               644 root root
ensure_mode_user_group /etc/krb5.conf           644 root root
ensure_mode_user_group /etc/krb5.keytab         600 root root
ensure_mode_user_group /etc/krb5kdc             700 root root
ensure_mode_user_group /etc/krb5kdc/kadm5.acl   644 root root
ensure_mode_user_group /etc/krb5kdc/kadm5.keytab        600 root root
ensure_mode_user_group /etc/krb5kdc/kdc.conf            644 root root
ensure_mode_user_group /etc/krb5kdc/kdc_master_db_pw    600 root root
ensure_mode_user_group /etc/krb5kdc/stash       600 root root
ensure_mode_user_group /var/lib/krb5kdc/        700 root root

