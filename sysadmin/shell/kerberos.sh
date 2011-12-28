#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

# Reference:
#   http://techpubs.spinlocksolutions.com/dklar/kerberos.html
#   Debian GNU: Setting up MIT Kerberos 5
#   Davor Ocelic
#   SPINLOCK - advanced GNU/Linux and Unix solutions for commercial and education sectors.

ensure_service_principal () {
    local principal="$1" keytab="$2"

    [ "$keytab" ] || keytab=/etc/krb5.keytab

    kadmin.local -q "get_principal -terse $principal" |
        grep -q '^\s*"\?'"$principal@" ||
            kadmin.local -q "add_principal -policy service -randkey $principal"

    # XXX: replace old keys?
    #
    # Also can use "klist -ket $keytab"
    k5srvutil -f $keytab list | grep -q "\s$principal@" || {
        kadmin.local -q "ktadd -k $keytab -norandkey $principal"
        chmod 600 $keytab
    }
}

ensure_policy () {
    local length="$1" classes="$2" policy="$3"

    kadmin.local -q "get_policy -terse $policy" | grep -q '^\s*"\?'"$policy"'"\?\s' || {
        kadmin.local -q "add_policy -minlength $length -minclasses $classes $policy"
        KDCDB_CHANGED=1
    }
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


ensure_policy 8 3 admin
ensure_policy 8 4 host
ensure_policy 8 4 service
ensure_policy 8 2 user


[ -z "$KRB5CONF_CHANGED$KDCCONF_CHANGED$KDCDB_CHANGED" ] ||
    service krb5-kdc restart

[ -z "$KRB5CONF_CHANGED$KDCCONF_CHANGED$KDCDB_CHANGED$KADM5ACL_CHANGED" ] ||
    service krb5-admin-server restart

[ "`pidof krb5kdc`" ] || service krb5-kdc start
[ "`pidof kadmind`" ] || service krb5-admin-server start


hostname=`hostname -f`
ensure_service_principal host/$hostname
ensure_service_principal ldap/$hostname /etc/slapd.keytab
ensure_service_principal HTTP/$hostname /etc/http.keytab

# Only used when Exim4 uses cyrus-sasl to authenticate clients
ensure_service_principal smtp/$hostname /etc/smtp.keytab
# Only used when Exim4 uses dovecot to authenticate clients
ensure_service_principal smtp/$hostname /etc/dovecot.keytab

ensure_service_principal imap/$hostname /etc/dovecot.keytab


ensure_mode_user_group /etc/slapd.keytab        600 openldap openldap
ensure_mode_user_group /etc/http.keytab         600 www-data www-data
ensure_mode_user_group /etc/smtp.keytab         600 Debian-exim Debian-exim
ensure_mode_user_group /etc/dovecot.keytab      600 dovecot dovecot

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

