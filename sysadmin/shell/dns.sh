#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


# resolv.conf
#
cmp_file $SCRIPT_DIR/etc/resolvconf/resolv.conf.d/head /etc/resolvconf/resolv.conf.d/head || {
    overwrite_file $SCRIPT_DIR/etc/resolvconf/resolv.conf.d/head /etc/resolvconf/resolv.conf.d/head
    RESOLVCONF_CHANGED=1
}

cmp_file $SCRIPT_DIR/etc/resolvconf/resolv.conf.d/tail /etc/resolvconf/resolv.conf.d/tail || {
    overwrite_file $SCRIPT_DIR/etc/resolvconf/resolv.conf.d/tail /etc/resolvconf/resolv.conf.d/tail
    RESOLVCONF_CHANGED=1
}

[ -z "$RESOLVCONF_CHANGED" ] || service resolvconf restart


# Kerberos and LDAP are critical services, avoid resolving to Internet sites
# by mistake in case internal DNS server is down.
sync_file $SCRIPT_DIR/etc/hosts /etc/hosts
sync_file $SCRIPT_DIR/etc/hostname /etc/hostname
[ "`cat /etc/hostname`" = "`hostname`" ] || hostname "`cat /etc/hostname`"


# bind9
#

ensure_service_started bind9 named

# required by named-checkconf
mkdir -p $SCRIPT_DIR/var/cache/bind
named-checkconf -t $SCRIPT_DIR -z


cmp_dir $SCRIPT_DIR/etc/bind /etc/bind --exclude rndc.key || {
    overwrite_dir_ignore_extra $SCRIPT_DIR/etc/bind /etc/bind
    BIND9_CONFCHANGED=1
}


ensure_mode_user_group /etc/resolvconf/resolv.conf.d/head   644 root root
ensure_mode_user_group /etc/resolvconf/resolv.conf.d/tail   644 root root
ensure_mode_user_group /etc/hosts           644 root root
ensure_mode_user_group /etc/hostname        644 root root

ensure_mode_user_group /etc/bind            2755 root bind
ensure_mode_user_group /etc/bind/bind.keys   644 root bind
ensure_mode_user_group /etc/bind/db.0        644 root bind
ensure_mode_user_group /etc/bind/db.10       644 root bind
ensure_mode_user_group /etc/bind/db.127      644 root bind
ensure_mode_user_group /etc/bind/db.255      644 root bind
ensure_mode_user_group /etc/bind/db.corp     644 root bind
ensure_mode_user_group /etc/bind/db.empty    644 root bind
ensure_mode_user_group /etc/bind/db.local    644 root bind
ensure_mode_user_group /etc/bind/db.root     644 root bind
ensure_mode_user_group /etc/bind/named.conf                 644 root bind
ensure_mode_user_group /etc/bind/named.conf.default-zones   644 root bind
ensure_mode_user_group /etc/bind/named.conf.local           644 root bind
ensure_mode_user_group /etc/bind/named.conf.options         644 root bind
ensure_mode_user_group /etc/bind/rndc.key                   640 bind bind
ensure_mode_user_group /etc/bind/zones.rfc1918              644 root bind


[ -z "$BIND9_CONFCHANGED" ] || service bind9 reload

