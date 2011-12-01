#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


checkzones () {
    while [ "$1" ]; do
        named-checkzone -t $SCRIPT_DIR -w /var/cache/bind \
            -i full -k fail -m fail -M fail -n fail -r fail -S fail -W warn \
            "$1" "$2"

        shift 2
    done
}


[ "`pidof named`" ] || service bind9 start

# required by named-checkconf and named-checkzone
mkdir -p $SCRIPT_DIR/var/cache/bind
named-checkconf -t $SCRIPT_DIR -z

checkzones `perl -nwe '
    if ( /^\s*zone\s+"([^"]+)"/ ) {
        $zone = $1;
    } elsif ( /^\s*type\s+master/ ) {
        $master = 1;
    } elsif ( /^\s*file\s+"([^"]+)"/ ) {
        print "$zone $1\n" if $master;
        undef $master;
    }' $SCRIPT_DIR/etc/bind/named.conf.*zones`


cmp_dir $SCRIPT_DIR/etc/bind /etc/bind || {
    overwrite_dir $SCRIPT_DIR/etc/bind /etc/bind
    service bind9 reload
}


ensure_mode_user_group /etc/bind            2755 root bind
ensure_mode_user_group /etc/bind/bind.keys   644 root root
ensure_mode_user_group /etc/bind/db.0        644 root root
ensure_mode_user_group /etc/bind/db.127      644 root root
ensure_mode_user_group /etc/bind/db.255      644 root root
ensure_mode_user_group /etc/bind/db.empty    644 root root
ensure_mode_user_group /etc/bind/db.local    644 root root
ensure_mode_user_group /etc/bind/db.root     644 root root
ensure_mode_user_group /etc/bind/named.conf                 644 root bind
ensure_mode_user_group /etc/bind/named.conf.default-zones   644 root bind
ensure_mode_user_group /etc/bind/named.conf.local           644 root bind
ensure_mode_user_group /etc/bind/named.conf.options         644 root bind
ensure_mode_user_group /etc/bind/rndc.key                   640 bind bind
ensure_mode_user_group /etc/bind/zones.rfc1918              644 root root

