#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


mkdir -p -m 0755 /srv/www/moin

{
    dir=/srv/www/moin/MainWiki

    [ -d $dir ] || mkdir -m 0755 $dir

    [ -d $dir/data ] || {
        cp -a /usr/share/moin/data $dir/data
        chown -R moin:moin $dir/data
    }

    [ -d $dir/underlay ] || {
        cp -a /usr/share/moin/underlay $dir/underlay
        chown -R moin:moin $dir/underlay
    }
}


cmp_dir $SCRIPT_DIR/etc/moin /etc/moin || {
    overwrite_dir $SCRIPT_DIR/etc/moin /etc/moin
    CONF_CHANGED=1
}


ensure_mode_user_group /srv/www/moin            750 root moin
ensure_mode_user_group /srv/www/moin/MainWiki   750 root moin
ensure_mode_user_group /etc/moin                755 root root


[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

