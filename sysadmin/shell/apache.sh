#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


cmp_dir $SCRIPT_DIR/etc/apache2 /etc/apache2 || {
    overwrite_dir $SCRIPT_DIR/etc/apache2 /etc/apache2
    CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/modsecurity /etc/modsecurity || {
    overwrite_dir $SCRIPT_DIR/etc/modsecurity /etc/modsecurity
    CONF_CHANGED=1
}


[ -z "$CONF_CHANGED" ] || service apache2 restart

[ "`pidof apache2`" ] || service apache2 start

