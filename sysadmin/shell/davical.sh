#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

cmp_dir $SCRIPT_DIR/etc/davical /etc/davical || {
    overwrite_dir $SCRIPT_DIR/etc/davical /etc/davical
    CONF_CHANGED=1
}

[ -z "$CONF_CHANGED" ] || service apache2 restart

