#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

shorewall check $SCRIPT_DIR/etc/shorewall
shorewall6 check $SCRIPT_DIR/etc/shorewall6

sync_file $SCRIPT_DIR/etc/default/shorewall-init /etc/default/shorewall-init
sync_file $SCRIPT_DIR/etc/default/shorewall /etc/default/shorewall
sync_file $SCRIPT_DIR/etc/default/shorewall6 /etc/default/shorewall6

cmp_dir $SCRIPT_DIR/etc/shorewall /etc/shorewall || {
    overwrite_dir $SCRIPT_DIR/etc/shorewall /etc/shorewall/
    service shorewall restart
}

cmp_dir $SCRIPT_DIR/etc/shorewall6 /etc/shorewall6 || {
    overwrite_dir $SCRIPT_DIR/etc/shorewall6 /etc/shorewall6/
    service shorewall6 restart
}

