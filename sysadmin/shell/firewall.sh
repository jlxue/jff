#!/bin/sh

set -e

ROOT=`dirname $0`

shorewall check $ROOT/etc/shorewall
shorewall6 check $ROOT/etc/shorewall6

cmp $ROOT/etc/default/shorewall-init /etc/default/shorewall-init ||
    rsync -av --no-owner --no-group $ROOT/etc/default/shorewall-init /etc/default/

diff -aurNq /etc/shorewall/ $ROOT/etc/shorewall/ >/dev/null || {
    rsync -avr --no-owner --no-group --delete $ROOT/etc/shorewall/ /etc/shorewall/
    service shorewall restart
}

diff -aurNq /etc/shorewall6/ $ROOT/etc/shorewall6/ >/dev/null || {
    rsync -avr --no-owner --no-group --delete $ROOT/etc/shorewall6/ /etc/shorewall6/
    service shorewall6 restart
}

