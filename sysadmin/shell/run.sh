#!/bin/sh

set -e -x

PATH=/bin:/sbin:/usr/bin:/usr/sbin

SCRIPT_DIR=`dirname $0`
. $SCRIPT_DIR/lib.sh

SCRIPTS=`cat<<END
install-packages.sh
locale.sh
firewall.sh
ntp.sh
kerberos.sh
END`

for f in $SCRIPTS; do
    f=$SCRIPT_DIR/$f
    if [ -f $f ]; then
        $f
    fi
done

save_etc "save /etc after some config change"

