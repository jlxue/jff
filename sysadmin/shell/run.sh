#!/bin/sh

set -e -x

PATH=/bin:/sbin:/usr/bin:/usr/sbin

SCRIPT_DIR=`dirname $0`

SCRIPTS=`cat<<END
install-packages.sh
locale.sh
firewall.sh
END`

for f in $SCRIPTS; do
    f=$SCRIPT_DIR/$f
    if [ -f $f ]; then
        $f
    fi
done

! etckeeper unclean || etckeeper commit "save /etc after some config change"

