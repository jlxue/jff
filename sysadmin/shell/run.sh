#!/bin/sh

set -e -x

PATH=/bin:/sbin:/usr/bin:/usr/sbin

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

SCRIPTS=`cat<<END
install-packages.sh
locale.sh
firewall.sh
dns.sh
dhcp.sh
ntp.sh
kerberos.sh
ldap.sh
END`

for f in $SCRIPTS; do
    f=$SCRIPT_DIR/$f
    if [ -f $f ]; then
        echo ">>>>>>>>>>>>> $f"
        $f && echo "<<<<<<<<<<<<< $f" || {
            echo "ERROR: failed to run $f" >&2
            exit 1
        }
    fi
done

save_etc "save /etc after some config change"

