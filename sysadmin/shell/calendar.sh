#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

cmp_file $SCRIPT_DIR/etc/default/calendarserver /etc/default/calendarserver || {
    overwrite_file $SCRIPT_DIR/etc/default/calendarserver /etc/default/calendarserver
    CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/caldavd /etc/caldavd || {
    overwrite_dir $SCRIPT_DIR/etc/caldavd /etc/caldavd
    CONF_CHANGED=1
}

[ -z "$CONF_CHANGED" ] || service calendarserver restart

## calendarserver starts memcached itself, see
##  /usr/lib/twisted-calendarserver/lib/python2.6/site-packages/calendarserver/tap/caldav.py
##  /usr/lib/twisted-calendarserver/lib/python2.6/site-packages/twistedcaldav/config.py
##
#ensure_service_started memcached memcached

# calendarserver requires "user_xattr" mount option for /var/spool/caldavd
mount_point=$(df /var/spool/caldavd | tail -1 | awk '{print $6}')
[ "$mount_point" ] && {
    perl -ane "print qq{\$F[3]\\n} if m{^/dev\S+\s\Q$mount_point\E\s}" /proc/mounts |
        grep -wq user_xattr || {
            echo "WARN: $mount_point isn't mounted with user_xattr option" >&2
            mount -o remount,user_xattr "$mount_point"
        }
}

[ "`pgrep -f caldavd`" ] || service calendarserver start

