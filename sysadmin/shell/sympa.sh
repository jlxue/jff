#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


add_system_user_group "Sympa mailing list manager" /var/lib/sympa sympa sympa

cmp_file $SCRIPT_DIR/etc/rsyslog.d/sympa.conf /etc/rsyslog.d/sympa.conf || {
    overwrite_file $SCRIPT_DIR/etc/rsyslog.d/sympa.conf /etc/rsyslog.d/sympa.conf
    service rsyslog restart
}


ensure_mode_user_group /etc/rsyslog.d               755 root root
ensure_mode_user_group /etc/rsyslog.d/sympa.conf    644 root root


ensure_service_started apache2 apache2

