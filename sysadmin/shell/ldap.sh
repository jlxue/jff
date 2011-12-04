#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

# Reference:
#   http://techpubs.spinlocksolutions.com/dklar/ldap.html
#   Debian GNU: Setting up OpenLDAP
#   Davor Ocelic
#   SPINLOCK — advanced GNU/Linux and Unix solutions for commercial and education sectors.


sync_file $SCRIPT_DIR/etc/ldap/ldap.conf /etc/ldap/ldap.conf

ensure_mode_user_group /etc/ldap/ldap.conf  644 root root

