#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

[ -e /usr/local/bin/rb-site -a -e /usr/local/bin/rbssh ] ||
    easy_install ReviewBoard

ensure_service_started postgresql postgres


ensure_service_started apache2 apache2

