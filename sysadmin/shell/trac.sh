#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


[ "`getent group trac`" ] || addgroup --system trac

[ "`getent passwd trac`" ] || adduser --system --home /srv/trac \
    --shell /bin/false --ingroup trac --disabled-password \
    --disabled-login --gecos "Trac account" trac

id -G -n trac | grep -w -q svn || adduser trac svn


mkdir -p -m 0755 /srv/trac
mkdir -p -m 0755 /srv/www/trac

[ -e /srv/www/trac/cgi-bin ] && [ -e /srv/www/trac/htdocs ] || {
    trac-admin /tmp/trac-env-$$ initenv "My Project" "sqlite:db/trac.db"
    trac-admin /tmp/trac-env-$$ deploy /srv/www/trac
    rm -rf /tmp/trac-env-$$

    CONF_CHANGED=1
}


ensure_mode_user_group /srv/trac            750 trac trac
ensure_mode_user_group /srv/www/trac        755 root root


[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

