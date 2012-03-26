#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


mkdir -p -m 0755 /srv/trac
mkdir -p -m 0755 /srv/www/trac

[ -e /srv/www/trac/cgi-bin ] && [ -e /srv/www/trac/htdocs ] || {
    trac-admin /tmp/trac-env-$$ initenv "My Project" "sqlite:db/trac.db"
    trac-admin /tmp/trac-env-$$ deploy /srv/www/trac
    rm -rf /tmp/trac-env-$$

    CONF_CHANGED=1
}


ensure_mode_user_group /srv/trac            750 trac trac

# Both www-data and trac users requires read permisson on it.
ensure_mode_user_group /srv/www/trac        755 root trac


[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

