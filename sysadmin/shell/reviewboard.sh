#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


[ -e /usr/local/bin/rb-site -a -e /usr/local/bin/rbssh ] ||
    easy_install --no-user-cfg ReviewBoard

ensure_service_started postgresql postgres

pg_create_db_user   reviewboard
pg_create_db        reviewboard reviewboard

[ -e /srv/www/ReviewBoard ] || {
    set +x

    db_password=`pwgen -cnys 24 1`
    admin_password=`pwgen -cnys 24 1`
    pg_set_role_password reviewboard "$db_password"

    /usr/local/bin/rb-site install --noinput \
        --domain-name=codereview.corp.example.com \
        --site-root=/ --media-url=media/ \
        --db-type=postgresql --db-name=reviewboard \
        --db-host=localhost --db-user=reviewboard \
        --db-pass="$db_password" --cache-type=memcached \
        --cache-info=memcached://localhost:11211/ \
        --web-server-type=apache --python-loader=wsgi \
        --admin-user=dieken --admin-password="$admin_password" \
        --admin-email=dieken@corp.example.com \
        /srv/www/ReviewBoard

    set -x

    mkdir -m 0755 -p /srv/www/ReviewBoard/search-index
    mkdir -m 0755 -p /srv/www/ReviewBoard/htdocs/media/uploaded/images
    chown -R reviewboard:reviewboard /srv/www/ReviewBoard/htdocs/media/uploaded
    chown -R reviewboard:reviewboard /srv/www/ReviewBoard/data
}


ensure_mode_user_group /srv/www/ReviewBoard         755 root root
ensure_mode_user_group /srv/www/ReviewBoard/conf    755 root root
ensure_mode_user_group /srv/www/ReviewBoard/htdocs  755 root root
ensure_mode_user_group /srv/www/ReviewBoard/logs    755 root root
ensure_mode_user_group /srv/www/ReviewBoard/tmp     755 root root

ensure_mode_user_group /srv/www/ReviewBoard/conf/settings_local.py  750 reviewboard reviewboard
ensure_mode_user_group /srv/www/ReviewBoard/conf/settings_local.pyc 750 reviewboard reviewboard
ensure_mode_user_group /srv/www/ReviewBoard/data                    750 reviewboard reviewboard
ensure_mode_user_group /srv/www/ReviewBoard/htdocs/media/uploaded   750 reviewboard reviewboard
ensure_mode_user_group /srv/www/ReviewBoard/search-index            750 reviewboard reviewboard


ensure_service_started memcached memcached
ensure_service_started apache2 apache2

