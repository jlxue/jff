#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

my_etckeeper () {
    etckeeper "$@" -d /srv/www/mantisbt
}


pkg=mantisbt-1.2.8

[ -d /srv/www/mantisbt ] || {
    [ ! -e /srv/www/$pkg ] || mv /srv/www/$pkg /srv/www/$pkg-`date +%Y%m%d-%H%M%S`
    rm -f /tmp/$pkg.tar.gz
    wget -O /tmp/$pkg.tar.gz 'http://sourceforge.net/projects/mantisbt/files/mantis-stable/1.2.8/mantisbt-1.2.8.tar.gz/download'
    [ 054035ba0ebfc8997e10e2bc75d39483 = `md5sum /tmp/$pkg.tar.gz` ] &&
        [ 6cff6fd7d709e25c620c9717d6bf079ce52b73c5 = `sha1sum /tmp/$pkg.tar.gz` ]

    tar --no-same-owner -C /srv/www -zxvf /tmp/$pkg.tar.gz || {
        /bin/rm -rf /srv/www/$pkg
        exit 1
    }

    mv /srv/www/$pkg /srv/www/mantisbt
    my_etckeeper init
    my_etckeeper "import $pkg"
}

! my_etckeeper unclean || my_etckeeper commit "save before configuring"

ensure_service_started postgresql postgres

pg_create_db_user   mantisbt
pg_create_db        mantisbt mantisbt


f=/srv/www/mantisbt/config_inc.php
tmpl=$SCRIPT_DIR$f
dummy='@@MANTISBT_PASSWORD@@'
isnew=
mantisbt_passwd=
set +x
parse_password_by_pattern "^\\s*\\\$g_db_password\\s*=\\s*['\"]([^'\"]+)" $f $dummy mantisbt_passwd isnew
[ ! "$isnew" ] || pg_set_role_password mantisbt "$mantisbt_passwd"

substitude_template "$tmpl" "$f" 640 root:www-data CONF_CHANGED -e "s/$dummy/$mantisbt_passwd/"
set -x


ensure_mode_user_group /srv/www/mantisbt                755 root root
ensure_mode_user_group /srv/www/mantisbt/config_inc.php 640 root www-data

[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

! my_etckeeper unclean || my_etckeeper commit "save before configuring"
