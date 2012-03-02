#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

pkg=mantisbt-1.2.8

[ -d /srv/www/mantisbt ] || (
    set -e
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
)

ensure_mode_user_group /srv/www/mantisbt    755 root root

[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

