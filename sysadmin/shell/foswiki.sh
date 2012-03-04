#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

my_etckeeper () {
    local command="$1"

    shift
    etckeeper "$command" -d /srv/www/foswiki "$@"
}


pkg=Foswiki-1.1.4
dir=/srv/www/foswiki

[ -d $dir ] || {
    [ ! -e /srv/www/$pkg ] || mv /srv/www/$pkg /srv/www/$pkg-`date +%Y%m%d-%H%M%S`
    rm -f /tmp/$pkg.tgz
    wget -O /tmp/$pkg.tgz 'http://sourceforge.net/projects/foswiki/files/foswiki/1.1.4/Foswiki-1.1.4.tgz'
    [ 19d6f20afd06d8eb321f583f417176df = `md5sum /tmp/$pkg.tgz` ] &&
        [ 7ed7c2bf2777a59d1a09c68747b72a2c52499276 = `sha1sum /tmp/$pkg.tgz` ]

    tar --no-same-owner -C /srv/www -zxvf /tmp/$pkg.tgz || {
        /bin/rm -rf /srv/www/$pkg
        exit 1
    }

    mv /srv/www/$pkg $dir
    chown -R www-data:www-data $dir

    my_etckeeper init
    my_etckeeper commit "import $pkg"
    my_etckeeper vcs tag $pkg
}

! my_etckeeper unclean || my_etckeeper commit "save before configuring"
[ "`my_etckeeper vcs config --get color.ui`" = auto ] || my_etckeeper vcs config color.ui auto


# http://foswiki.org/System/InstallationGuide

# lack Encode::compat, Lingua::EN::Sentence

ensure_mode_user_group /srv/www/foswiki             750 www-data www-data

[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

! my_etckeeper unclean || my_etckeeper commit "save after configuring"

