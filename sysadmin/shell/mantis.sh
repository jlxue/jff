#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

my_etckeeper () {
    local command="$1"

    shift
    etckeeper "$command" -d /srv/www/mantisbt "$@"
}


pkg=mantisbt-1.2.14
dir=/srv/www/mantisbt

[ -d $dir ] || {
    [ ! -e /srv/www/$pkg ] || mv /srv/www/$pkg /srv/www/$pkg-`date +%Y%m%d-%H%M%S`
    rm -f /tmp/$pkg.tar.gz
    wget -O /tmp/$pkg.tar.gz 'http://sourceforge.net/projects/mantisbt/files/mantis-stable/1.2.14/mantisbt-1.2.14.tar.gz/download'
    [ 71ab7c0a8d3697c9d094e104e80add21 = `md5sum /tmp/$pkg.tar.gz` ] &&
        [ 3ae652e721da519e706e072fd1b1209b0810c7b6 = `sha1sum /tmp/$pkg.tar.gz` ]

    tar --no-same-owner -C /srv/www -zxvf /tmp/$pkg.tar.gz || {
        /bin/rm -rf /srv/www/$pkg
        exit 1
    }

    mv /srv/www/$pkg $dir
    my_etckeeper init
    my_etckeeper commit "import $pkg"
    my_etckeeper vcs tag $pkg
}

! my_etckeeper unclean || my_etckeeper commit "save before configuring"
[ "`my_etckeeper vcs config --get color.ui`" = auto ] || my_etckeeper vcs config color.ui auto

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


fgrep -q 'if( BASIC_AUTH != $t_login_method && !auth_does_password_match( $t_user_id, $p_password ) ) {' \
        /srv/www/mantisbt/core/authentication_api.php || {
    patch -bst /srv/www/mantisbt/core/authentication_api.php \
        $SCRIPT_DIR/contrib/Mantis/authentication_api.php.patch
    CONF_CHANGED=1
}

ensure_mode_user_group /srv/www/mantisbt                750 root www-data
ensure_mode_user_group /srv/www/mantisbt/config_inc.php 640 root www-data
ensure_mode_user_group /srv/www/mantisbt/.git           700 root root
ensure_mode_user_group /srv/www/mantisbt/.gitignore     600 root root


[ -z "$CONF_CHANGED" ] || {
    service apache2 restart
    # config_inc.php is in .gitignore, so add it explictly
    my_etckeeper vcs add -f config_inc.php
}

ensure_service_started apache2 apache2

! my_etckeeper unclean || my_etckeeper commit "save after configuring"

