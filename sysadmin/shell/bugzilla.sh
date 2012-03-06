#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

my_etckeeper () {
    local command="$1"

    shift
    etckeeper "$command" -d /srv/www/bugzilla "$@"
}


pkg=bugzilla-4.2
dir=/srv/www/bugzilla

[ -d $dir ] || {
    [ ! -e /srv/www/$pkg ] || mv /srv/www/$pkg /srv/www/$pkg-`date +%Y%m%d-%H%M%S`
    rm -f /tmp/$pkg.tar.gz
    wget -O /tmp/$pkg.tar.gz 'http://ftp.mozilla.org/pub/mozilla.org/webtools/bugzilla-4.2.tar.gz'
    [ 7c712b26fbf7d8684f57c2e89caff422 = `md5sum /tmp/$pkg.tar.gz` ] &&
        [ 9ecde503712de41f90d84cdb5bf892841ad16840 = `sha1sum /tmp/$pkg.tar.gz` ]

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


{
    pwd=`pwd`
    cd $dir

    [ -e $dir/lib/PatchReader.pm ] || {
        /usr/bin/perl install-module.pl PatchReader
        new_module=1
    }

    [ -e $dir/lib/Email/MIME/Attachment/Stripper.pm ] || {
        /usr/bin/perl install-module.pl Email::MIME::Attachment::Stripper
        new_module=1
    }

    [ -e $dir/lib/Email/Reply.pm ] || {
        /usr/bin/perl install-module.pl Email::Reply
        new_module=1
    }

    [ -e $dir/lib/Daemon/Generic.pm ] || {
        /usr/bin/perl install-module.pl Daemon::Generic
        new_module=1
    }

    [ -e $dir/lib/Apache2/SizeLimit.pm ] || {
        /usr/bin/perl install-module.pl Apache2::SizeLimit
        new_module=1
    }

    [ -z "$new_module" ] || {
        my_etckeeper commit "save after installing modules"
    }

    [ -e $dir/lib/PatchReader.pm ] &&
        [ -e $dir/lib/Email/MIME/Attachment/Stripper.pm ] &&
        [ -e $dir/lib/Email/Reply.pm ] &&
        [ -e $dir/lib/Daemon/Generic.pm ] &&
        [ -e $dir/lib/Apache2/SizeLimit.pm ]

    cd "$pwd"
}


ensure_service_started postgresql postgres

pg_create_db_user   bugzilla
pg_create_db        bugzilla bugzilla


f=/srv/www/bugzilla/localconfig
tmpl=$SCRIPT_DIR$f
dummy1='@@DB_PASS@@'
dummy2='@@SITE_WIDE_SECRET@@'
isnew=
db_pass=
site_wide_secret=
set +x
parse_password_by_pattern "^\\s*\\\$db_pass\\s*=\\s*['\"]([^'\"]+)" $f $dummy1 db_pass isnew
[ ! "$isnew" ] || pg_set_role_password bugzilla "$db_pass"

isnew=
parse_password_by_pattern "^\\s*\\\$site_wide_secret\\s*=\\s*['\"]([^'\"]+)" $f $dummy2 site_wide_secret isnew 64

substitude_template "$tmpl" "$f" 640 root:www-data CONF_CHANGED -e "s/$dummy1/$db_pass/" -e "s/$dummy2/$site_wide_secret/"
set -x


[ -e /srv/www/bugzilla/data/params ] || {
    cp $SCRIPT_DIR/srv/www/bugzilla/data/params /srv/www/bugzilla/data/params
    CONF_CHANGED=1
}


ensure_mode_user_group /srv/www/bugzilla                750 root www-data
ensure_mode_user_group /srv/www/bugzilla/localconfig    640 root www-data
ensure_mode_user_group /srv/www/bugzilla/data/params    660 www-data www-data


[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

! my_etckeeper unclean || my_etckeeper commit "save after configuring"

sync_file $SCRIPT_DIR/etc/cron.d/bugzilla /etc/cron.d/bugzilla

