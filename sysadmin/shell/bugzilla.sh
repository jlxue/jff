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

(
    cd $dir
    [ -e $dir/lib/PatchReader.pm ] ||
        /usr/bin/perl install-module.pl PatchReader
    [ -e $dir/lib/Email/MIME/Attachment/Stripper.pm ] ||
        /usr/bin/perl install-module.pl Email::MIME::Attachment::Stripper
    [ -e $dir/lib/Email/Reply.pm ] ||
        /usr/bin/perl install-module.pl Email::Reply
    [ -e $dir/lib/Daemon/Generic.pm ] ||
        /usr/bin/perl install-module.pl Daemon::Generic
    [ -e $dir/lib/Apache2/SizeLimit.pm ] ||
        /usr/bin/perl install-module.pl Apache2::SizeLimit

    [ -e $dir/lib/PatchReader.pm ] &&
        [ -e $dir/lib/Email/MIME/Attachment/Stripper.pm ] &&
        [ -e $dir/lib/Email/Reply.pm ] &&
        [ -e $dir/lib/Daemon/Generic.pm ] &&
        [ -e $dir/lib/Apache2/SizeLimit.pm ]
)


ensure_service_started postgresql postgres


[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

! my_etckeeper unclean || my_etckeeper commit "save before configuring"

