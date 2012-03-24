#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


[ "`getent group svn`" ] || addgroup --system svn

[ "`getent passwd svn`" ] || adduser --system --home /srv/svn \
    --shell /bin/false --ingroup svn --disabled-password \
    --disabled-login --gecos "svnserver server account" svn


mkdir -p -m 0755 /srv/svn

sync_file $SCRIPT_DIR/etc/logrotate.d/svnserve /etc/logrotate.d/svnserve

cmp_file $SCRIPT_DIR/etc/default/svnserve /etc/default/svnserve || {
    overwrite_file $SCRIPT_DIR/etc/default/svnserve /etc/default/svnserve
    CONF_CHANGED=1
}

cmp_file $SCRIPT_DIR/etc/init.d/svnserve /etc/init.d/svnserve || {
    overwrite_file $SCRIPT_DIR/etc/init.d/svnserve /etc/init.d/svnserve
    CONF_CHANGED=1
}

cmp_file $SCRIPT_DIR/srv/svn/svnserve.conf /srv/svn/svnserve.conf || {
    overwrite_file $SCRIPT_DIR/srv/svn/svnserve.conf /srv/svn/svnserve.conf
    CONF_CHANGED=1
}

cmp_file $SCRIPT_DIR/srv/svn/authz /srv/svn/authz || {
    overwrite_file $SCRIPT_DIR/srv/svn/authz /srv/svn/authz
    CONF_CHANGED=1
}

cmp_file $SCRIPT_DIR/etc/sasl2/svn.conf /etc/sasl2/svn.conf || {
    overwrite_file $SCRIPT_DIR/etc/sasl2/svn.conf /etc/sasl2/svn.conf
    CONF_CHANGED=1
}

ensure_mode_user_group /srv/svn                 750 svn svn
ensure_mode_user_group /srv/svn/authz           640 svn svn
ensure_mode_user_group /srv/svn/svnserve.conf   640 svn svn
ensure_mode_user_group /etc/sasl2               755 root root
ensure_mode_user_group /etc/sasl2/svn.conf      644 root root
ensure_mode_user_group /etc/default/svnserve    644 root root
ensure_mode_user_group /etc/init.d/svnserve     755 root root
ensure_mode_user_group /etc/logrotate.d/svnserve    644 root root


update-rc.d svnserve defaults

[ -z "$CONF_CHANGED" ] || service svnserve restart

ensure_service_started svnserve svnserve

