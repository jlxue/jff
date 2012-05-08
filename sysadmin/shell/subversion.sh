#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


mkdir -p -m 0755 /srv/svn
mkdir -p -m 0755 /srv/viewvc

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

cmp_dir $SCRIPT_DIR/etc/viewvc /etc/viewvc || {
    overwrite_dir $SCRIPT_DIR/etc/viewvc /etc/viewvc
    APACHE_CONF_CHANGED=1
}


ensure_mode_user_group /srv/svn                 750 svn svn
ensure_mode_user_group /srv/svn/authz           640 svn svn
ensure_mode_user_group /srv/svn/svnserve.conf   640 svn svn
ensure_mode_user_group /srv/svn/sasldb2         400 svn svn
ensure_mode_user_group /etc/sasl2               755 root root
ensure_mode_user_group /etc/sasl2/svn.conf      644 root root
ensure_mode_user_group /etc/default/svnserve    644 root root
ensure_mode_user_group /etc/init.d/svnserve     755 root root
ensure_mode_user_group /etc/logrotate.d/svnserve    644 root root
ensure_mode_user_group /etc/viewvc              755 root root
ensure_mode_user_group /srv/viewvc              750 viewvc viewvc


update-rc.d svnserve defaults

[ -z "$CONF_CHANGED" ] || service svnserve restart
[ -z "$APACHE_CONF_CHANGED" ] || service apache2 restart

ensure_service_started svnserve svnserve

