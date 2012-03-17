#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


cmp_file $SCRIPT_DIR/etc/default/ejabberd /etc/default/ejabberd || {
    overwrite_file $SCRIPT_DIR/etc/default/ejabberd /etc/default/ejabberd
    CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/ejabberd /etc/ejabberd --exclude ejabberd.pem || {
    overwrite_dir $SCRIPT_DIR/etc/ejabberd /etc/ejabberd --exclude ejabberd.pem
    CONF_CHANGED=1
}

ensure_mode_user_group /etc/default/ejabberd        644 root root
ensure_mode_user_group /etc/ejabberd                750 root ejabberd
ensure_mode_user_group /etc/ejabberd/ejabberd.cfg   600 ejabberd ejabberd
ensure_mode_user_group /etc/ejabberd/ejabberd.pem   640 root ejabberd
ensure_mode_user_group /etc/ejabberd/inetrc         644 root root
ensure_mode_user_group /var/log/ejabberd            2750 ejabberd adm
ensure_mode_user_group /var/lib/ejabberd            700 ejabberd ejabberd


[ -z "$CONF_CHANGED" ] || service ejabberd restart
[ "`pgrep -f ejabberd`" ] || service ejabberd start

