#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


/usr/bin/testparm -s -v $SCRIPT_DIR/etc/samba/smb.conf

cmp_file $SCRIPT_DIR/etc/default/samba /etc/default/samba || {
    overwrite_file $SCRIPT_DIR/etc/default/samba /etc/default/samba
    CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/samba /etc/samba --exclude smbpasswd || {
    overwrite_dir $SCRIPT_DIR/etc/samba /etc/samba --exclude smbpasswd
    CONF_CHANGED=1
}


ensure_mode_user_group /etc/default/samba           644 root root
ensure_mode_user_group /etc/samba                   755 root root
ensure_mode_user_group /etc/samba/gdbcommands       644 root root
ensure_mode_user_group /etc/samba/smb.conf          644 root root
ensure_mode_user_group /var/lib/samba/usershares    1770 root sambashare


[ -z "$CONF_CHANGED" ] || service samba restart

ensure_service_started samba smbd

