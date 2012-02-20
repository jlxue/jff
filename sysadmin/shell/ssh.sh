#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


cmp_file $SCRIPT_DIR/etc/ssh/sshd_config /etc/ssh/sshd_config || {
    overwrite_file $SCRIPT_DIR/etc/ssh/sshd_config /etc/ssh/sshd_config
    service ssh restart
}

ensure_mode_user_group /etc/ssh                 755 root root
ensure_mode_user_group /etc/ssh/sshd_config     644 root root

ensure_service_started ssh sshd

