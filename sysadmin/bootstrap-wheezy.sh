#!/bin/bash

set -e

if [ -z "$1" ] || [ -e "$1" -a ! -d "$1" ]; then
    echo "Usage: $0 path/to/directory" >&2
    exit 1
fi
ROOT="$1"; ROOT=${ROOT%/}
SCRIPT_DIR=`dirname "$0"`

[ -d $ROOT/bin ] || sudo multistrap -d "$ROOT" -f "$SCRIPT_DIR/multistrap-wheezy.conf"

do_mount () {
    mount | fgrep -q "$1" || return 0
    mount | fgrep -q "$ROOT$1" && return 0

    sudo mkdir -p "$ROOT$1"
    sudo mount -B "$1" "$ROOT$1"
}


do_mount /lib/init/rw
do_mount /proc
do_mount /sys
do_mount /dev
do_mount /dev/shm
do_mount /dev/pts
do_mount /var/lib/nfs/rpc_pipefs
do_mount /sys/fs/fuse/connections
do_mount /proc/sys/fs/binfmt_misc
do_mount /proc/fs/nfsd

[ ! -e $ROOT/etc/resolv.conf ] && sudo cp -a /etc/resolv.conf $ROOT/etc/

sudo chroot $ROOT /bin/sh -c "aptitude update; aptitude install '?or(~prequired,~pstandard,~pimportant)'"

