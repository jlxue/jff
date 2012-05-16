#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


add_system_user_group "git repository hosting" /srv/git git git
[ "x/bin/bash" = x$(perl -le 'print ((getpwnam("git"))[8])') ] ||
    chsh -s /bin/bash git


ensure_mode_user_group /srv/git     700 git git


ensure_service_started apache2 apache2
ensure_service_started ssh sshd

