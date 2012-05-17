#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


add_system_user_group "git repository hosting" /srv/git git git
[ "x/bin/bash" = x$(perl -le 'print ((getpwnam("git"))[8])') ] ||
    chsh -s /bin/bash git


mkdir -m 755 -p /usr/local/share /usr/local/bin
[ -e /usr/local/share/gitolite ] || overwrite_dir $SCRIPT_DIR/usr/local/share/gitolite /usr/local/share/gitolite
[ -e /usr/local/bin/gitolite ] || ln -s /usr/local/share/gitolite/gitolite /usr/local/bin

mkdir -m 755 -p /srv/www/git
sync_file $SCRIPT_DIR/srv/www/git/gitolite-shell-wrapper.sh /srv/www/git/gitolite-shell-wrapper.sh


ensure_mode_user_group /srv/git         700 git git
ensure_mode_user_group /srv/www/git     700 git git
ensure_mode_user_group /srv/www/git/gitolite-shell-wrapper.sh   700 git git


ensure_service_started apache2 apache2
ensure_service_started ssh sshd

