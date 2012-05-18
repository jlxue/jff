#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


[ "x/bin/bash" = x$(perl -le 'print ((getpwnam("git"))[8])') ] ||
    chsh -s /bin/bash git


mkdir -m 755 -p /usr/local/share /usr/local/bin
[ -e /usr/local/share/gitolite ] || overwrite_dir $SCRIPT_DIR/usr/local/share/gitolite /usr/local/share/gitolite
[ -e /usr/local/bin/gitolite ] || ln -s /usr/local/share/gitolite/gitolite /usr/local/bin

mkdir -m 755 -p /srv/www/git
sync_file $SCRIPT_DIR/srv/www/git/gitolite-shell-wrapper /srv/www/git/gitolite-shell-wrapper

sync_file $SCRIPT_DIR/srv/git/gitolite.rc /srv/git/.gitolite.rc


ensure_mode_user_group /srv/git                 750 git git
ensure_mode_user_group /srv/git/.gitolite       755 git git
ensure_mode_user_group /srv/git/.gitolite.rc    644 git git
ensure_mode_user_group /srv/git/projects.list   644 git git
ensure_mode_user_group /srv/git/repositories    755 git git
ensure_mode_user_group /srv/git/.ssh            700 git git


# SuExec feature demands this file to be owned by "git" owner and "git"
# group, and "www-data" user need check status info of gitolite-shell-wrapper,
# so this directory must be readable for other users.
ensure_mode_user_group /srv/www/git     755 git git
ensure_mode_user_group /srv/www/git/gitolite-shell-wrapper  700 git git


ensure_service_started apache2 apache2
ensure_service_started ssh sshd

