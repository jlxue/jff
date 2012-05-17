#!/bin/sh
#
# Suexec wrapper for gitolite-shell
#

export GIT_PROJECT_ROOT="/srv/git/repositories"
export GITOLITE_HTTP_HOME="/srv/git"
export GIT_HTTP_EXPORT_ALL=1

exec /usr/local/share/gitolite/gitolite-shell

