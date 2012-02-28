#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

#### modified from /usr/share/davical/dba/create-database.sh
db_users() {
  su postgres 'psql -qXAt -c "SELECT usename FROM pg_user;" template1'
}

create_db_user() {
  if ! db_users | grep "^${1}$" >/dev/null ; then
    su postgres "psql -qXAt -c 'CREATE USER ${1} NOCREATEDB NOCREATEROLE;'" template1
  fi
}

#############################################################

ensure_service_started postgresql postgres

create_db_user davical_dba
create_db_user davical_app

set +x
davical_dba_passwd=$(capture_match
set -x

cmp_dir $SCRIPT_DIR/etc/davical /etc/davical --exclude administration.yml --exclude config.php || {
    overwrite_dir $SCRIPT_DIR/etc/davical /etc/davical --exclude administration.yml --exclude config.php
    CONF_CHANGED=1
}

ensure_mode_user_group /etc/davical/administration.yml      640 root www-data
ensure_mode_user_group /etc/davical/config.php              640 root www-data

[ -z "$CONF_CHANGED" ] || service apache2 restart

#############################################################
#### must create db users and copy configuration files first, see above
su postgres -c 'psql -c "" davical' 2>/dev/null || {
    su postgres -c /usr/share/davical/dba/create-database.sh
}

