#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


cmp_dir $SCRIPT_DIR/etc/drupal/7 /etc/drupal/7 --exclude dbconfig.php || {
    overwrite_dir $SCRIPT_DIR/etc/drupal/7 /etc/drupal/7 --exclude dbconfig.php
    CONF_CHANGED=1
}


#mkdir -m 755 -p /usr/local/share/drupal/modules
#mkdir -m 755 -p /usr/local/share/drupal/themes
#
#cmp_dir $SCRIPT_DIR/contrib/Drupal/webserver_auth /usr/local/share/drupal/modules/webserver_auth || {
#    overwrite_dir $SCRIPT_DIR/contrib/Drupal/webserver_auth /usr/local/share/drupal/modules/webserver_auth
#    CONF_CHANGED=1
#}
#
#[ -e /usr/share/drupal7/modules/local ] || {
#    ln -s /usr/local/share/drupal/modules /usr/share/drupal7/modules/local
#    CONF_CHANGED=1
#}
#
#[ -e /usr/share/drupal7/themes/local ] || {
#    ln -s /usr/local/share/drupal/themes /usr/share/drupal7/themes/local
#    CONF_CHANGED=1
#}


######################################################################################
ensure_service_started postgresql postgres


f=/etc/dbconfig-common/drupal7.conf
tmpl=$SCRIPT_DIR$f
dummy='@@DRUPAL_DB_PASSWORD@@'
isnew=
db_passwd=
set +x
parse_password_by_pattern "dbc_dbpass\\s*=\\s*['\"]([^'\"]+)" $f $dummy db_passwd isnew
[ ! "$isnew" ] || pg_set_role_password drupal7 "$db_passwd"

substitude_template "$tmpl" "$f" 600 root:root CONF_CHANGED -e "s/$dummy/$db_passwd/"

f=/etc/drupal/7/sites/default/dbconfig.php
tmpl=$SCRIPT_DIR$f
dummy='@@DRUPAL_DB_PASSWORD@@'
substitude_template "$tmpl" "$f" 640 root:www-data CONF_CHANGED -e "s/$dummy/$db_passwd/"

set -x

######################################################################################

ensure_mode_user_group /etc/dbconfig-common                 755 root root
ensure_mode_user_group /etc/dbconfig-common/drupal7.conf    600 root root
ensure_mode_user_group /etc/drupal                          755 root root
ensure_mode_user_group /etc/drupal/7                        755 root root
ensure_mode_user_group /etc/drupal/7/sites                  755 root root
ensure_mode_user_group /etc/drupal/7/sites/default          755 root root
ensure_mode_user_group /etc/drupal/7/sites/default/dbconfig.php     640 root www-data
ensure_mode_user_group /etc/drupal/7/sites/default/settings.php     644 root root


[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

