#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


#########################################################################################
ensure_service_started postgresql postgres

f=/etc/dbconfig-common/redmine/instances/default.conf
tmpl=$SCRIPT_DIR$f
dummy='@@REDIMINE_DB_PASSWORD@@'
isnew=
set +x
parse_password_by_pattern "dbc_dbpass\\s*=\\s*['\"]([^'\"]+)" $f $dummy db_passwd isnew
[ ! "$isnew" ] || pg_set_role_password redmine "$db_passwd"

substitude_template "$tmpl" "$f" 600 root:root CONF_CHANGED -e "s/$dummy/$db_passwd/"

f=/etc/redmine/default/database.yml
tmpl=$SCRIPT_DIR$f
dummy='@@REDIMINE_DB_PASSWORD@@'
substitude_template "$tmpl" "$f" 640 root:www-data CONF_CHANGED -e "s/$dummy/$db_passwd/"
set -x
#########################################################################################


# See /usr/share/doc/redmine/examples/apache2-passenger-host.conf
[ -e /var/lib/redmine/default/passenger ] || ln -s /usr/share/redmine /var/lib/redmine/default/passenger


cmp_dir $SCRIPT_DIR/etc/redmine /etc/redmine --exclude database.yml --exclude session.yml || {
    overwrite_dir $SCRIPT_DIR/etc/redmine /etc/redmine --exclude database.yml --exclude session.yml
    CONF_CHANGED=1
}

# Custom authentication
#   http://www.redmine.org/issues/1131
#   https://github.com/AdamLantos/redmine_http_auth
#   https://github.com/edavis10/redmine_sso_client
#   http://www.redmine.org/projects/redmine/wiki/Plugin_Tutorial
#
#   http://www.redmine.org/projects/redmine/wiki/Alternativecustom_authentication_HowTo
cmp_file $SCRIPT_DIR/usr/share/redmine/app/models/auth_source_http.rb /usr/share/redmine/app/models/auth_source_http.rb || {
    overwrite_file $SCRIPT_DIR/usr/share/redmine/app/models/auth_source_http.rb /usr/share/redmine/app/models/auth_source_http.rb
    CONF_CHANGED=1
}

grep -wq 'REMOTE_USER' /usr/share/redmine/app/controllers/application_controller.rb || {
    patch -bst /usr/share/redmine/app/controllers/application_controller.rb \
        $SCRIPT_DIR/contrib/Redmine/application_controller.rb.patch
    CONF_CHANGED=1
}

echo 'select * from auth_sources;' | su postgres -c 'psql -w -X -1 -f - redmine_default' |
    grep -w -q 'AuthSourceHTTP' || {
        echo "INSERT INTO auth_sources VALUES (DEFAULT, 'AuthSourceHTTP', 'HTTP', NULL, NULL,  NULL, NULL, NULL, 'name', 'firstName', 'lastName', 'email', TRUE, FALSE)" |
            su postgres -c 'psql -w -X -1 -f - redmine_default'
        CONF_CHANGED=1
}


ensure_mode_user_group /etc/dbconfig-common                 755 root root
ensure_mode_user_group /etc/dbconfig-common/redmine         755 root root
ensure_mode_user_group /etc/dbconfig-common/redmine/instances   755 root root
ensure_mode_user_group /etc/dbconfig-common/redmine/instances/default.conf  600 root root

ensure_mode_user_group /etc/redmine                         755 root root
ensure_mode_user_group /etc/redmine/default                 755 www-data www-data
ensure_mode_user_group /etc/redmine/default/database.yml    640 root www-data
ensure_mode_user_group /etc/redmine/default/session.yml     640 root www-data
ensure_mode_user_group /etc/redmine/default/configuration.yml   640 root www-data

ensure_mode_user_group /var/log/redmine             755 www-data www-data
ensure_mode_user_group /var/log/redmine/default     750 www-data www-data
ensure_mode_user_group /var/lib/redmine             755 www-data www-data
ensure_mode_user_group /var/lib/redmine/default     750 www-data www-data
ensure_mode_user_group /var/cache/redmine           755 www-data www-data
ensure_mode_user_group /var/cache/redmine/default   750 www-data www-data

ensure_mode_user_group /usr/share/redmine/app/models/auth_source_http.rb    644 root root


[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

