#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


######################################################################
# After you change these settings, remember to
#   * adjust etc/apache2/sites-available/monitor
#   * enable or disable pnp4nagios broker module in etc/nagios3/nagios.cfg
ENABLE_PNP4NAGIOS=no
ENABLE_ZABBIX=no
ENABLE_MUNIN=no

######################################################################
mkdir -p -m 0755 /srv/www/monitor

cmp_file $SCRIPT_DIR/etc/default/nagios3 /etc/default/nagios3 || {
    overwrite_file $SCRIPT_DIR/etc/default/nagios3 /etc/default/nagios3
    CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/nagios-plugins /etc/nagios-plugins || {
    overwrite_dir $SCRIPT_DIR/etc/nagios-plugins /etc/nagios-plugins
    CONF_CHANGED=1
}

# /etc/nagios3/conf.d/ngraph is a symlink to ../../nagiosgrapher/nagios3
# which contains generated files in serviceext directory.
[ "$ENABLE_PNP4NAGIOS" = yes ] && exclude_pnp4nagios= || exclude_pnp4nagios="--exclude pnp4nagios.cfg"
cmp_dir $SCRIPT_DIR/etc/nagios3 /etc/nagios3 --exclude serviceext $exclude_pnp4nagios || {
    overwrite_dir $SCRIPT_DIR/etc/nagios3 /etc/nagios3
    CONF_CHANGED=1
    APACHE_CONF_CHANGED=1
}

######################################################################

# Files in /etc/nagiosgrapher/nagios3/serviceext/ are generated.
cmp_dir $SCRIPT_DIR/etc/nagiosgrapher /etc/nagiosgrapher --exclude serviceext || {
    overwrite_dir $SCRIPT_DIR/etc/nagiosgrapher /etc/nagiosgrapher --exclude serviceext
    NG_CONF_CHANGED=1
    CONF_CHANGED=1
}

######################################################################
if [ "$ENABLE_PNP4NAGIOS" = yes ]; then

cmp_file $SCRIPT_DIR/etc/default/npcd /etc/default/npcd || {
    overwrite_file $SCRIPT_DIR/etc/default/npcd /etc/default/npcd
    NPCD_CONF_CHANGED=1
}

cmp_file $SCRIPT_DIR/etc/default/pnp_gearman_worker /etc/default/pnp_gearman_worker || {
    overwrite_file $SCRIPT_DIR/etc/default/pnp_gearman_worker /etc/default/pnp_gearman_worker
    PNP_GEARMAN_WORKER_CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/pnp4nagios /etc/pnp4nagios || {
    overwrite_dir $SCRIPT_DIR/etc/pnp4nagios /etc/pnp4nagios
    CONF_CHANGED=1
    NPCD_CONF_CHANGED=1
    PNP_GEARMAN_WORKER_CONF_CHANGED=1
    APACHE_CONF_CHANGED=1
}

fi

######################################################################
if [ "$ENABLE_MUNIN" = yes ]; then

cmp_dir $SCRIPT_DIR/etc/munin /etc/munin || {
    overwrite_dir $SCRIPT_DIR/etc/munin /etc/munin
    MUNIN_CONF_CHANGED=1
    APACHE_CONF_CHANGED=1
}
[ -d /etc/munin/munin-conf.d ] || mkdir -m 0755 /etc/munin/munin-conf.d

fi

######################################################################
cmp_dir $SCRIPT_DIR/etc/ganglia /etc/ganglia || {
    overwrite_dir $SCRIPT_DIR/etc/ganglia /etc/ganglia
    GANGLIA_CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/ganglia-webfrontend /etc/ganglia-webfrontend || {
    overwrite_dir $SCRIPT_DIR/etc/ganglia-webfrontend /etc/ganglia-webfrontend
    APACHE_CONF_CHANGED=1
}

######################################################################
if [ "$ENABLE_ZABBIX" = yes ]; then

ensure_service_started postgresql postgres

cmp_file $SCRIPT_DIR/etc/default/snmpd /etc/default/snmpd || {
    overwrite_file $SCRIPT_DIR/etc/default/snmpd /etc/default/snmpd
    SNMPD_CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/snmp /etc/snmp || {
    overwrite_dir $SCRIPT_DIR/etc/snmp /etc/snmp
    SNMPD_CONF_CHANGED=1
}

cmp_dir $SCRIPT_DIR/etc/zabbix /etc/zabbix --exclude dbconfig.php --exclude zabbix_server.conf || {
    overwrite_dir $SCRIPT_DIR/etc/zabbix /etc/zabbix --exclude dbconfig.php --exclude zabbix_server.conf
    ZABBIX_CONF_CHANGED=1
    APACHE_CONF_CHANGED=1
}

f=/etc/dbconfig-common/zabbix-server-pgsql.conf
tmpl=$SCRIPT_DIR$f
dummy='@@ZABBIX_DB_PASSWORD@@'
isnew=
set +x
parse_password_by_pattern "dbc_dbpass\\s*=\\s*['\"]([^'\"]+)" $f $dummy db_passwd isnew
[ ! "$isnew" ] || pg_set_role_password zabbix "$db_passwd"

substitude_template "$tmpl" "$f" 600 root:root ZABBIX_CONF_CHANGED -e "s/$dummy/$db_passwd/"

f=/etc/dbconfig-common/zabbix-frontend-php.conf
tmpl=$SCRIPT_DIR$f
substitude_template "$tmpl" "$f" 600 root:root ZABBIX_CONF_CHANGED -e "s/$dummy/$db_passwd/"

f=/etc/zabbix/dbconfig.php
tmpl=$SCRIPT_DIR$f
substitude_template "$tmpl" "$f" 640 root:www-data ZABBIX_CONF_CHANGED -e "s/$dummy/$db_passwd/"

f=/etc/zabbix/zabbix_server.conf
tmpl=$SCRIPT_DIR$f
substitude_template "$tmpl" "$f" 640 root:root ZABBIX_CONF_CHANGED -e "s/$dummy/$db_passwd/"
set -x

fi

######################################################################

ensure_mode_user_group /etc/default/nagios3         644 root root
ensure_mode_user_group /etc/nagios-plugins          755 root root
ensure_mode_user_group /etc/nagios3                 755 root root
ensure_mode_user_group /etc/nagios3/resource.cfg    640 root nagios
ensure_mode_user_group /etc/nagiosgrapher           755 root root

ensure_mode_user_group /var/cache/nagios3           2750 nagios www-data
ensure_mode_user_group /var/log/nagios3             2751 nagios adm
ensure_mode_user_group /var/lib/nagios              755 nagios nagios
ensure_mode_user_group /var/lib/nagios3             750 nagios nagios

ensure_mode_user_group /etc/munin                   755 root root
ensure_mode_user_group /etc/munin/plugin-conf.d     750 root munin

ensure_mode_user_group /etc/ganglia                 755 root root
ensure_mode_user_group /etc/ganglia-webfrontend     755 root root


ensure_mode_user_group /etc/dbconfig-common/zabbix-frontend-php.conf    600 root root
ensure_mode_user_group /etc/dbconfig-common/zabbix-server-pgsql.conf    600 root root
ensure_mode_user_group /etc/zabbix                  755 root root
ensure_mode_user_group /etc/zabbix/alert.d          755 root root
ensure_mode_user_group /etc/zabbix/apache.conf      644 root root
ensure_mode_user_group /etc/zabbix/dbconfig.php     640 root www-data
ensure_mode_user_group /etc/zabbix/zabbix_agentd.conf   644 root root
ensure_mode_user_group /etc/zabbix/zabbix_agentd.conf.d 755 root root
ensure_mode_user_group /etc/zabbix/zabbix_server.conf   640 root root

ensure_mode_user_group /etc/default/snmpd           644 root root
ensure_mode_user_group /etc/snmp                    755 root root
ensure_mode_user_group /etc/snmp/snmp.conf          644 root root
ensure_mode_user_group /etc/snmp/snmpd.conf         600 root root
ensure_mode_user_group /etc/snmp/snmptrapd.conf     600 root root


######################################################################

if [ "$ENABLE_ZABBIX" = yes ]; then
    [ -z "$SNMPD_CONF_CHANGED" ] || service snmpd restart
    [ -z "$ZABBIX_CONF_CHANGED" ] || {
        service zabbix-server restart
        service zabbix-agent restart
    }
fi

[ -z "$GANGLIA_CONF_CHANGED" ] || {
    service ganglia-monitor restart
    service gmetad restart
}

if [ "$ENABLE_MUNIN" = yes ]; then
    [ -z "$MUNIN_CONF_CHANGED" ] || {
        service munin restart
        service munin-async restart
        service munin-node restart
    }
fi

if [ "$ENABLE_PNP4NAGIOS" = yes ]; then
    [ -z "$NPCD_CONF_CHANGED" ] || service npcd restart
    #[ -z "$PNP_GEARMAN_WORKER_CONF_CHANGED" ] || service pnp_gearman_worker restart
fi

[ -z "$NG_CONF_CHANGED" ] || service nagiosgrapher restart
[ -z "$CONF_CHANGED" ] || service nagios3 restart

[ -z "$APACHE_CONF_CHANGED" ] || service apache2 restart


######################################################################

if [ "$ENABLE_ZABBIX" = yes ]; then
    ensure_service_started snmpd snmpd
    ensure_service_started zabbix-server zabbix_server
    ensure_service_started zabbix-agent zabbix_agentd
fi

ensure_service_started ganglia-monitor gmond
ensure_service_started gmetad gmetad

if [ "$ENABLE_MUNIN" = yes ]; then
    ensure_service_started munin-async munin-async-server
    ensure_service_started munin-node munin-node
fi

if [ "$ENABLE_PNP4NAGIOS" = yes ]; then
    ensure_service_started npcd npcd
    #[ "`pgrep pgrep process_perfdat`" ] || service pnp_gearman_worker start
fi

[ "`pgrep nagiosgrapher`" ] || service nagiosgrapher start
ensure_service_started nagios3 nagios3
ensure_service_started apache2 apache2
ensure_service_started cron cron

