#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


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

cmp_dir $SCRIPT_DIR/etc/nagios3 /etc/nagios3 || {
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
[ -d /etc/nagiosgrapher/ngraph.d/templates ] || mkdir -m 0755 /etc/nagiosgrapher/ngraph.d/templates

######################################################################
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


[ -z "$NPCD_CONF_CHANGED" ] || service npcd restart
#[ -z "$PNP_GEARMAN_WORKER_CONF_CHANGED" ] || service pnp_gearman_worker restart
[ -z "$NG_CONF_CHANGED" ] || service nagiosgrapher restart
[ -z "$CONF_CHANGED" ] || service nagios3 restart
[ -z "$APACHE_CONF_CHANGED" ] || service apache2 restart


ensure_service_started npcd npcd
#[ "`pgrep pgrep process_perfdat`" ] || service pnp_gearman_worker start
[ "`pgrep nagiosgrapher`" ] || service nagiosgrapher start
ensure_service_started nagios3 nagios3
ensure_service_started apache2 apache2

