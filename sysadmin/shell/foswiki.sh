#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


ensure_mode_user_group /etc/mantis/apache.conf                   644 root root
ensure_mode_user_group /etc/mantis/config_inc.php                660 www-data root
ensure_mode_user_group /etc/mantis/config_local.php              660 www-data root
ensure_mode_user_group /etc/mantis/custom_constants_inc.php      660 www-data root
ensure_mode_user_group /etc/mantis/custom_functions_inc.php      660 www-data root
ensure_mode_user_group /etc/mantis/custom_relationships_inc.php  660 www-data root
ensure_mode_user_group /etc/mantis/custom_strings_inc.php        660 www-data root
ensure_mode_user_group /etc/mantis/htacess.dat                   640 root www-data


[ -z "$CONF_CHANGED" ] || service apache2 restart

ensure_service_started apache2 apache2

