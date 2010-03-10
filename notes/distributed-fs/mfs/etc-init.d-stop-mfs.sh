#!/bin/sh

# Purpose:
#	stop mfs servers when shutdown or reboot
#
# Usage:
#       for d in /etc/rc0.d /etc/rc1.d; do
#		( cd $d; ln -s ../init.d/stop-mfs.sh K01stop-mfs.sh )
#	done

su yubaoliu -c "/opt/mfs/mfs71.sh stop >/opt/mfs/stop-mfs.log 2>&1"

