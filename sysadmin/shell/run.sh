#!/bin/sh

set -e -x

PATH=/bin:/sbin:/usr/bin:/usr/sbin
export BACKUP_DIR=${BACKUP_DIR:-/root/cfg-bak/`date +%Y%m%d-%H%M%S`}

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

# fai.sh        fai-project.org
# ci.sh         jenkins
# backup.sh     bacula/amanda/backuppc
# vpn.sh        openvpn
# grok.sh       opengrok
# search.sh     apache solr, katta.sourceforge.net
# monitor.sh    cacti, nagios, munin, SEC(Simple Event Correlator), logcheck, fluentd, scribe
#               monit
# sugarCRM?, groupware?, zimbra?, asterisk?


# postgresql installation script requires correct locale setting,
# so "locale.sh" is the first.
SCRIPTS=`cat<<END
locale.sh
install-packages.sh
firewall.sh
dns.sh
dhcp.sh
ntp.sh
kerberos.sh
ldap.sh
ssh.sh
imap.sh
smtp.sh
apache.sh
mailman.sh
roundcube.sh
calendar.sh
davical.sh
mantis.sh
bugzilla.sh
foswiki.sh
moinmoin.sh
ejabberd.sh
samba.sh
subversion.sh
trac.sh
redmine.sh
drupal.sh
monitoring.sh
reviewboard.sh
gitolite.sh
gerrit.sh
END`

for f in $SCRIPTS; do
    f=$SCRIPT_DIR/$f
    if [ -f $f ]; then
        echo ">>>>>>>>>>>>> $f"
        $f && echo "<<<<<<<<<<<<< $f" || {
            echo "ERROR: failed to run $f" >&2
            exit 1
        }
    fi
done

save_etc "save /etc after some config change"

