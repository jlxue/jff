#!/bin/sh

set -e -x

PATH=/bin:/sbin:/usr/bin:/usr/sbin
export BACKUP_DIR=${BACKUP_DIR:-/root/cfg-bak/`date +%Y%m%d-%H%M%S`}

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

# fai.sh        fai-project.org
# samba.sh
# im.sh         ejabberd/openfire
# wiki.sh       foswiki/moinmoin
# cms.sh        drupal
# blog.sh       wordpress
# svn.sh        subversion + http, viewvc, reviewboard, Rietveld
# git.sh        gitweb/cgit, gitolite, git-http-backend, gerrit
# trac.sh
# redmine.sh
# ci.sh         jenkins
# backup.sh     bacula/amanda/burp/backuppc/obnam
# vpn.sh        openvpn
# grok.sh       opengrok
# search.sh     apache solr, katta.sourceforge.net
# monitor.sh    cacti, nagios, munin, SEC(Simple Event Correlator), logcheck, fluentd, scribe
#               monit
# sugarCRM?, groupware?, zimbra?, asterisk?
# web conference:
#   http://incubator.apache.org/openmeetings/
#   http://code.google.com/p/bigbluebutton/
#   https://www.webhuddle.com/
#   http://ekiga.org
#   http://moodle.org

SCRIPTS=`cat<<END
install-packages.sh
locale.sh
firewall.sh
dns.sh
dhcp.sh
ntp.sh
kerberos.sh
ssh.sh
ldap.sh
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

