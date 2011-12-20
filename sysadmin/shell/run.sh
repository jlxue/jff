#!/bin/sh

set -e -x

PATH=/bin:/sbin:/usr/bin:/usr/sbin

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

# fai.sh        fai-project.org
# mta.sh        exim + smarthost, spamassassin/bogofilter/bsfilter, clamav
# webmail.sh    squirrelmail, calendar
# maillist.sh   sympa/mailman   (forum?cms?)
# samba.sh
# im.sh         ejabberd/openfire
# wiki.sh       fosiwiki/moinmoin
# cms.sh        drupal
# blog.sh       wordpress
# svn.sh        subversion + http, viewvc, reviewboard, Rietveld
# git.sh        gitweb/cgit, gitolite, git-http-backend, gerrit
# trac.sh
# redmine.sh
# bts.sh        bugzilla/mantis
# ci.sh         jenkins
# backup.sh     bacula/amanda
# vpn.sh        openvpn
# grok.sh       opengrok
# search.sh     apache solr, katta.sourceforge.net
# monitor.sh    cacti, nagios, SEC(Simple Event Correlator), logcheck, fluentd, scribe
#               monit
# sugarCRM?, groupware?, zimbra?, asterisk?

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

