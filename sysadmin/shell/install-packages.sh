#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

###########################################################
i () {
    PACKAGES="$PACKAGES $@"
}


###########################################################
i "?or(~prequired,~pstandard,~pimportant)"

## manage /etc under git
i git etckeeper

## security
i libpam-cracklib
i auditd acct
i debsums aide samhain
i chkrootkit rkhunter   # loop in unhide.rb
i fail2ban snort

## firewall
i shorewall-init shorewall shorewall6

## kerberos
i ntp
i pwgen krb5-admin-server krb5-kdc krb5-user krb5-clients
i libpam-krb5

## ldap
i slapd ldap-utils libsasl2-modules-gssapi-mit
i libpam-ccreds libnss-ldapd nscd

###########################################################
APT_LISTBUGS_FRONTEND=none
APT_LISTCHANGES_FRONTEND=none
DEBIAN_FRONTEND=noninteractive
export APT_LISTBUGS_FRONTEND APT_LISTCHANGES_FRONTEND DEBIAN_FRONTEND

# See debconf(7) in package debconf-doc
# The file must be absolute file path!
DEBCONF_DB_FALLBACK="File{filename:$SCRIPT_DIR/debconf-db.fallback}"
export DEBCONF_DB_FALLBACK


aptitude -q update

aptitude -q safe-upgrade -o Dpkg::Options::=--force-confold \
    -o Dpkg::Options::=--force-confdef --assume-yes

aptitude -q install -o Dpkg::Options::=--force-confold \
    -o Dpkg::Options::=--force-confdef --assume-yes -s $PACKAGES |
        grep -q 'No packages will be installed' || {

    aptitude -q install -o Dpkg::Options::=--force-confold \
        -o Dpkg::Options::=--force-confdef --assume-yes $PACKAGES
}


###########################################################
[ "`etckeeper vcs config --get color.ui`" = auto ] ||
    etckeeper vcs config --local color.ui auto


save_etc "save /etc before any config change"

