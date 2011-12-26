#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

###########################################################
i () {
    PACKAGES="$PACKAGES $@"
}


###########################################################
# ~pstandard includes exim4-daemon-light, but we want exim4-daemon-heavy
#i "?or(~prequired,~pstandard,~pimportant)"

i resolvconf
#i unattended-upgrades

# make Vimmer happier:-)
i vim-nox vim-addon-manager vim-scripts

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

## dhcp
#i isc-dhcp-server

## dns
# unbound and dnsmasq are also very interesting:
#   http://en.wikipedia.org/wiki/Comparison_of_DNS_server_software#Feature_matrix
i bind9

## kerberos
i ntp
i pwgen krb5-admin-server krb5-kdc krb5-user krb5-clients kstart
i libpam-krb5

## OpenSSH
i openssh-server

## ldap
i slapd ldap-utils libsasl2-modules-gssapi-mit
i libpam-ccreds libnss-ldapd nscd nss-updatedb

# other nss cache schemes: nsscache sssd

i ldapscripts smbldap-tools shelldap ldapvi

# MTA: exim/postfix
i exim4 exim4-daemon-heavy

# IMAP: dovecot/cyrus/courier
i dovecot-imapd dovecot-lmtpd dovecot-gssapi
i dovecot-managesieved dovecot-sieve
i dovecot-ldap dovecot-solr
i dovecot-antispam #crm114

# antivirus
# XXX: http://web.nvd.nist.gov/view/vuln/search-results?query=clamav&search_type=all&cves=on
i clamav clamav-unofficial-sigs
#i amavisd-new

# antispam: http://www.maretmanu.org/homepage/inform/exim-spam.php#spam
i spamassassin

# Web mail: squirrelmail
i squirrelmail

# mailing list: sympa/mailman
i sympa

# SQL database, Sympa requires mysql | postgresql
i postgresql

# apache2
i apache2 apache2-mpm-worker

# php, Squirrelmail requires libapache2-mod-php5 | php5, the former
# requires libapache2-mpm-prefork, but I want more efficient worker
# mpm and all dynamic web contents running in seperate processes for
# maximum security, that's to say, FastCGI mode or separate WSGI/PSGI
# mode.
i php5 php5-fpm

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
    etckeeper vcs config color.ui auto


save_etc "save /etc before any config change"

