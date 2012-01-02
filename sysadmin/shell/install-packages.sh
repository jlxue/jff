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


# antivirus
# XXX: http://web.nvd.nist.gov/view/vuln/search-results?query=clamav&search_type=all&cves=on
i clamav clamav-unofficial-sigs
#i amavisd-new

# antispam: http://www.maretmanu.org/homepage/inform/exim-spam.php#spam
i spamassassin

# MTA: exim/postfix
i exim4 exim4-daemon-heavy

# IMAP: dovecot/cyrus/courier
i dovecot-imapd dovecot-lmtpd dovecot-gssapi
i dovecot-managesieved dovecot-sieve
i dovecot-ldap dovecot-solr
i dovecot-antispam #crm114


# apache2
# Web SSO: http://webauth.stanford.edu/features.html
i apache2 apache2-mpm-worker
i ssl-cert  # See /usr/share/doc/apache2.2-common/README.Debian.gz

    # GSSAPI SPNEGO
    i libapache2-mod-auth-kerb

    # Michigen Cosign: http://weblogin.org/, no Debian package yet

    # Pubcookie: http://pubcookie.org/

    # Standford WebAuth: doesn't support site-wide logout and MicroSoft IIS
    #i libapache2-webkdc libwebkdc-perl

    # Yale CAS, the CAS server depends on Java
    #i libapache2-mod-auth-cas

    # uses SAML, only service provider(SP) part, requires WebAuth/CAS/Cosign
    # to act as home identity provider(IdP)
    #i libapache2-mod-shib2

# Other apache2 modules
i libapache2-mod-auth-ntlm-winbind
i libapache2-mod-bw libapache2-mod-evasive libapache2-mod-qos libapache2-modsecurity
i libapache2-mod-encoding libapache2-mod-xsendfile
i libapache2-mod-fcgid libapache2-mod-wsgi


# PHP
# Squirrelmail requires libapache2-mod-php5 | php5, the former requires
# libapache2-mpm-prefork, but I want more efficient worker mpm and all
# dynamic web contents running in seperate processes for maximum
# security, that's to say, FastCGI mode or separate WSGI/PSGI mode.
i php5 php5-cgi php5-sasl


# SQL database, Sympa requires mysql | postgresql
#i postgresql

# Web mail: squirrelmail or roundcube or imp4
    #i squirrelmail squirrelmail-compatibility squirrelmail-decode
    #i squirrelmail-logger squirrelmail-quicksave squirrelmail-secure-login
    #i squirrelmail-sent-confirmation squirrelmail-spam-buttons
    #i squirrelmail-viewashtml avelsieve
i roundcube roundcube-plugins roundcube-plugins-extra
# Let's try roundcube-sqlite first.
#i roundcube-pgsql

# mailing list: sympa/mailman
#i sympa
i mailman

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

