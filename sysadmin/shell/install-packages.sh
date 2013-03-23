#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

###########################################################
i () {
    PACKAGES="$PACKAGES $@"
}


###########################################################
# Installation of RoudCube, Redmine require PostgreSQL service
# to create database and tables.
[ ! -e /etc/init.d/postgresql ] || ensure_service_started postgresql postgres

###########################################################
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
i libverto-libev1   # krb5-kdc depends libverto1 which depends libverto-libev1 | libverto-glib1
                    # but actually krb5-kdc-1.10+dfsg~alpha2-1 can't start without libverto-libev1

## OpenSSH
i openssh-server

## ldap
i slapd ldap-utils libsasl2-modules-gssapi-mit sasl2-bin
i libpam-ccreds libnss-ldapd nscd nss-updatedb

# other nss cache schemes: nsscache sssd

i ldapscripts smbldap-tools shelldap ldapvi


# antivirus
# XXX: http://web.nvd.nist.gov/view/vuln/search-results?query=clamav&search_type=all&cves=on
i clamav clamav-daemon clamav-unofficial-sigs
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
i apache2-suexec-custom # required by gitolite http-backend
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
i libapache2-mod-bw libapache2-mod-evasive libapache2-mod-qos libapache2-modsecurity
i libapache2-mod-encoding libapache2-mod-xsendfile
i libapache2-mod-fcgid libapache2-mod-wsgi
#i libapache2-mod-passenger


# PHP
# Squirrelmail requires libapache2-mod-php5 | php5, the former requires
# libapache2-mpm-prefork, but I want more efficient worker mpm and all
# dynamic web contents running in seperate processes for maximum
# security, that's to say, FastCGI mode or separate WSGI/PSGI mode.
i php5 php5-cgi php5-sasl


# SQL database, Sympa, Roundcube etc require mysql | postgresql
i postgresql

# Web mail: squirrelmail or roundcube or imp4
    #i squirrelmail squirrelmail-compatibility squirrelmail-decode
    #i squirrelmail-logger squirrelmail-quicksave squirrelmail-secure-login
    #i squirrelmail-sent-confirmation squirrelmail-spam-buttons
    #i squirrelmail-viewashtml avelsieve
i roundcube roundcube-plugins roundcube-plugins-extra
i roundcube-pgsql

# mailing list: sympa or mailman
i mailman
#i sympa
# required modules for sympa, so we can install sympa from source tarball
#i libarchive-zip-perl \
#  libcgi-fast-perl \
#  libcrypt-ciphersaber-perl \
#  libdbd-pg-perl \
#  libdbd-sqlite3-perl \
#  libdbi-perl \
#  libfcgi-perl \
#  libfile-copy-recursive-perl \
#  libfile-nfslock-perl \
#  libhtml-format-perl \
#  libhtml-stripscripts-parser-perl \
#  libhtml-tree-perl \
#  libintl-perl \
#  libio-socket-ssl-perl \
#  libio-stringy-perl \
#  libmail-dkim-perl \
#  libmailtools-perl \
#  libmime-charset-perl \
#  libmime-encwords-perl \
#  libmime-lite-html-perl \
#  libmime-tools-perl \
#  libmsgcat-perl \
#  libnet-ldap-perl \
#  libnet-netmask-perl \
#  libregexp-common-perl \
#  libsoap-lite-perl \
#  libtemplate-perl \
#  libterm-progressbar-perl \
#  libtext-wrap-perl \
#  libunicode-linebreak-perl \
#  libxml-libxml-perl \
#  mhonarc \
#  sqlite3


# calendar service
#   "davical" is powerful but a little tough to setup
#   "radicale" is funny but not powerful and mature enough
#i radicale
i davical php5-curl
i calendarserver

# host git repositories
# XXX: Debian doesn't package rewritten gitolite 3.x yet, I don't want 2.x.
#i gitolite
i gitweb highlight

# host svn repositories
i subversion subversion-tools
i viewvc
# svnserve is preferred than svn apache module
#i libapache2-svn

# dependencies for bugzilla 4.2.x
i libtimedate-perl libdatetime-perl libdatetime-timezone-perl \
  libdbi-perl libtemplate-perl libemail-send-perl libemail-mime-perl \
  liburi-perl liblist-moreutils-perl libmath-random-isaac-perl \
  libdbd-pg-perl libdbd-mysql-perl libgd-gd2-perl libchart-perl \
  libtemplate-plugin-gd-perl libgd-text-perl libmime-tools-perl \
  libwww-perl libxml-twig-perl libnet-ldap-perl libauthen-sasl-perl \
  libauthen-radius-perl libsoap-lite-perl libjson-rpc-perl \
  libjson-xs-perl libtest-taint-perl libhtml-parser-perl \
  libhtml-scrubber-perl libencode-detect-perl libtheschwartz-perl \
  libapache2-mod-perl2 patchutils \
  libfile-flock-perl libfile-slurp-perl \
  libanyevent-perl libevent-perl \
  libapache2-mod-perl2-dev # required by Apache2::SizeLimit

i build-essential # required by bugzilla's install-modle.pl

# dependencies for foswiki 1.1.x
i rcs libapache-htpasswd-perl libcgi-session-perl \
  liblocale-maketext-lexicon-perl libcrypt-passwdmd5-perl \
  libarchive-zip-perl libnet-smtp-ssl-perl \
  liberror-perl libfreezethaw-perl libtime-modules-perl \
  libfcgi-perl libgraphics-magick-perl

# MoinMoin wiki
i python-moinmoin python-flup python-docutils \
    antiword catdoc poppler-utils

i ejabberd imagemagick  # imagemagick is required for CAPTCHA
#i pymsnt   # msn transport for Jabber

i samba smbclient samba-tools

# Trac
i trac trac-announcer trac-bitten trac-customfieldadmin \
  trac-datefieldplugin trac-git trac-icalviewplugin \
  trac-jsgantt trac-mastertickets trac-odtexport \
  trac-subtickets trac-tags trac-wikiprint \
  trac-wysiwyg

# Redmine
i redmine redmine-pgsql libfcgi-ruby1.9.1

# Drupal 7
i drupal7

## Monitoring systems: Nagios/Icinga, Ganglia, Munin, Zabbix

# Nagios
i nagios3 nagios-plugins nagios-plugins-contrib nagios3-doc \
  nagiosgrapher check-postgres
#i pnp4nagios

# Ganglia
i ganglia-monitor ganglia-monitor-python ganglia-webfrontend ganglia-modules-linux

# Munin
#i munin munin-async munin-plugins-extra

# Zabbix
#i zabbix-agent zabbix-frontend-php zabbix-server-pgsql

# For ReviewBoard
i python-setuptools python-dev memcached python-memcache patch \
  python-svn python-psycopg2 python-lucene

# For Gerrit
i default-jre-headless

# BackupPC
#i backuppc rsync libfile-rsyncp-perl openssh-client rrdtool libio-dirent-perl
#i libfilesys-smbclient-perl # http://backuppc.sourceforge.net/faq/limitations.html#incremental_backups_might_not_be_accurate


###########################################################
if [ -z "$interactive" ]; then
    APT_LISTBUGS_FRONTEND=none
    APT_LISTCHANGES_FRONTEND=none
    DEBIAN_FRONTEND=noninteractive
    export APT_LISTBUGS_FRONTEND APT_LISTCHANGES_FRONTEND DEBIAN_FRONTEND

    # See debconf(7) in package debconf-doc
    # The file must be absolute file path!
    DEBCONF_DB_FALLBACK="File{filename:$SCRIPT_DIR/debconf-db.fallback}"
    export DEBCONF_DB_FALLBACK

    update_options=-q
    install_options="-q -o Dpkg::Options::=--force-confold -o Dpkg::Options::=--force-confdef --assume-yes"
else
    update_options=
    install_options=
fi


aptitude $update_options update

aptitude $install_options safe-upgrade

aptitude $install_options -s install $PACKAGES |
        grep -q 'No packages will be installed' || {

    aptitude $install_options install $PACKAGES
}


###########################################################
[ "`etckeeper vcs config --get color.ui`" = auto ] ||
    etckeeper vcs config color.ui auto


save_etc "save /etc before any config change"

