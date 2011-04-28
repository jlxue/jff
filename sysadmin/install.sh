#!/bin/bash

set -e

PACKAGES="isc-dhcp-server-ldap \
 pdns-backend-ldap \
 gnutls-bin"

#### OpenLDAP:

certtool --generate-privkey --outfile /etc/ldap/self-ca-key.pem
certtool --generate-self-signed --load-privkey /etc/ldap/self-ca-key.pem --outfile /etc/ldap/self-ca-cert.pem
# CN must be fully qualified domain name, see http://www.openldap.org/doc/admin24/tls.html 16.1.1 Server Certificates

chown openldap:openldap /etc/ldap/self-ca-key.pem
chmod 0600 /etc/ldap/self-ca-key.pem
chmod 0644 /etc/ldap/self-ca.pem


ldapmodify -c -f ldap-mods -Y EXTERNAL -H ldapi:///

/etc/init.d/slapd stop
slapindex
chown -R openldap:openldap /var/lib/ldap
/etc/init.d/slapd start

# Backup:
#   /etc        (no krb5 keytab and stash!)
#   /var/backups
#   /var/lib
#   /root
