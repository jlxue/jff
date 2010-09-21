#!/bin/bash

set -e

PACKAGES="isc-dhcp-server-ldap \
 pdns-backend-ldap \
 gnutls-bin \
 slapd ldap-utils libnss-ldapd libsasl2-modules-gssapi-mit \
 krb5-admin-server krb5-kdc-ldap libpam-krb5"

aptitude install $PACKAGES

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

#### Kerberos:
#   Default kerberos version 5 realm?       EXAMPLE.COM
#   Kerberos servers for your realm:        krb.example.com
#   Administrative server for your Kerberos realm:      krb.example.com
if [ ! -e /var/lib/krb5kdc/principal ]; then
    cat etc/krb5.conf > /etc/krb5.conf
    krb5_newrealm
    /etc/init.d/krb5-admin-server restart
    /etc/init.d/krb5-kdc restart
    (echo "add_policy -minlength 8 -minclasses 3 admin";
     echo "add_policy -minlength 8 -minclasses 4 host";
     echo "add_policy -minlength 8 -minclasses 4 service";
     echo "add_policy -minlength 8 -minclasses 2 user";) | kadmin.local
fi


# Backup:
#   /etc        (no krb5 keytab and stash!)
#   /var/backups
#   /var/lib
#   /root
