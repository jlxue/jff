#!/bin/bash

set -e

PACKAGES="isc-dhcp-server-ldap \
    pdns-backend-ldap \
    slapd ldap-utils \
    libpam-ldapd libnss-ldapd \
    krb5-admin-server krb5-kdc libpam-krb5"

aptitude install $PACKAGES

# Kerberos:
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

