#!/bin/sh

set -e

kdc_db_pw="$1"
krb_realm="$2"
krb5kdc_db="$3"

[ -z "$kdc_db_pw" ] && kdc_db_pw=/etc/krb5kdc/kdc_master_db_pw
[ -z "$krb_realm" ] && krb_realm=CORP.EXAMPLE.COM
[ -z "$krb5kdc_db" ] && krb5kdc_db=/var/lib/krb5kdc/principal

if [ ! -e "$krb5kdc_db" ]; then
    pw=`pwgen -cnys 24 1`

    ( date; echo $pw; ) >> $kdc_db_pw
    chmod 600 $kdc_db_pw

    ( echo $pw; echo $pw; ) | /usr/sbin/kdb5_util -r $krb_realm -d $krb5kdc_db create -s
fi

( echo 'add_policy -minlength 8 -minclasses 3 admin';
  echo 'add_policy -minlength 8 -minclasses 4 host';
  echo 'add_policy -minlength 8 -minclasses 4 service';
  echo 'add_policy -minlength 8 -minclasses 2 user'; ) | /usr/sbin/kadmin.local

