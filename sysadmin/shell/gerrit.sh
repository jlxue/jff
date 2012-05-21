#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


## create database user, database, system user
ensure_service_started postgresql postgres

pg_create_db_user   gerrit
pg_create_db        gerrit gerrit

add_system_user_group "Gerrit account" /srv/gerrit gerrit gerrit
[ "x/bin/bash" = x$(perl -le 'print ((getpwnam("gerrit"))[8])') ] ||
        chsh -s /bin/bash gerrit


## download gerrit
war=gerrit-2.3.war
[ -e /srv/gerrit/$war ] || {
    rm -f /tmp/$war
    wget -O /tmp/$war 'http://gerrit.googlecode.com/files/gerrit-2.3.war'
    [ 0b2181bdc9519d30181185dc9a09433c = `md5sum /tmp/$war` ] &&
        [ 9034d2f21676b85c8b4633a98c524a6d7fa18571 = `sha1sum /tmp/$war` ]
    mv /tmp/$war /srv/gerrit/$war
}
[ -e /srv/gerrit/gerrit.war ] || ln -s /srv/gerrit/$war /srv/gerrit/gerrit.war


## prepare truststore for SSL certificate verification
## -- MUST be done before creating a Gerrit site
truststore=/srv/gerrit/truststore
truststorepw=changeit
keytool -list -alias exim -keystore $truststore -storepass $truststorepw || {
    echo y | keytool -importcert -alias exim -file /etc/exim4/exim.crt -keystore $truststore -storepass $truststorepw
    chown gerrit:gerrit $truststore
    chmod 600 $truststore
    CONF_CHANGED=1
}

cmp_file $SCRIPT_DIR/etc/default/gerritcodereview /etc/default/gerritcodereview || {
    overwrite_file $SCRIPT_DIR/etc/default/gerritcodereview /etc/default/gerritcodereview
    CONF_CHANGED=1
}


## create a Gerrit site
[ -e /srv/gerrit/init-info ] || {
    set +x
    db_password=`pwgen -cnys 24 1`
    pg_set_role_password gerrit "$db_password"

    smtp_password=$(get_or_generate_passwd_in_sasldb /etc/exim4/sasldb2 gerrit corp.example.com)
    [ "$smtp_password" ]

    f=/srv/gerrit/init-info
    tmpl=$SCRIPT_DIR/$f
    substitude_template "$tmpl" "$f" 600 gerrit:gerrit CONF_CHANGED \
        -e "s/@@GERRIT_DB_PASSWORD@@/$db_password/" \
        -e "s/@@GERRIT_SMTP_PASSWORD@@/$smtp_password/"
    set -x
}

[ -e /srv/gerrit/site ] || {
    set +x
    cat >&2 <<EOF
ERROR: require manually action!

Run these commands to initialize Gerrit:
    (root)# su gerrit
    (gerrit)$ cd
    (gerrit)$ java -jar gerrit.war init -d /srv/gerrit/site

The last command will ask many questions, you can find the answers
from /srv/gerrit/init-info.

Don't access http://127.0.0.1:2080/#/admin/projects/, you should access
http://gerrit.corp.example.com/#/admin/projects/ instead.

    !!! After that run this script again!
EOF

    exit 1
}


###########################
ensure_mode_user_group /etc/default/gerrit      600 gerrit gerrit
ensure_mode_user_group /srv/gerrit              700 gerrit gerrit
ensure_mode_user_group /srv/gerrit/init-info    600 gerrit gerrit
ensure_mode_user_group /srv/gerrit/truststore   600 gerrit gerrit
ensure_mode_user_group /srv/gerrit/site         700 gerrit gerrit
ensure_mode_user_group /srv/gerrit/site/etc/secure.config       600 gerrit gerrit
ensure_mode_user_group /srv/gerrit/site/etc/ssh_host_dsa_key    600 gerrit gerrit
ensure_mode_user_group /srv/gerrit/site/etc/ssh_host_rsa_key    600 gerrit gerrit


[ -z "$CONF_CHANGED" ] || {
    /srv/gerrit/site/bin/gerrit.sh stop
    /srv/gerrit/site/bin/gerrit.sh start
}

[ "`pidof GerritCodeReview`" ] || /srv/gerrit/site/bin/gerrit.sh start

ensure_service_started apache2 apache2

