#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh


my_etckeeper () {
    local command="$1"

    shift
    etckeeper "$command" -d /srv/gerrit/site "$@"
}

init_git_config() {
    local cfg=/srv/gerrit/site/etc/gerrit.config

    git config --file $cfg "$1" >/dev/null || {
        git config --file $cfg "$1" "$2"
        CONF_CHANGED=1
    }
}


## create database user, database, system user
ensure_service_started postgresql postgres

pg_create_db_user   gerrit
pg_create_db        gerrit gerrit

add_system_user_group "Gerrit account" /srv/gerrit gerrit gerrit
[ "x/bin/bash" = x$(perl -le 'print ((getpwnam("gerrit"))[8])') ] ||
        chsh -s /bin/bash gerrit


## download gerrit
ver=2.6-rc0	# also used by "my_etckeeper commit/tag..." below
war=gerrit-$ver.war
[ -e /srv/gerrit/$war ] || {
    rm -f /tmp/$war
    wget -O /tmp/$war 'http://gerrit.googlecode.com/files/gerrit-2.6-rc0.war'
    [ 2e45287c38875528af8ced2497995f51 = `md5sum /tmp/$war` ] &&
        [ 28af453c062b222f51ed02f8b8807f928030e567 = `sha1sum /tmp/$war` ]
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

sync_file $SCRIPT_DIR/etc/init.d/gerrit /etc/init.d/gerrit


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


## version control /srv/gerrit/site
[ -e /srv/gerrit/site/.gitignore ] || {
    echo cache/
    echo git/
    echo logs/
    echo tmp/
} > /srv/gerrit/site/.gitignore

[ -e /srv/gerrit/site/.git ] || {
    my_etckeeper init
    my_etckeeper commit "import Gerrit $ver site"
    my_etckeeper vcs tag "v$ver"
}

! my_etckeeper unclean || my_etckeeper commit "save before configuring"
[ "`my_etckeeper vcs config --get color.ui`" = auto ] || my_etckeeper vcs config color.ui auto


## more settings for Gerrit

# user's preferred email address when they first login
init_git_config auth.emailFormat '{0}@corp.example.com'

# comment links
init_git_config commentlink.changeid.match '(I[0-9a-f]{8,40})'
init_git_config commentlink.changeid.link '#q,$1,n,z'

# /bug|bugzilla|bz|issue|ticket/i, Gerrit doesn't ignore case, I have to
# write it in an ugly way...
init_git_config commentlink.bugzilla.match '\b([bB][uU][gG]|[bB][uU][gG][zZ][iI][lL][lL][aA]|[bB][zZ]|[iI][sS][sS][uU][eE]|[tT][iI][cC][kK][eE][tT])\s*:?\s*#?(\d+)\b'
init_git_config commentlink.bugzilla.link 'http://bugzilla.corp.example.com/show_bug.cgi?id=$2'

# other good settings
init_git_config gerrit.canonicalWebUrl 'http://gerrit.corp.example.com/'

init_git_config sendemail.allowrcpt 'corp.example.com'

init_git_config sshd.advertisedAddress 'gerrit.corp.example.com:2022'

init_git_config trackingid.bugzilla.footer 'Ticket'
init_git_config trackingid.bugzilla.match  '\b(\d{1,10})\b'
init_git_config trackingid.bugzilla.system 'Bugzilla'


#######################################################################
ensure_mode_user_group /etc/default/gerritcodereview    600 gerrit gerrit
ensure_mode_user_group /srv/gerrit                      700 gerrit gerrit
ensure_mode_user_group /srv/gerrit/init-info            600 gerrit gerrit
ensure_mode_user_group /srv/gerrit/truststore           600 gerrit gerrit
ensure_mode_user_group /srv/gerrit/site                 700 gerrit gerrit
ensure_mode_user_group /srv/gerrit/site/etc/gerrit.config       644 gerrit gerrit
ensure_mode_user_group /srv/gerrit/site/etc/secure.config       600 gerrit gerrit
ensure_mode_user_group /srv/gerrit/site/etc/ssh_host_dsa_key    600 gerrit gerrit
ensure_mode_user_group /srv/gerrit/site/etc/ssh_host_rsa_key    600 gerrit gerrit
ensure_mode_user_group /srv/gerrit/site/.git            700 root root
ensure_mode_user_group /srv/gerrit/site/.gitignore      600 root root


update-rc.d gerrit defaults

[ -z "$CONF_CHANGED" ] || {
    service gerrit restart
}

ensure_service_started gerrit GerritCodeReview
ensure_service_started apache2 apache2

! my_etckeeper unclean || my_etckeeper commit "save after configuring"

