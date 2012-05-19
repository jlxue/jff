#!/bin/sh

export BACKUP_DIR=${BACKUP_DIR:-/root/cfg-bak/`date +%Y%m%d-%H%M%S`}
[ `expr index "$BACKUP_DIR" /` -eq 1 ] || {
    echo "BACKUP_DIR must be absolute path!" >&2
    exit 1
}


save_etc () {
    local msg="$1"

    dpkg -l > /tmp/dpkg-list.$$
    cmp -s /tmp/dpkg-list.$$ /etc/dpkg-list.txt || cp /tmp/dpkg-list.$$ /etc/dpkg-list.txt
    rm /tmp/dpkg-list.$$

    ! etckeeper unclean || etckeeper commit "$msg"
}


ensure_mode_user_group () {
    local file="$1" mode="$2" user="$3" group="$4"

    [ "$1" -a "$2" -a "$3" -a "$4" ] || return 1

    [ -e "$file" ] || {
        echo "[WARN] ensure_mode_user_group(): file not exist - $file"
        return 0
    }

    [ x`stat --printf=%a "$file"` = x$mode ] || chmod $mode "$file"
    [ x`stat --printf=%U "$file"` = x$user -a x`stat --printf=%G "$file"` = x$group ] ||
        chown $user:$group "$file"
}


cmp_file () {
    local src="$1" dst="$2"

    [ -e "$dst" ] && cmp -s "$src" "$dst"
}


cmp_dir () {
    local src="$1" dst="$2"

    [ -d "$dst" ] && diff -aurNq "$@" >/dev/null
}


overwrite_file () {
    local src="$1" dst="$2" bdir="$BACKUP_DIR/`dirname $2`"

    mkdir -p `dirname "$dst"` "$bdir"
    rsync -b --backup-dir "$bdir" -c -av --no-owner --no-group "$src" "$dst"
}


overwrite_dir () {
    local src="$1" dst="$2" bdir="$BACKUP_DIR/$2"

    mkdir -p `dirname "$dst"` "$bdir"
    shift 2
    rsync -b --backup-dir "$bdir" -c -avr --delete --no-owner --no-group "$src/" "$dst" "$@"
}


overwrite_dir_ignore_extra () {
    local src="$1" dst="$2" bdir="$BACKUP_DIR/$2"

    mkdir -p `dirname "$dst"` "$bdir"
    shift 2
    rsync -b --backup-dir "$bdir" -c -avr --no-owner --no-group "$src/" "$dst" "$@"
}


sync_file () {
    cmp_file "$@" || overwrite_file "$@"
}


sync_dir () {
    cmp_dir "$@" || overwrite_dir "$@"
}


file_newer () {
    [ "$1" -a "$2" -a -e "$1" -a -e "$2" -a \( "$1" -nt "$2" \) ]
}

file_older () {
    [ "$1" -a "$2" -a -e "$1" -a -e "$2" -a \( "$1" -ot "$2" \) ]
}

ensure_service_started () {
    local name="$1" cmd="$2"

    [ "$cmd" ] || cmd="$name"

    [ "`pidof $cmd`" ] || service "$name" start
}

capture_match () {
    local pattern="$1" file="$2" flags="$3"

    perl -ne "if ( m{$pattern}$flags ) { print \"\$1\"; exit(0); }" "$file"
}

parse_password_by_pattern () {
    local pattern="$1" file="$2" dummy="$3" passwd_var="$4" newflag="$5" len="$6" passwd=

    [ ! -e "$file" ] || {
        passwd=$(capture_match "$pattern" "$file")
        [ "$passwd" != "$dummy" ] || passwd=
    }

    [ "$passwd" ] || {
        [ "$len" ] || len=24
        passwd=`pwgen -cnys $len 1`
        [ -z "$newflag" ] || eval $newflag=1
    }

    eval $passwd_var=$(echo "$passwd" | sed -e 's|\(.\)|\\\1|g')
}

substitude_template () {
    local tmpl="$1" file="$2" mode="$3" og="$4" flag="$5"

    shift 5
    sed "$@" $tmpl | diff -q $file - >/dev/null || {
        sed "$@" $tmpl > $file
        chmod $mode $file
        chown $og $file
        eval $flag=1
    }
}

pg_run_sql () {
    {
        echo '\set ON_ERROR_STOP'
        echo "$@"
    } | su -c "cd /; psql -w -X -1 -f -" postgres
}

pg_set_role_password () {
    local role="$1" passwd="$2"

    pg_run_sql "ALTER ROLE $role WITH ENCRYPTED PASSWORD '$passwd'"
}

#### modified from /usr/share/davical/dba/create-database.sh
pg_db_users () {
  su postgres -c 'cd /; psql -qXAt -c "SELECT usename FROM pg_user;" template1'
}

pg_create_db_user () {
  if ! pg_db_users | grep "^${1}$" >/dev/null ; then
    su postgres -c "cd /; psql -qXAt -c 'CREATE USER ${1} NOCREATEDB NOCREATEROLE;' template1"
  fi
}

pg_create_db () {
    local dba="$1" dbname="$2"

    su postgres -c "cd /; psql -c '' '$dbname'" 2>/dev/null ||
        su postgres -c "cd /; /usr/bin/createdb --encoding UTF8 --template template0 --owner '$dba' '$dbname'"
}

# create user:group and add the user to other groups
#   add_system_user_group "xxx account" /srv/xxx userA groupA groupB groupC
add_system_user_group () {
    local gecos="$1" home="$2" user="$3" group="$4"

    [ "$group" ] || exit 1

    shift 4

    [ "`getent group $group`" ] || addgroup --system $group
    [ "`getent passwd $user`" ] || adduser --system --home "$home" \
        --shell /bin/false --ingroup $group --disabled-password \
        --disabled-login --gecos "$gecos" $user

    while [ "$1" ]; do
        id -G -n $user | grep -w -q "$1" || adduser $user "$1"
        shift
    done
}

get_or_generate_passwd_in_sasldb () {
    local sasldb="$1" user="$2" realm="$3" passwd="$4" result

    [ "$realm" ] || {
        echo "get_or_generate_passwd_in_sasldb(): realm must be specified" >&2
        exit 1
    }

    sasldblistusers2 -f "$sasldb" 2>/dev/null | grep -q "^$user@$realm:" && {
        result=$(perl -we 'use DB_File; tie %h, "DB_File", $ARGV[0], O_RDONLY or die "$!"; print $h{"$ARGV[1]\0$ARGV[2]\0userPassword"}, "\n"' "$sasldb" "$user" "$realm")
        [ -n "$result" ]
    } || {
        [ "$passwd" ] && result="$passwd" || result=`pwgen -cnys 24 1`
        echo "$result" | saslpasswd2 -p -c -u "$realm" -f "$sasldb" "$user"
    }

    echo "$result"
}

