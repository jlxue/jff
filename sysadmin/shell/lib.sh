#!/bin/sh

save_etc () {
    local msg="$1"

    dpkg -l > /tmp/dpkg-list.$$
    cmp -s /tmp/dpkg-list.$$ /etc/dpkg-list.txt || cp /tmp/dpkg-list.$$ /etc/dpkg-list.txt

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
    local src="$1" dst="$2"

    rsync -av --no-owner --no-group "$src" "$dst"
}


overwrite_dir () {
    local src="$1" dst="$2"

    rsync -avr --delete --no-owner --no-group "$src/" "$dst"
}


overwrite_dir_ignore_extra () {
    local src="$1" dst="$2"

    rsync -avr --no-owner --no-group "$src/" "$dst"
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

