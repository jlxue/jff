#!/bin/bash

set -e

###########################################################
## global variables

XDE_HOME="`dirname $0`"

BACKUP_DIR="$HOME/.dot_files"
TIMESTAMP=`date +%Y%m%d-%H%M%S`

###########################################################
## functions

backup () {
    local f="$1"
    local dest="$BACKUP_DIR/${f#$HOME/}_$TIMESTAMP~"

    if [ -e "$f" ]; then
        mkdir -p "$(dirname "$dest")"
        cp "$f" "$dest"
    fi
}

remove_backup_if_not_modified () {
    local f="$1"
    local dest="$BACKUP_DIR/${f#$HOME/}_$TIMESTAMP~"

    if [ -e "$f" -a -e "$dest" ]; then
        if cmp -s "$f" "$dest"; then
            rm "$dest"
        fi
    fi
}

sync_dot_file () {
    local f="$1" dest="$2" backup_dir

    [ -z "$dest" ] && dest="$HOME/${f#$XDE_HOME/xde/dot}"
    backup_dir="$BACKUP_DIR/${dest#$HOME/}"

    if [ -d "$f" ]; then
        mkdir -p "$backup_dir"

        echo "rsync directory : $f to $dest ..."
        rsync -ar -b --suffix=_$TIMESTAMP~ --backup-dir="$backup_dir" $f/ $dest
    else
        backup_dir="$(dirname "$backup_dir")"
        mkdir -p "$backup_dir"

        echo "rsync file      : $f to $dest ..."
        rsync -a -b --suffix=_$TIMESTAMP~ --backup-dir="$backup_dir" $f $dest
    fi

    if [ "$BACKUP_DIR" != "$backup_dir" ]; then
        (
            cd $BACKUP_DIR;
            rmdir --ignore-fail-on-non-empty -p "${backup_dir#$BACKUP_DIR/}"
        )
    fi
}

###########################################################
## initialization
mkdir -p "$BACKUP_DIR"

###########################################################
## set theme
f=$HOME/.gtkrc-2.0
backup "$f"
gtk-theme-switch2 /usr/share/themes/Clearlooks
remove_backup_if_not_modified "$f"


###########################################################
## dot files

for f in $XDE_HOME/xde/dot.*; do
    sync_dot_file "$f"
done

sync_dot_file $XDE_HOME/xde/vim/_vimrc $HOME/.vimrc
sync_dot_file $XDE_HOME/xde/vim/_gvimrc $HOME/.gvimrc
sync_dot_file $XDE_HOME/xde/vim/vimfiles/ $HOME/.vim

sync_dot_file $XDE_HOME/xde/bin/ $HOME/bin

###########################################################
## menu
backup $HOME/.config/openbox/fd-menu.xml
backup $HOME/.config/openbox/menu.xml
update-menu.pl --template $HOME/.config/openbox/menu.xml.tmpl \
    --menu $HOME/.config/openbox/menu.xml \
    --output $HOME/.config/openbox/fd-menu.xml \
    /etc/xdg/menus/lxde-applications.menu
remove_backup_if_not_modified $HOME/.config/openbox/fd-menu.xml
remove_backup_if_not_modified $HOME/.config/openbox/menu.xml

openbox --reconfigure

