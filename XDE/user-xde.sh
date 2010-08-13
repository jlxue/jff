#!/bin/bash

set -e

###########################################################
## global variables

XDE_HOME="`dirname $0`"


###########################################################
## functions

sync_dot_file () {
    local f="$1" dest="$2"

    [ -z "$dest" ] && dest="$HOME/${f#$XDE_HOME/xde/dot}"

    if [ -d "$f" ]; then
        echo "rsync directory : $f to $dest ..."
        rsync -ar $f/ $dest
    else
        echo "rsync file      : $f to $dest ..."
        rsync -a $f $dest
    fi
}


###########################################################
## set theme
gtk-theme-switch2 /usr/share/themes/Clearlooks


###########################################################
## dot files

for f in $XDE_HOME/xde/dot.*; do
    sync_dot_file "$f"
done

sync_dot_file $XDE_HOME/xde/vim/_vimrc $HOME/.vimrc
sync_dot_file $XDE_HOME/xde/vim/_gvimrc $HOME/.gvimrc
sync_dot_file $XDE_HOME/xde/vim/vimfiles/ $HOME/.vim

sync_dot_file $XDE_HOME/xde/bin/ $HOME/bin

