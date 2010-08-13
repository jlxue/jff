#!/bin/bash

set -e

###########################################################
## global variables

XDE_HOME="`dirname $0`"


###########################################################
## set theme
gtk-theme-switch2 /usr/share/themes/Clearlooks


###########################################################
## dot files

for f in $XDE_HOME/xde/dot.*; do
    echo "rsync $f..."
    rsync -ar $f $HOME/${f#$XDE_HOME/xde/dot}
done

