#!/bin/bash

set -e

###########################################################
## global variables

XDE_HOME="`dirname $0`"
PKGS_INSTALL_WITH_RECOMMENDS=
PKGS_INSTALL_WITHOUT_RECOMMENDS=

###########################################################
## functions

i () {
    if [ $1 = '-R' ]; then
        shift
        PKGS_INSTALL_WITHOUT_RECOMMENDS="$PKGS_INSTALL_WITHOUT_RECOMMENDS $@"
    else
        PKGS_INSTALL_WITH_RECOMMENDS="$PKGS_INSTALL_WITH_RECOMMENDS $@"
    fi
}

install () {
    [ -z "$2" ] && return

    if [ "$LOGNAME" = root ]; then
        echo "aptitude install $@"
        aptitude install "$@"
    else
        echo "aptitude install -s $@"
        aptitude install -s "$@"
    fi
}

is_installed () {
    dpkg -s "$1" 2>/dev/null | grep "^Status: install ok installed"
}

###########################################################
## packages to be installed

### X server, X display manager, X window manager
i xserver-xorg slim openbox

### theme
i gtk2-engines gtk-theme-switch

### chinese fonts
i ttf-arphic-uming ttf-arphic-ukai ttf-wqy-microhei

### X terminal emulator
# Or: mlterm
i rxvt-unicode-ml

### editor
i vim-gtk

### panel
i tint2

### system monitor
i conky hddtemp

### screen capture
i scrot

### picture viewer
# Or: gpicview geeqie gliv
i feh

### input method
i scim-pinyin

### web browser
# And: fireftp chatzilla
i -R iceweasel icedtea6-plugin flashplugin-nonfree

### mail user agent
# And: lightning or iceowl-extension
i -R icedove

### Office
i -R openoffice.org

### MS Windows compatibility
i wine

### disc recording
# Or: k3b
#i brasero
i genisoimage wodim

### archive manager
i file-roller

### instant messaging
# Or: emesene
i pidgin pidgin-encryption pidgin-hotkeys msn-pecan

### movie player
i mplayer

### music player
# Or: quodlibet listen exaile
i audacious

### printing
i cups

### network configuration
# Or: wicd
#i network-manager

### MS Windows remote desktop
# Or: grdesktop
#i rdesktop

### bluetooth
#i gnome-bluetooth

### PDF reader
# Or: apvlv
i evince

### audio mixer
i alsa-utils

### utilities
i git screen expect etckeeper

### file integrity checker
debsums_already_installed=$(is_installed debsums)
aide_already_installed=$(is_installed aide)

i debsums   #aide samhain fcheck



###########################################################
## install packages

install -r $PKGS_INSTALL_WITH_RECOMMENDS
install -R $PKGS_INSTALL_WITHOUT_RECOMMENDS




###########################################################
## set locale for chinese input method
update-locale LC_CTYPE=zh_CN.UTF-8

###########################################################
## use urxvt
update-alternatives --set x-terminal-emulator /usr/bin/urxvt

###########################################################
## flash plugin
/usr/sbin/update-flashplugin-nonfree --install

###########################################################
## configure debsums

[ "$debsums_already_installed" ] || debsums_init

f=/etc/default/debsums
[ -f $f ] && grep -q '^CRON_CHECK="\?never"\?' $f && {
    echo "Change $f to daily check in cron."
    sed -i -e '/^CRON_CHECK="\?never"\?.*/s//CRON_CHECK=daily/' $f
}

###########################################################
## configure fcheck

###########################################################
## configure samhain

#!!! dpkg hook

###########################################################
## configure aide


