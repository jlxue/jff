#!/bin/bash

PKGS_INSTALL_WITH_RECOMMENDS=
PKGS_INSTALL_WITHOUT_RECOMMENDS=

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

###########################################################

### X server, X display manager, X window manager
i xserver-xorg slim openbox

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
# And: lightning
i -R icedove

### Office
i -R openoffice.org

### MS Windows compatibility
i wine

### disc recording
# Or: k3b
i brasero

### archive manager
i file-roller

### instant messaging
# Or: pidgin
i emesene

### movie player
i mplayer

### music player
# Or gmpc
i mpd ario

### printing
i cups

### network configuration
# Or: wicd
i network-manager

### MS Windows remote desktop
# Or: grdesktop
#i rdesktop

### bluetooth
i gnome-bluetooth

### PDF reader
# Or: apvlv
i evince

###########################################################

install -r $PKGS_INSTALL_WITH_RECOMMENDS
install -R $PKGS_INSTALL_WITHOUT_RECOMMENDS

