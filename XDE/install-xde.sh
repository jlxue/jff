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
    if dpkg -s "$1" 2>/dev/null | grep "^Status: install ok installed"; then
        echo "installed"
    else
        echo ""
    fi
}

subst () {
    local f="$1" msg="$2" from="$3" to="$4"

    [ -f "$f" ] && grep -q "$from" $f && {
        echo "AUTO-MODIFY $f: $msg"
        sed -i -e "/$from.*/s//$to/" $f
    }
}

###########################################################
## packages to be installed

### X server, X display manager, X window manager
i xserver-xorg slim openbox x11-xserver-utils xscreensaver

### theme
i gtk2-engines gtk-theme-switch desktop-base

### menu
i menu menu-l10n extra-xdg-menus menu-xdg
i lxmenu-data libfile-basedir-perl libxml-sax-perl      # for update-menu.pl

### chinese fonts
i ttf-arphic-uming ttf-arphic-ukai ttf-wqy-microhei

### X terminal emulator
# Or: mlterm
i rxvt-unicode-ml

### editor
i vim-gtk exuberant-ctags

### panel
i tint2

### system monitor
i conky hddtemp

### screen capture
i scrot

### picture viewer
# Or: gpicview geeqie gliv
i feh

### Image Manipulation
#i mtpaint
#i gimp inkscape

### input method
i scim-pinyin

### web browser
# And: fireftp chatzilla
i -R iceweasel icedtea6-plugin flashplugin-nonfree
i xul-ext-downloadstatusbar xul-ext-fission

### mail user agent
# And: lightning or iceowl-extension
i -R icedove
i iceowl-extension xul-ext-dispmua icedove-quotecolors
# Provider for Microsoft Exchange https://addons.mozilla.org/en-US/thunderbird/addon/195279/

### Office
i -R openoffice.org

### MS Windows compatibility
i wine cifs-utils

### disc recording
# Or: k3b
#i brasero
i genisoimage wodim
#i bchunk   # for .bin/.cue

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
i evince-gtk poppler-data

### audio mixer
i alsa-utils

### power management
i upower

### utilities
i git screen expect etckeeper zip unzip unrar rlwrap #uniread

### firewall
i shorewall-init    # it depends shorewall

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
## configure shorewall
subst /etc/default/shorewall "enable shorewall..." \
    '^startup="\?0"\?' 'startup=1'

subst /etc/default/shorewall-init "enable shorewall-init..." \
    'PRODUCTS=""' 'PRODUCTS="shorewall"'

subst /etc/default/shorewall-init "enable shorewall-init for ifupdown..." \
    'IFUPDOWN="\?0"\?' 'IFUPDOWN=1'

###########################################################
## configure debsums

[ "$debsums_already_installed" ] || debsums_init

subst /etc/default/debsums "enable daily check in cron..." \
    '^CRON_CHECK="\?never"\?' 'CRON_CHECK=daily'

###########################################################
## configure fcheck

###########################################################
## configure samhain

#!!! dpkg hook

###########################################################
## configure aide


