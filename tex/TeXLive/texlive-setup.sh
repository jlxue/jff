#!/bin/bash

# See
# http://support.river-valley.com/wiki/index.php?title=Notes_on_TeX_Live_setup
#   how to use TL data with non-TL binaries
#   how to use different versions of pdftex
#   how to add local packages in a systematic way
#

has_sub_command () {
    test "$(type -t "cmd_$1")" = 'function'
}

cmd_help () {
    echo "Available commands:"
    declare -F | sort | while read a b f; do
        [ ${f:0:4} = "cmd_" ] && echo "  " ${f:4}
    done
}

cmd_backup_texmfcnf () {
    [ -e $TEXMFYEAR/texmf.cnf ] &&
        cp $TEXMFYEAR/texmf.cnf $TEXMFYEAR/texmf.cnf-$(date +%Y%m%d-%H%M%S)
}

cmd_create_texmfcnf () {
    backup_texmfcnf

    grep 'SELFAUTOPARENT\|^TEXMF \|^TEXMFDBS ' $TEXMFMAIN/web2c/texmf.cnf |
        grep -v '^%' > $TEXMFYEAR/texmf.cnf
    sed -i -e "s:.SELFAUTOPARENT:$TEXMFYEAR:" $TEXMFYEAR/texmf.cnf
}

cmd_shell () {
    [ -z "$SHELL" ] && bash "$@" || $SHELL "$@"
}

add_texmf_tree () {
    local name=$1
    local texmf="$2"
    local var

    [ -z "$texmf" ] && {
        echo "Usage: add_texmf_tree texmf-xxx DIRECTORY" >&2
        return 1
    }

    var=$(name^^)
    var=$(name//-)
    if [ $(kpsexpand \$$var) != \$$var ]; then
        echo "$var is used already!" >&2
        return 1
    fi

    backup_texmfcnf

    ln -s "$texmf" $TEXMFYEAR/../$name || return 1
    sed -i "/^TEXMFLOCAL/ a\\
$var = $texmf
s/^\\(TEXMF = .*\\)}/\1,\$var}/" $TEXMFYEAR/texmf.cnf

    texhash "$texmf"
}

######################## main entry ####################################
which kpsexpand >/dev/null || {
    echo "Can't find kpsexpand in PATH!" >&2
    exit 1
}

TEXMFMAIN=$(kpsexpand '$TEXMFMAIN')
TEXMFYEAR=$(dirname $TEXMFMAIN)

TEXMFCNF=$TEXMFYEAR:$TEXMFMAIN/web2c
MANPATH=$TEXMFMAIN/doc/man:$MANPATH
INFOPATH=$TEXMFMAIN/doc/info:$INFOPATH

export TEXMFCNF MANPATH INFOPATH


cmd="${1//-}"
shift

if [ "$cmd" ] && has_sub_command "$cmd"; then
    # execute the sub command
    cmd_$cmd "$@"
else
    echo "Unknown command, run '$0 help' for help information." >&2
    exit 1
fi

