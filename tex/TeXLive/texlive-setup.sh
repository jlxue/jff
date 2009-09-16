#!/bin/bash

# See
# http://support.river-valley.com/wiki/index.php?title=Notes_on_TeX_Live_setup
#   how to use TL data with non-TL binaries
#   how to use different versions of pdftex
#   how to add local packages in a systematic way
#

set -e

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
    local last=$(ls $TEXMFYEAR/texmf.cnf-????????-?????? | tail -1 2>/dev/null)
    local backup=$TEXMFYEAR/texmf.cnf-$(date +%Y%m%d-%H%M%S)

    [ -e $TEXMFYEAR/texmf.cnf ] &&
        { [ -z "$last" ] || ! cmp -s $TEXMFYEAR/texmf.cnf $last ; } &&
        cp $TEXMFYEAR/texmf.cnf $backup &&
        echo "Backuped $TEXMFYEAR/texmf.cnf to $backup."
}

cmd_create_texmfcnf () {
    cmd_backup_texmfcnf
    local texmfhome=$(kpsexpand \$TEXMFHOME)

    grep 'SELFAUTOPARENT\|^TEXMF \|^TEXMFDBS ' $TEXMFMAIN/web2c/texmf.cnf |
        grep -v '^%' > $TEXMFYEAR/texmf.cnf
    sed -i -e "s:.SELFAUTOPARENT:$TEXMFYEAR:" $TEXMFYEAR/texmf.cnf
    echo "TEXMFHOME = $texmfhome" >> $TEXMFYEAR/texmf.cnf

    echo "Created $TEXMFYEAR/texmf.cnf."
}

cmd_shell () {
    echo "TEXMFCNF=$TEXMFCNF"
    echo "MANPATH=$MANPATH"
    echo "INFOPATH=$INFOPATH"
    echo "PATH=$PATH"
    [ -z "$SHELL" ] && bash "$@" || $SHELL "$@"
}

cmd_add_texmf_tree () {
    local name=$1
    local texmf="$2"
    local var

    [ -z "$texmf" -o ! -d "$texmf" ] && {
        echo "Usage: add_texmf_tree texmf-xxx DIRECTORY" >&2
        return 1
    }

    var=${name^^}
    var=${var//-}

    if [ $(kpsexpand \$$var) != \$$var ]; then
        echo "$var is used already!" >&2
        return 1
    fi

    ln -s "$texmf" $TEXMFYEAR/../$name || return 1
    echo "Created symbolic link $TEXMFYEAR/../$name pointing to $texmf."

    cmd_backup_texmfcnf

    sed -i "/^TEXMFLOCAL/ a\\
$var = $texmf
s/^\\(TEXMF = .*\\)}/\1,\$$var}/" $TEXMFYEAR/texmf.cnf

    echo "Added variable $var to $TEXMFYEAR/texmf.cnf and TEXMF in this file."

    texhash "$texmf"
}

cmd_install_pdftex () {
    local prog="$1"
    local pdflatex="$2"
    local pdftex
    local destdir="$(kpsexpand \$TEXMFVAR)"

    [ -z "$prog" ] && prog=pdftex2
    [ -z "$pdflatex" ] && pdflatex=pdflatex2
    pdftex=$(basename "$prog")
    pdflatex=$(basename "$pdflatex")
    which "$prog" >/dev/null || {
        echo "Can't find $prog in PATH or it isn't executable!" >&2
        return 1
    }

    [ -z "$destdir" ] && {
        echo "TEXMFVAR is empty, check your texmf.cnf!" >&2
        return 1
    }
    destdir="$destdir/web2c/pdftex"
    mkdir -p "$destdir"

    local fmt="$(kpsewhich -engine=pdftex $pdftex.fmt || echo)"
    [ "$fmt" ] && {
        echo "You already have $fmt!" >&2
        return 1
    }
    fmt="$(kpsewhich -engine=pdftex $pdflatex.fmt || echo)"
    [ "$fmt" ] && {
        echo "You already have $fmt!" >&2
        return 1
    }

    [ -f "$destdir/$pdftex.fmt" -o -f "$destdir/$pdflatex.fmt" ] && {
        echo "Find $destdir/$pdftex.fmt or $destdir/$pdflatex.fmt, remove them first if you don't need them." >&2
        return 1
    }

    cd "$destdir"
    "$prog" -ini -translate-file=cp227.tcx '*pdfetex.ini'
    "$prog" -ini -translate-file=cp227.tcx '*pdflatex.ini'
    echo; echo; echo

    mv pdfetex.fmt "$pdftex.fmt"
    echo "Generated $destdir/$pdftex.fmt"

    mv pdflatex.fmt "$pdflatex.fmt"
    echo "Generated $destdir/$pdflatex.fmt"

    echo
    echo "Make sure TEXMFCNF environment is $TEXMFCNF,"
    echo "then you can type"
    echo "  $prog story \\\\bye"
    echo "or"
    echo "  $pdflatex sample2e"
    echo "to test the new pdftex."
}

######################## main entry ####################################
which kpsexpand >/dev/null || {
    echo "Can't find kpsexpand in PATH!" >&2
    exit 1
}

TEXMFMAIN=$(kpsexpand '$TEXMFMAIN')
TEXMFYEAR=$(dirname $TEXMFMAIN)

TEXMFCNF=$TEXMFYEAR:$TEXMFMAIN/web2c

[ 0 -eq $(expr index "$MANPATH" "$TEXMFMAIN/doc/man") ] && {
    [ -z "$MANPATH" ] && MANPATH=$TEXMFMAIN/doc/man ||
        MANPATH=$TEXMFMAIN/doc/man:$MANPATH
}

[ 0 -eq $(expr index "$INFOPATH" "$TEXMFMAIN/doc/info") ] && {
    [ -z "$INFOPATH" ] && INFOPATH=$TEXMFMAIN/doc/info ||
        INFOPATH=$TEXMFMAIN/doc/info:$INFOPATH
}

export TEXMFCNF MANPATH INFOPATH


cmd="${1//-/_}"
shift || true

if [ "$cmd" ] && has_sub_command "$cmd"; then
    # execute the sub command
    cmd_$cmd "$@"
else
    echo "Unknown command, run '$0 help' for help information." >&2
    exit 1
fi

