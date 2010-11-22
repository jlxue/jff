#!/bin/sh

# These environment variables contain strange characters which CmapTools doesn't like.

unset LESS_TERMCAP_mb
unset LESS_TERMCAP_md
unset LESS_TERMCAP_me
unset LESS_TERMCAP_ue
unset LS_COLORS
unset ACK_PAGER
unset TERMCAP
unset LESS_TERMCAP_us
unset LESS_TERMCAP_so
unset LESSOPEN
unset LESSCLOSE
unset LESS_TERMCAP_se
unset PS1

DIR=`which CmapTools`
DIR=`readlink $DIR`
DIR=`dirname $DIR`
if cmp -s /etc/java-6-openjdk/fontconfig.properties $DIR/../jre/lib 2>/dev/null; then
    cp -i /etc/java-6-openjdk/fontconfig.properties $DIR/../jre/lib
fi

CmapTools

