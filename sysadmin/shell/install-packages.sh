#!/bin/sh

set -e

i () {
    PACKAGES="$PACKAGES $@"
}


i "?or(~prequired,~pstandard,~pimportant)"

i git etckeeper


aptitude update
aptitude install $PACKAGES

etckeeper unclean && etckeeper commit "save /etc before any config change"

