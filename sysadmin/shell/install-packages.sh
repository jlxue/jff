#!/bin/sh

set -e

###########################################################
i () {
    PACKAGES="$PACKAGES $@"
}


###########################################################
i "?or(~prequired,~pstandard,~pimportant)"

i git etckeeper

i auditd

i shorewall-init shorewall shorewall6


###########################################################
aptitude update
aptitude install -o Dpkg::Options::=--force-confold \
    -o Dpkg::Options::=--force-confdef --assume-yes $PACKAGES

etckeeper unclean && etckeeper commit "save /etc before any config change"

