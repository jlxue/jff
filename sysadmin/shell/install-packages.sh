#!/bin/sh

set -e -x

###########################################################
i () {
    PACKAGES="$PACKAGES $@"
}


###########################################################
i "?or(~prequired,~pstandard,~pimportant)"

## manage /etc under git
i git etckeeper

## security
i auditd
i debsums aide samhain
i chkrootkit rkhunter   # loop in unhide.rb
i fail2ban snort

## firewall
i shorewall-init shorewall shorewall6


###########################################################
APT_LISTBUGS_FRONTEND=none
APT_LISTCHANGES_FRONTEND=none
DEBIAN_FRONTEND=noninteractive
export APT_LISTBUGS_FRONTEND APT_LISTCHANGES_FRONTEND DEBIAN_FRONTEND

aptitude -q update

aptitude -q install -o Dpkg::Options::=--force-confold \
    -o Dpkg::Options::=--force-confdef --assume-yes -s $PACKAGES |
        grep -q 'No packages will be installed' || {

    aptitude -q install -o Dpkg::Options::=--force-confold \
        -o Dpkg::Options::=--force-confdef --assume-yes $PACKAGES
}

! etckeeper unclean || etckeeper commit "save /etc before any config change"

