#!/bin/sh

set -e

adjust_cfengine3_dir () {
    [ -L "$1" ] && rm "$1"
    [ -d "$1" ] || mkdir -m 755 "$1"
}

adjust_cfengine3_dir /var/lib/cfengine3/bin
adjust_cfengine3_dir /var/lib/cfengine3/inputs
adjust_cfengine3_dir /var/lib/cfengine3/masterfiles

if [ ! -L "/etc/cfengine3" ]; then
    rmdir /etc/cfengine3
    ln -s /var/lib/cfengine3/inputs /etc/cfengine3
fi

cf-key
rm -rf /var/lib/cfengine3/inputs/*
rm -rf /var/lib/cfengine3/masterfiles/*

cp -a `dirname "$0"`/inputs/failsafe.cf /var/lib/cfengine3/inputs
cp -a `dirname "$0"`/inputs/update.cf   /var/lib/cfengine3/inputs
cp -a `dirname "$0"`/inputs/*           /var/lib/cfengine3/masterfiles

chmod 600 /var/lib/cfengine3/inputs/*.cf
chmod 600 /var/lib/cfengine3/masterfiles/*.cf

cf-agent --bootstrap
cf-agent -K -v -I

echo 'Successfully!'

