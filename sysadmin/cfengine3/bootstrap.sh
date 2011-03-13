#!/bin/sh

set -e

adjust_cfengine3_dir () {
    [ -L "$1" ] && rm "$1"
    [ -d "$1" ] || mkdir -m 755 "$1"
}

adjust_cfengine3_dir /var/lib/cfengine3/bin
adjust_cfengine3_dir /var/lib/cfengine3/inputs

if [ ! -L "/etc/cfengine3" ]; then
    rmdir /etc/cfengine3
    ln -s /var/lib/cfengine3/inputs /etc/cfengine3
fi

# http://www.cfengine.org/pages/getting_started
cf-key
#cp -a /usr/sbin/cf-* /var/lib/cfengine3/bin
cp -a `dirname "$0"`/inputs/* /var/lib/cfengine3/inputs
chmod 600 /var/lib/cfengine3/inputs/*.cf
cf-agent --bootstrap
cf-agent

echo 'Successfully!'

