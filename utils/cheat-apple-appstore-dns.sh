#!/bin/sh

# See http://dns.v2ex.com/  DNS server: 199.91.73.222, 178.79.131.110
# See https://smarthosts.googlecode.com/svn/trunk/mobile_devices/iOS/hosts
#
# IP for a*.phobos.apple.com:
#   203.78.36.40
#   203.78.36.42
#   219.76.10.14

sudo dnsmasq -h -r `pwd`/appstore-resolv.conf -H `pwd`/appstore-hosts.txt -q -8 /tmp/dnsmasq.log -b -D -C /dev/null

