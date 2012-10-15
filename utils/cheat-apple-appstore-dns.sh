#!/bin/sh

# See http://dns.v2ex.com/  DNS server: 199.91.73.222, 178.79.131.110

sudo dnsmasq -h -r `pwd`/appstore-resolv.conf -H `pwd`/appstore-hosts.txt -q -8 /tmp/dnsmasq.log -b -D -C /dev/null

