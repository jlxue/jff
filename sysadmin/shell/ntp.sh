#!/bin/sh

set -e -x

[ "`pidof ntpd`" ] || service ntp start

