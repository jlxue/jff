#!/bin/sh

set -e -x

[ "`pidof dovecot`" ] || service dovecot start

