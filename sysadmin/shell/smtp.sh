#!/bin/sh

set -e -x

[ "`pidof exim4`" ] || service exim4 start

