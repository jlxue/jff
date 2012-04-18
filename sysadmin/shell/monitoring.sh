#!/bin/sh

set -e -x

SCRIPT_DIR=$(readlink -f $(dirname $0))
. $SCRIPT_DIR/lib.sh

ensure_service_started apache2 apache2

