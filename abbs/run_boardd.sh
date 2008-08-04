#!/bin/sh

set -x

mkdir -p for_test/pool for_test/tmp 2>/dev/null

./boardd for_test/pool 100 for_test/myboard

