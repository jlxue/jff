#!/bin/sh

mkdir -p for_test/pool for_test/tmp 2>/dev/null

LD_LIBRARY_PATH=. ./boardd for_test/myboard for_test/pool

