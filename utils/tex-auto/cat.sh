#!/bin/bash

export LD_LIBRARY_PATH=`pwd`
export LD_PRELOAD=`pwd`/libtex-auto.so

/bin/rm -f "$@"
/bin/cat "$@"
/bin/rm -f "$@"

