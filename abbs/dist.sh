#!/bin/bash

SRC="apdb.h apdb.c board.h board.c bbs.c boardd.c env.h env.c jail.c \
test_apdb.c util.h util.c SConstruct test_apdb.sh test_boardd.sh \
test_jail.sh run_bbs.sh run_boardd.sh"

echo $SRC

D=abbs-`date +%Y%m%d%H%M%S`
mkdir $D

for f in $SRC; do
    cp $f $D/$f
done

tar czvf $D.tar.gz $D

