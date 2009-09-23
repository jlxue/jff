#!/bin/bash

HOSTS=./hosts
SERVER_PROG=./server.sh
MASTER_PROG=./master.pl

[ -e $HOSTS ] || for ((i = 2000; i < 2010; ++i)); do
    echo localhost $i
done > $HOSTS

while read host port; do
    $SERVER_PROG $port $MASTER_PROG &
done < $HOSTS

