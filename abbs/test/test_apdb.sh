#!/bin/bash

ok_num=`grep -cw ok test_apdb.c`
mkdir for_test 2>/dev/null
cd for_test
result=`../test_apdb myboard 2>&1 |tee log | grep -cw ok`

if [ $ok_num -eq $result ]; then
    echo Passed.
    rm myboard.?
else
    echo Failed, see ./for_test/log for details.
fi

