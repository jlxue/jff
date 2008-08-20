mkdir -p for_test/tmp for_test/pool 2>/dev/null
cd for_test && ../../boardd pool 100 myboard &

sleep 2

cd for_test &&
d=`pwd`/tmp &&
for i in `seq 1 10`; do
    ls pool
    f=p_`date +%Y%m%d%H%M%S`_100_dieken_$i.xxxxxx
    echo hello world $i $i > $d/$f
    date >> $d/$f
    ps -ef >> $d/$f
    ln -s $d/$f pool/$f
    echo
    sleep 1
done

sleep 2

killall boardd


