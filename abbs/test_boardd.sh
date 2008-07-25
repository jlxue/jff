mkdir -p for_test/tmp for_test/pool 2>/dev/null
cd for_test &&
LD_LIBRARY_PATH=/home/dieken/abbs ../boardd myboard pool &


cd for_test &&
for i in `seq 1 10`; do
    ls pool
    echo hello world $i $i > p-$i
    date >> p-$i
    ps -ef >> p-$i
    ln -s ../p-$i pool/p-$i
    echo
    sleep 1
done

sleep 2

killall boardd


