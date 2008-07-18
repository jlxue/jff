mkdir -p for_test/t
cd for_test &&
LD_LIBRARY_PATH=/home/dieken/abbs ../boardd linuxapp t &


cd for_test &&
for i in `seq 1 10`; do
    ls t; 
    echo hello world $i $i > a-$i
    date >> a-$i
    ps -ef >> a-$i
    ln a-$i t/a-$i
    echo
    sleep 1
done

sleep 2

killall boardd


