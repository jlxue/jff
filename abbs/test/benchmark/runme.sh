#!/bin/bash

dbs="apdb bdb gdbm tc sqlite3"

{
    for d in $dbs ; do
        ./bench_$d t-$d.db rw
        ./bench_$d t-$d.db rw
        ./bench_$d t-$d.db rw
    done


    for d in $dbs ; do
        i=0
        while [ $((i++)) -lt 20 ]; do
            echo $i
            ./bench_$d t-$d.db r &
        done

        wait
    done
} > log


cat > plot-write.plt <<EOF
set style data linespoints
set term png
set output "plot-write.png"
EOF

cat > plot-read.plt <<EOF
set style data linespoints
set term png
set output "plot-read.png"
EOF

echo -n "plot " >> plot-write.plt
echo -n "plot " >> plot-read.plt

for d in $dbs ; do
    sed -ne "s/$d write.* \([0-9]\+\) ms.*/\1/p" log > write-$d.txt
    sed -ne "s/$d read.* \([0-9]\+\) ms.*/\1/p"  log > read-$d.txt
    echo -n "\"write-$d.txt\", " >> plot-write.plt
    echo -n "\"read-$d.txt\", " >> plot-read.plt
done

echo "\"write-xxx.txt\"" >> plot-write.plt
echo "\"read-xxx.txt\"" >> plot-read.plt

echo "Use gnuplot and plot-write.plt, plot-read.plt to get graphs."

