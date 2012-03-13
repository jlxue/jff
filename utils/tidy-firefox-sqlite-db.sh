#!/bin/bash

set -e

tidy_places_moz_historyvisits () {
    echo tidy_places_moz_historyvisits "$@"
}


while read f table; do
    db=$(basename $f .sqlite | tr - _)

    n=$(sqlite3 $f "select count(*) from $table")
    printf "%-20s %6d   %s\n" $table $n $f

    if expr "$n" ">=" 10000 >/dev/null; then
        if [ x$(type -t "tidy_${db}_$table") = "xfunction" ]; then
            "tidy_${db}_$table" $f $table
        else
            echo "WARN: no tidy function for table $table in $f" >&2
        fi
    fi
done < <(
    find ~/.mozilla -type f -name "*.sqlite" | while read f; do
        sqlite3 $f ".tables" | sed -e 's/ \+/\n/g' | while read table; do
            [ -z "$table" ] || echo "$f" "$table"
        done
    done
)
