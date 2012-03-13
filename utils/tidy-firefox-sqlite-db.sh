#!/bin/bash

set -e

if pgrep firefox >/dev/null; then
    echo "Firefox is running, please quit it."
    exit 1
fi

tidy_places_moz_historyvisits () {
    local f="$1" table="$2"

    sqlite3 "$f" \
        "delete from $table where datetime(visit_date/1000000, \"unixepoch\") > datetime(\"now\", \"-1 month\")"
}

tidy_places_moz_places () {
    true
}

tidy_urlclassifier3_moz_classifier () {
    true
}

find ~/.mozilla -type f -name "*.sqlite" | while read f; do
    sqlite3 $f ".tables" | sed -e 's/ \+/\n/g' | while read table; do
        [ -z "$table" ] || {
            db=$(basename $f .sqlite | tr - _)

            n=$(sqlite3 $f "select count(*) from $table")
            printf "%-20s %6d   %s\n" $table $n $f

            if [ x$(type -t "tidy_${db}_$table") = "xfunction" ]; then
                "tidy_${db}_$table" $f $table
            else
                echo "WARN: no tidy function for table $table in $f" >&2
            fi

            sqlite3 "$f" VACUUM
        }
    done
done
