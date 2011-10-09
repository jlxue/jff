#!/bin/bash

while read f; do
    perl -ne '
        if (/^sub\s+([^\s\(]+)/ .. /^}\s*$/) {
            ++$n; $s = $1 if $1;
        } else {
            printf "%4d %-30s %s:%d\n", $n, $s, "'"$f"'", $. if $n > 0;
            $n = 0;
        }' $f | sort -k1,1 -nb
done

