#!/bin/bash

if [ -z "$1" ] ; then
    echo "Usage: run-ctags-cscope.sh source-dir-list"
    exit 1
fi

while read d; do
    find "$d" -type f -iregex '.*\.\(c\|h\|cpp\|java\)'
done < "$1"  > cscope.files

rm cscope.*out tags
cscope -b -q -k
ctags -L cscope.files --extra=+fq --c-kinds=+px --totals

