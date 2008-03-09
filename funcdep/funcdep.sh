#!/bin/bash

usage () {
    echo "funcdep your/project/root"
    exit 1
}

ROOT=.
[ -n "$1" ] && ROOT="$1"

if [ ! -d "$ROOT" ]; then
    echo "$ROOT" is not directory or doesn\'t exists.
    usage
fi

cd "$ROOT"


find . -iname "*.[ch]" -o -iname "*.cpp" | sed -e "s/^\.\///" > cscope.files

ctags -L cscope.files

cscope -bkqu

#perl funcdep.pl tags cscope.out

# functions called by readfile()
#cscope -dL -2readfile

# functions calling readfile()
#cscope -dL -3readfile



#gtags -v

# list all functions in fileio.c
#gobal -f fileio.c

# functions calling readfile()
#global -r -x readfile



# id-utils
#mkid -v
#lid readfile
#fid main.c

