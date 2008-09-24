#!/bin/sh
#
# Get the minimal source file list used in Linux kernel building process.
#
#   make mrproper
#   touch ../timestamp
#   make allnoconfig
#   make
#   make mrproper
#   find . -anewer ../timestamp -type f -iname '*.[chS]' | sed -e "s/^\.\///" | grep -v "^scripts" | sort > cscope.files

rm tags cscope.*out
cscope -b -k -q     # `cscope -d` to enter cscope browse mode
ctags -L cscope.files \
    -I __initdata,__exitdata,__acquires,__releases \
    -I __read_mostly,____cacheline_aligned,____cacheline_aligned_in_smp,____cacheline_internodealigned_in_smp \
    -I EXPORT_SYMBOL,EXPORT_SYMBOL_GPL \
    --extra=+f --c-kinds=+px \
    --regex-asm="/^ENTRY\(([^)]*)\).*/\1/"

