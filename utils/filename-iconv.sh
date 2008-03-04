#! /bin/bash

# convert file name encoding.
#
# Usage:
# filename-iconv.sh from_enc to_enc file1 file2 ...
#
# Refer to iconv(1) for available encoding names.
#
# Generally, use it to convert GBK file names from Windows to
# UTF-8 names in Linux like this:
# (gnome-terminal: Terminal-> Set Character Encoding: UTF-8)
# 	./filename-iconv.sh GBK UTF-8 *
# It should be safe for wrong encoding names or file names 
# encoded in ASCII. You can test it before converting:
# 	mkdir test_it; for f in *; do touch test_it/$f; done
# 	./filename-iconv.sh GBK UTF-8 test_it/*
# 	ls; rm -rf test_it
#
# tip: if you use bash, you can type
# 	./filename-iconv.sh GBK UTF-8 M-*
# to get garbled file names. M-* means pressing Alt-Shift-9.
#
# dieken, 2006-05-05
#
#
# There is a package named "convmv" doing similar thing and
# more powerful.
#

(($# > 2)) &&
	(fe=$1; te=$2; s=""; shift 2;
	for f in "$@"; do
		s=`echo -n "$f" | iconv -f $fe -t $te`
		(($?)) || /bin/mv -i "$f" "$s"
	done)

