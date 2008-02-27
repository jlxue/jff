#!/bin/bash -x
#
# ****************** NOTICE *****************************
#   Run this script manually at the first time, because
#   svk create repository interactively, and Subversion
#   requires user inputs name and password interactively
#   to cache them.
#
#   Adjust the variables below before running.
#
#   Usage:
#   ./svk-cvs2svn.pl
#
# *******************************************************

export SVKROOT=/home/cvs2svn/cvs2svn-svk-repos
export CVSROOT=":pserver:cvs2svn:mypass@localhost:24/cvsroot"
export SVNROOT="http://localhost:80/svn/x-phone/branches/CMS_mFone"

export SVKTMP=/home/cvs2svn/cvs2svn-svk-wc

#--------------------------------------------------------------------------
export MODULE="mFone2.0"

export LANG=C
export LC_ALL=C

date
echo -e "begin\n\n\n"

if [ ! -f "$SVKROOT/config" ]; then
	svk depotmap --init || exit 1
fi

if ! svk ls //mirror >/dev/null 2>&1; then
	svk mkdir -m "make directory for mirros" //mirror || exit 1
fi

if ! svk ls //mirror/$MODULE-cvs >/dev/null 2>&1; then
	svk mirror cvs:$CVSROOT:$MODULE //mirror/$MODULE-cvs || exit 1
fi

svk sync //mirror/$MODULE-cvs || exit 1

if ! svk ls //mirror/$MODULE-svn >/dev/null 2>&1; then
	svk mirror $SVNROOT //mirror/$MODULE-svn || exit 1
	svk sync //mirror/$MODULE-svn || exit 1
	svk smerge -I -l -B //mirror/$MODULE-cvs //mirror/$MODULE-svn || exit 1
else
	svk sync //mirror/$MODULE-svn || exit 1
	svk smerge -I -l //mirror/$MODULE-cvs //mirror/$MODULE-svn || exit 1
fi

if [ ! -d "$SVKTMP" ]; then
	rm -f "$SVKTMP"
	svk co //mirror/$MODULE-svn "$SVKTMP" || exit 1
else
	svk update "$SVKTMP" || exit 1
fi

cd "$SVKTMP" || exit 1
do_commit=0
find . -iname "*.[ch]" -o -iname "*.dsw" -o -iname "*.dsp" -o -iname "*.txt" \
	-o -iname "*.[ch]pp" -o -iname "*.[ch]xx" -o -iname "*.bat"	\
	-o -iname "*.cmd" -o -iname "*.pl" 	|
		while read -r file; do
			eol=`svk pg svn:eol-style "$file"`
			if [ -z "$eol" ]; then
				do_commit=1
				svk ps svn:eol-style native "$file"
			fi
		done

if [ 1 -eq $do_commit ]; then
	svk commit -m "add svn:eol-style(native) to files" || exit 1
fi

echo -e "\n\n\n"
date
echo -e "Done.\n\n\n"

