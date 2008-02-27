#!/bin/bash

umask 077

SVK_CVS2SVN=/home/cvs2svn/svk-cvs2svn.sh
LOGFILE=/home/cvs2svn/logs/cvs2svn-`date +%Y%m%d-%H%M%S`.log

if bash -x "$SVK_CVS2SVN" >"$LOGFILE" 2>&1 ; then
	SUBJECT="report from svk-cvs2svn.pl"
else
	SUBJECT="ERROR: REPORT FROM SVK-CVS2SVN.PL !!!"
fi

cat "$LOGFILE" | mail -s "$SUBJECT" cvs2svn
#rm -f "$LOGFILE"

