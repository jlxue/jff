#!/bin/bash
HOME=/home/cvs2svn

CVS_REPOS=:pserver:cvs2svn@localhost:24/cvsroot
CVS_WC=$HOME/cvs

SVN_REPOS=file://$HOME/svn_repos
SVN_WC=$HOME/svn_wc

SVN_LOAD_DIRS=$HOME/svn_load_dirs.pl

export CVSROOT=$CVS_REPOS
#cvs login || exit 1

if [ ! -e "$CVS_WC" -o ! -d "$CVS_WC" ]; then
	(rm -f "$CVS_WC" && mkdir "$CVS_WC") || exit 1
fi

cd "$CVS_WC" || exit 1

if [ -n "`cvs update -CdP 2>&1 | grep -v ': Updating'`" ]; then
	date >> dirty
fi

if [ -e dirty ]; then
	/usr/bin/perl $SVN_LOAD_DIRS --no_user_input 			\
		--svn_username=cvs2svn --svn_password=mypass 		\
		$SVN_REPOS branches/CMS/mFone 				\
		-t "tags/CMS/mFone-`date +%Y%m%d-%H%M%S`" $CVS_WC	&&
	rm dirty
fi

