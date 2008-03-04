# for Bash
#
# Usage:
# add these settings to ~/.bashrc:
# alias mv=mv -i
# alias cp=cp -i
# alias rm=safe_rm
# export TRASH_DIR=$HOME/.__trash
# . /path/to/safe_rm.sh
#
# Use "/bin/rm" for real rm and DON'T use rm to remove .__trash
# or files in it.
#

safe_rm () {
	local d t f s

	[ -z "$PS1" ] && (/bin/rm "$@"; return)

	d="${TRASH_DIR:=$HOME/.__trash}/`date +%W`"
	t=`date +%F_%H-%M-%S`
	[ -e "$d" ] || mkdir -p "$d" || return

	for f do
		[ -e "$f" ] || continue
		s=`basename "$f"`
		/bin/mv "$f" "$d/${t}_$s" || break
	done

	echo -e "[$? $t `whoami` `pwd`]$@\n" >> "$d/00rmlog.txt"
}

