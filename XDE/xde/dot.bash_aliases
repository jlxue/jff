# Reference: procps-3.2.7/ps/output.c:1575
#   {"Std_f",    "uid_hack,pid,ppid,c,stime,tname,time,cmd"},                     /* new -f */
#
# uid_hack's width is 8 by default, which is often too short.
# (procps-3.2.7/proc/pwcache.h:10:#define P_G_SZ 20)
#
alias psf='ps -ouid_hack:20,pid,ppid,c,stime,tname,time,cmd'
alias apt='aptitude -F "%c%a%M%T %40p %20V %v %R:%12s %r %D %I - %60d" -w `stty size | sed "s/.* //"`'

alias newsmth='luit -encoding GBK bbs.sh'
alias minicom='minicom -w'

alias mv='mv -i'
alias cp='cp -i'
. $HOME/bin/safe_rm.sh
alias rm=safe_rm

alias c=clear

export ACK_PAGER="less -R"
which ack-grep >/dev/null 2>&1 && alias ack=ack-grep

alias locallib="eval \$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"

alias gb='git branch'
alias gd='git diff'
alias gdc='git diff --cached'
alias gs='git status'
alias gl='git log'
alias gk='gitk --all'

u () {
    if expr "$1" : "[1-9][0-9]*$" >/dev/null; then
        local arg="$1" s=..
        while [ $((--arg)) -gt 0 ]; do
            s="$s/.."
        done
        cd $s
    else
        local i=$(expr "$PWD" : ".*$1") j
        if [ $i -gt 0 ]; then
            j=$(expr index "$(expr substr "$PWD" $((++i)) 10000)" /)
            [ $j -gt 0 ] && cd "$(expr substr "$PWD" 1 $((i+j)))"
        else
            echo "ERROR: can't find \"$1\" in \"$PWD\"!" >&2
        fi
    fi
}

# for j() function
. $HOME/bin/jump.sh

