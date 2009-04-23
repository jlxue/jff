#### start ssh-agent automatically when login
##  v1.0
##  A better alternative is keychain written by drobbins@gentoo.org
##
## add this code snippet to /etc/profile or $HOME/{.profile,.bash_profile,.bashrc}

[ -r $HOME/.ssh-agent-info ] && . $HOME/.ssh-agent-info

uid=`id -u`
[ -z "$SSH_AGENT_PID" ] && agent_uid= || agent_uid=`ps -p $SSH_AGENT_PID -o uid=`

if [ -z "$agent_uid" ] || [ "$uid" -ne "$agent_uid" ]; then
    killall -u "$USER" ssh-agent 2>/dev/null

    ssh-agent > $HOME/.ssh-agent-info
    [ -r $HOME/.ssh-agent-info ] && . $HOME/.ssh-agent-info
fi

if [ -n "$SSH_AGENT_PID" ]; then
    ps -C ssh-agent -o uid=,pid= | while read u p; do
        [ "$u" -eq "$uid" ] && [ "$p" -ne "$SSH_AGENT_PID" ] && kill $p
    done
fi

unset uid
unset agent_uid

ssh-add -l >/dev/null || ssh-add

#### start ssh-agent automatically when login

