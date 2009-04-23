#### start ssh-agent automatically when login
##
## add this code snippet to /etc/profile or $HOME/{.profile,.bash_profile,.bashrc}

[ -r $HOME/.ssh-agent-info ] && source $HOME/.ssh-agent-info

uid=`id -u`

if [ -z "$SSH_AGENT_PID" ] || [ "$uid" -ne "`ps -p $SSH_AGENT_PID -o uid=`" ]; then
    killall -u "$USER" ssh-agent 2>/dev/null

    ssh-agent > $HOME/.ssh-agent-info
    [ -r $HOME/.ssh-agent-info ] && source $HOME/.ssh-agent-info
fi

if [ -n "$SSH_AGENT_PID" ]; then
    ps -C ssh-agent -o uid=,pid= | while read u p; do
        [ "$u" -eq "$uid" ] && [ "$p" -ne "$SSH_AGENT_PID" ] && kill $p
    done
fi

unset uid

ssh-add -l >/dev/null || ssh-add

#### start ssh-agent automatically when login

