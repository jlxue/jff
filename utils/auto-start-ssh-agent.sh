#### start ssh-agent automatically when login
##
## add this code snippet to /etc/profile or $HOME/{.profile,.bash_profile,.bashrc}

[ -r $HOME/.ssh-agent-info ] && source $HOME/.ssh-agent-info

if [ -z "$SSH_AGENT_PID" ] || [ -n "$USER" -a "$USER" != "`ps -p $SSH_AGENT_PID -o user=`" ]; then
    killall -u "$USER" ssh-agent 2>/dev/null

    ssh-agent > $HOME/.ssh-agent-info
    [ -r $HOME/.ssh-agent-info ] && source $HOME/.ssh-agent-info
fi

ssh-add -l >/dev/null || ssh-add

#### start ssh-agent automatically when login

