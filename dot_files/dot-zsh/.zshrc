#命令提示符 {{{
RPROMPT=$(echo '%{\033[31m%}%D %T%{\033[m%}')
PROMPT=$(echo '%{\033[34m%}%M%{\033[32m%}%/
%{\033[36m%}%n%{\033[01;33m%} >>> %{\033[m%}')
#}}}

## import other source{{{
for zshrc_snipplet in ~/.zsh.d/S[0-9][0-9]*[^~]; do
    source $zshrc_snipplet
done
#}}}

## END OF FILE #################################################################
# vim:filetype=zsh foldmethod=marker autoindent expandtab shiftwidth=4
