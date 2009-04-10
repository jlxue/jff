#!/bin/sh

[ -z "$1" ] && cmd="spawn -noecho ssh newsmth" ||
    cmd="spawn -noecho $@"
# send "Ctrl-L"
cmd="$cmd; "'
interact {
    timeout 60 { send "\014"}
}'

LC_ALL=zh_CN.GBK expect -c "$cmd"

