#!/bin/sh

[ -z "$1" ] && cmd="spawn -noecho ssh -1 bbs.newsmth.net" ||
    cmd="spawn -noecho $@"
cmd="$cmd; "'
expect_background eof {     # avoid nasty error when the spawned process exits
    exit
}

interact {
    timeout 60 { send "\x00"}
}'

LC_ALL=zh_CN.GBK expect -c "$cmd"

