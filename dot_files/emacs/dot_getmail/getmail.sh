#!/bin/bash

# 避免 cron 不导出环境变量
export USER=dieken
export HOME=/home/dieken

P=`ps -C getmail u | grep "^$USER"`

# 如果当前用户没有 getmail 正在运行并且 .getmail
# 下没有 nogetmail 文件，则开始收信。
if [ -z "$P" -a ! -e "$HOME/.getmail/nogetmail" ]; then
    getmail -r gmail.rc -r 126.rc >/dev/null
fi

