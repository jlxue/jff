#!/bin/sh
#
# Modified according to this post on NewSMTH BBS:
#
# 发信人: feigepp (看多，上地破两万！), 信区: LinuxDev
# 标  题: Re: 问一个shell脚本
# 发信站: 水木社区 (Thu Oct 23 13:50:34 2008), 站内
# 
# 一个简单的办法：
# grep -o "[0-9|a-z|A-Z]\+" file |sort|uniq -c
# 
# --
# 家有贤妻
# 

grep -o "\w\+" "$@" | sort | uniq -c

