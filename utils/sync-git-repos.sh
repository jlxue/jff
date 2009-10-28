#!/bin/bash
#
# Resolve the problem:
#   git clone reposA reposB
#   git clone reposB reposC
#       now reposB contains "origin/branch1, origin/branch2, origin/master",
#       but reposC contains only "origin/master".
#
#   This scripts synchronized origin/$branch to $branch in reposB,
#   so that reposC can get all these remote branches.
#

git pull

HEAD=`git symbolic-ref HEAD`

git show-ref | grep "refs/remotes/origin/" | grep -v "HEAD$" |
    while read hash ref; do
        head=${ref##*/}
        [ -z "$head" ] && continue

        echo "Process remote branch $ref..."
        if git show-ref --verify --quiet -- refs/heads/$head; then
            [ "$HEAD" != refs/heads/$head ] && git push . $ref:refs/heads/$head
        else
            git branch $head $ref
        fi
        echo
    done

