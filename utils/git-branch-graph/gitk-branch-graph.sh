#!/bin/sh

# to use our modified 'git' command first
export PATH="`dirname $0`:$PATH"

gitk "$@"

