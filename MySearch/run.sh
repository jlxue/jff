#!/bin/sh
classpath=`perl -e 'print join(":", <selenium-java-2.0a4/*.jar>)'`
classpath=$classpath:.

prog=$1
shift

$prog -cp $classpath "$@"

