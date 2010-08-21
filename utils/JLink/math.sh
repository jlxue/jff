#!/bin/sh

DIR=`dirname "$0"`

if [ -z "$MATH_ROOT" ]; then
    MATH_ROOT=`which MathematicaPlayer 2>/dev/null`
    [ -z "$MATH_ROOT" ] && {
        echo "No MathematicaPlayer found in $PATH" >&2
        exit 1
    }

    MATH_ROOT=`readlink "$MATH_ROOT"`
    MATH_ROOT=`dirname "$MATH_ROOT"`
    MATH_ROOT=`dirname "$MATH_ROOT"`
    [ ! -d "$MATH_ROOT" ] && {
        echo "Can't find root directory of your Mathematica installation!" >&2
        exit 1
    }
fi

export MATH_ROOT

JLINK_JAR=$MATH_ROOT/SystemFiles/Links/JLink/JLink.jar
MATH_JAVA=$MATH_ROOT/SystemFiles/Java/Linux/bin/java
MATH_JAVAC=$MATH_ROOT/SystemFiles/Java/Linux/bin/javac

if [ ! -e "$DIR/Mathematica.class" ] ||
        [ "$DIR/Mathematica.java" -nt "$DIR/Mathematica.class" ]; then
    "$MATH_JAVAC" -classpath "$JLINK_JAR" -d "$DIR" "$DIR/Mathematica.java"
fi

if which rlwrap >/dev/null 2>&1; then
    exec rlwrap "$MATH_JAVA" -classpath "$JLINK_JAR:$DIR" Mathematica
elif which uniread >/dev/null 2>&1; then
    exec uniread "$MATH_JAVA" -classpath "$JLINK_JAR:$DIR" Mathematica
else
    exec "$MATH_JAVA" -classpath "$JLINK_JAR:$DIR" Mathematica
fi

