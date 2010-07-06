#!/bin/bash

set -e

export PERLBREW_ROOT=$HOME/perlbrew
target=perl-5.12.1

if [ -d $PERLBREW_ROOT/locallib ]; then
    source $PERLBREW_ROOT/etc/bashrc
    eval $(perl -I$PERLBREW_ROOT/locallib/lib/perl5 -Mlocal::lib=$PERLBREW_ROOT/locallib)
    perl -e 'print join("\n", @INC), "\n"'
    bash
else
    if ! perlbrew installed | fgrep -q $target; then
        perlbrew init
        perlbrew --force install perl-5.12.1
    fi

    perlbrew switch $target

    (
        cd $PERLBREW_ROOT/build
        p=local-lib-1.006004
        if [ ! -d $p ]; then
            wget http://search.cpan.org/CPAN/authors/id/G/GE/GETTY/$p.tar.gz
            tar xf $p.tar.gz
            cd $p
            perl Makefile.PL --bootstrap=$PERLBREW_ROOT/locallib
            make install

            cd $PERLBREW_ROOT/
            find locallib -print > locallib.list
        fi
    )
fi

