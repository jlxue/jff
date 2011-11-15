#!/bin/sh

set -e -x

# generate locale data for zh_CN.UTF-8 and en_US.UTF-8
perl -e '
    local $/;
    $_ = <>;
    exit(1) if /^\s*#\s*en_US\.UTF-8/m;
    exit(1) if /^\s*#\s*zh_CN\.UTF-8/m;' /etc/locale.gen || {

    perl -i -pe 's/^\s*#\s*(en_US\.UTF-8.*)$/$1/;
                 s/^\s*#\s*(zh_CN\.UTF-8.*)$/$1/;' /etc/locale.gen

    locale-gen
}


# set default locale
[ -e /etc/default/locale ] || touch /etc/default/locale
perl -e '
    local $/;
    $_ = <>;
    exit(1) if ! /^\s*LANG="?en_US\.UTF8/m;
    exit(1) if ! /^\s*LANGUAGE="?en_US:en/m;
    exit(1) if ! /^\s*LC_CTYPE="?zh_CN\.UTF-8/m;
    exit(1) if ! /^\s*LC_COLLATE="?zh_CN\.UTF-8/m;' /etc/default/locale || {

    update-locale LANG=en_US.UTF-8 LANGUAGE=en_US:en \
        LC_CTYPE=zh_CN.UTF-8 LC_COLLATE=zh_CN.UTF-8
}
