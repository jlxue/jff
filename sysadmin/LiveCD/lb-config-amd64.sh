#!/bin/sh

lb config \
    --apt aptitude \
    --apt-source-archives false \
    -a amd64 \
    -b iso-hybrid \
    --cache true \
    --checksums md5 \
    --debian-installer live \
    --debian-installer-distribution sid \
    --debian-installer-gui false \
    -d sid \
    --hostname gold \
    --iso-publisher 'Dieken <dieken@newsmth.net>' \
    -k amd64 \
    -m http://mirrors.163.com/debian/ \
    --parent-mirror-binary http://mirrors.163.com/debian/ \
    --mirror-bootstrap http://mirrors.163.com/debian/ \
    --mode debian \
    --system live \
    --username dieken \

