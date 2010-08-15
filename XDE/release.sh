#!/bin/sh

RELEASE=XDE-`date +%Y%m%d`

echo "Package files to $RELEASE.tar.gz ..."

tar -czf $RELEASE.tar.gz --transform "s|^|$RELEASE/|" \
    install-xde.sh user-xde.sh manual.txt xde RelNotes

echo "Done."

