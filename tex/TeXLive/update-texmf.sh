#!/bin/bash

set -e

add_package () {
    PACKAGES="$PACKAGES $PKG"
    eval ${PKG}_NAME=$PKG
    eval ${PKG}_DESTDIR=$DESTDIR
    eval ${PKG}_VCS=$VCS
    eval ${PKG}_URL=$URL
}

svn_checkout () {
    svn checkout "$1" "$2"
}

svn_update () {
    svn update
}

svn_export () {
    mkdir -p "$2"
    svn export --force "$1" "$2"
}

######################################################################
if [ -z "$TEXMFHOME" ]; then
    which kpsexpand >/dev/null || {
        echo "Can't find your TeXLive installation, do you set your PATH properly?" >&2
        exit 1
    }

    TEXMFHOME="`kpsexpand '$TEXMFHOME'`"
fi
echo "TEXMFHOME is $TEXMFHOME"
[ -d "$TEXMFHOME" ] || mkdir -p "$TEXMFHOME"


[ "$REPOS_TEXMFHOME" ] ||
    REPOS_TEXMFHOME=`dirname "$TEXMFHOME"`/repos-`basename "$TEXMFHOME"`
echo "REPOS_TEXMFHOME is $REPOS_TEXMFHOME"
[ -d "$REPOS_TEXMFHOME" ] || mkdir -p "$REPOS_TEXMFHOME"


PKG=xeCJK
DESTDIR=/
VCS=svn
URL=http://ctex-kit.googlecode.com/svn/trunk/xecjk
add_package

PKG=ctex
DESTDIR=/tex/latex/ctex
VCS=svn
URL=http://ctex-kit.googlecode.com/svn/trunk/ctex
add_package

PKG=zhmetrics
DESTDIR=/
VCS=svn
URL=http://ctex-kit.googlecode.com/svn/trunk/chinese-fonts
add_package

PKG=CJKpunct
DESTDIR=/
VCS=svn
URL=http://ctex-kit.googlecode.com/svn/trunk/CJKpunct
add_package

for p in $PACKAGES ; do
    eval vcs=\$${p}_VCS
    eval url=\$${p}_URL
    eval destdir=\$${p}_DESTDIR

    echo "Package $p: $vcs $url $TEXMFHOME/$destdir"

    if [ -d "$REPOS_TEXMFHOME/$p" ]; then
        ( cd "$REPOS_TEXMFHOME/$p"; ${vcs}_update )
    else
        ${vcs}_checkout "$url" "$REPOS_TEXMFHOME/$p"
    fi

    ${vcs}_export "$REPOS_TEXMFHOME/$p" "$TEXMFHOME/$destdir"
done

######################################################################
if [ "$NOT_USE_FAKE_ZHMETRICS" ]; then
    [ -e "$TEXMFHOME/fonts/tfm/zhmetrics/gbksong/gbksong00.tfm" ] || (
        cd $TEXMFHOME
        which pltotf >/dev/null 2>&1 || {
            [ -e ./pltotf ] || wget http://www.tug.org/svn/texlive/trunk/Master/bin/`tlmgr print-arch`/pltotf
            chmod a+x ./pltotf
            export PATH=$PATH:.
        }

        texlua "$REPOS_TEXMFHOME/zhmetrics/source/fonts/zhmetrics/zhtfm.lua"
    )
else
    [ -e "$TEXMFHOME/fonts/tfm/zhmetrics/fake/gbksong00.tfm" ] || (
        cd "$TEXMFHOME/fonts/tfm/zhmetrics/fake"
        for f in gbksong gbkhei gbkkai gbkfs gbkli gbkyou; do
            for ((i=0; i < 95; ++i)); do
                ln gbksong00-tfm $f$(printf %02d $i).tfm
                ln gbksong00-tfm ${f}sl$(printf %02d $i).tfm
            done
        done

        for f in unisong unihei unikai unifs unili uniyou; do
            for ((i=0; i < 256; ++i)); do
                ln gbksong00-tfm $f$(printf %02x $i).tfm
                ln gbksong00-tfm ${f}sl$(printf %02x $i).tfm
            done
        done
    )
fi

[ -e "$TEXMFHOME/tex/latex/zhmetrics/c19hei.fd" ] || (
    cd $TEXMFHOME/tex/latex/zhmetrics/ &&
        texlua "$REPOS_TEXMFHOME/zhmetrics/source/fonts/zhmetrics/zhfd.lua"
)

######################################################################
LATEX_PREVIEW=/usr/share/texmf/tex/latex/preview
LATEX_PREVIEW_DESTDIR="$TEXMFHOME/tex/latex"
[ -d "$LATEX_PREVIEW" ] && [ ! -e "$LATEX_PREVIEW_DESTDIR/preview" ] && {
    mkdir -p "$LATEX_PREVIEW_DESTDIR"
    ln -s "$LATEX_PREVIEW" "$LATEX_PREVIEW_DESTDIR/preview"
}

######################################################################
GBK_EUC_UCS2_DESTDIR="$TEXMFHOME/fonts/cmap/dvipdfmx"
[ ! -e "$GBK_EUC_UCS2_DESTDIR/GBK-EUC-UCS2" ] && {
    mkdir -p "$GBK_EUC_UCS2_DESTDIR"
    for f in /usr/share/poppler/cMap/Adobe-GB1/GBK-EUC-UCS2 \
            /usr/share/fonts/cmap/adobe-gb1/GBK-EUC-UCS2 ; do
        [ -e "$f" ] && ln -s "$f" "$GBK_EUC_UCS2_DESTDIR/GBK-EUC-UCS2" && break
    done
}

######################################################################
[ -e ~/.fonts ] && for f in `ls ~/.fonts/{sim*,SIM*} 2>/dev/null` ; do
    dir="$TEXMFHOME/fonts/truetype/windows/sim"
    mkdir -p "$dir"
    name=`basename "$f"`
    name=${name,,}   # lower case
    [ -e "$dir/$name" ] || ln -s "$f" "$dir/$name"
done

[ -e ~/.fonts ] && for f in `ls ~/.fonts/Adobe* 2>/dev/null` ; do
    dir="$TEXMFHOME/fonts/opentype/adobe/zh"
    mkdir -p "$dir"
    name=`basename "$f"`
    [ -e "$dir/$name" ] || ln -s "$f" "$dir/$name"
done

######################################################################
CCT=cct-0.61803-1.tar.bz2
CCT_FTP=ftp://ftp.cc.ac.cn/pub/cct/src/
[ -e "$REPOS_TEXMFHOME/$CCT" ] || {
    wget -O "$REPOS_TEXMFHOME/$CCT" $CCT_FTP/$CCT &&
        rm -rf "$REPOS_TEXMFHOME/cct-dist" &&
        tar -C "$REPOS_TEXMFHOME" -jxvf "$REPOS_TEXMFHOME/$CCT"
}

CCMAP_DISTDIR="$TEXMFHOME/tex/latex/ccmap"
[ -e "$CCMAP_DISTDIR" ] || (
    cd "$REPOS_TEXMFHOME/cct-dist/ccmap"
    mkdir -p "$CCMAP_DISTDIR"
    cp ccmap.sty *.sfd "$CCMAP_DISTDIR"
    latex "\def\cmapEnc{GBK} \input{makecmap.tex}"
    mv *.cmap "$CCMAP_DISTDIR"
    echo "Installed ccmap to $CCMAP_DISTDIR"
)

which gbk2uni >/dev/null || (
    cd "$REPOS_TEXMFHOME/cct-dist/cct"
    gcc -o gbk2uni gbk2uni.c

    for d in ~/bin ~/usr/bin ~/local/bin "`kpsexpand '$TEXMFMAIN'`/../bin/i386-linux" ; do
        [ -d "$d" ] && [ -w "$d" ] && cp gbk2uni "$d" &&
            echo Installed gbk2uni into "$d" && break
    done
)

######################################################################
rm -rf "$TEXMFHOME/doc" "$TEXMFHOME/source" "$TEXMFHOME/README" "$TEXMFHOME/setup-win32"

