LD_PRELOAD=`pwd`/libjail.so \
   _R_ALLOW_OPEN_READ="a.c;/usr/share/vim/*;/usr/share/terminfo/*;/etc/vim/*;$HOME/.vim*" \
   _R_ALLOW_OPEN_WRITE="a.c;/tmp/*;$HOME/.viminfo;$HOME/.viminfo.tmp" \
   _R_ALLOW_OPENDIR="/usr/share/vim/*;$HOME/.vim/*" \
   rvim a.c
