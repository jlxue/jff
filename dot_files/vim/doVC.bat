@echo off

rem set path=c:\winnt;c:\winnt\system32

rem d:\work\vcvars32.bat

cd vim7\src
rem 由于link.sh跟其它程序有关联，因此临时改名了
del link.sh.old
ren link.sh link.sh.old

@echo clean...
nmake -f Make_mvc.mak clean GUI=yes OLE=yes MBYTE=yes IME=yes GDYNAMIC_IME=yes GIME=yes PERL=d:\work\program\Perl DYNAMIC_PERL=yes PERL_VER=58 PYTHON=d:\work\program\Python24 DYNAMIC_PYTHON=yes PYTHON_VER=24 CSCOPE=yes CPUNU=i586 DEBUG=yes MAP=lines
rd /s/q ObjGOLYd

@echo make...
nmake -f Make_mvc.mak all GUI=yes OLE=yes MBYTE=yes IME=yes GDYNAMIC_IME=yes GIME=yes PERL=d:\work\program\Perl DYNAMIC_PERL=yes PERL_VER=58 PYTHON=d:\work\program\Python24 DYNAMIC_PYTHON=yes PYTHON_VER=24 CSCOPE=yes CPUNU=i586 DEBUG=yes MAP=lines

rem nmake -f Make_mvc.mak all FEATURES=HUGE GUI=yes OLE=yes MBYTE=yes IME=yes GDYNAMIC_IME=yes GIME=yes PERL=d:\work\program\Perl DYNAMIC_PERL=yes PERL_VER=58 PYTHON=d:\work\program\Python24 DYNAMIC_PYTHON=yes PYTHON_VER=24  SNIFF=no CSCOPE=yes CPUNU=i586 DEBUG=yes MAP=lines

ren link.sh.old link.sh

@echo done.

pause

