@echo off
rem set path=c:\winnt;c:\winnt\system32
rem vcvars32.bat

cd src

rem .sh文件跟其它程序有关联,避免运行VC的link时执行了link.sh
rem ren link.sh link.sh.old


set IME_OPT=MBYTE=yes IME=yes GDYNAMIC_IME=yes GIME=yes
set PERL_OPT=PERL=C:\Perl DYNAMIC_PERL=yes PERL_VER=510
set PYTHON_OPT=PYTHON=C:\Python25 DYNAMIC_PYTHON=yes PYTHON_VER=25
set DEBUG_OPT=DEBUG=yes MAP=lines
set OTHER_OPT=GUI=yes OLE=yes CPUNU=i586 SNIFF=yes CSCOPE=yes

set ALL_OPT=FEAUTURES=HUGE %IME_OPT% %PERL_OPT% %PYTHON_OPT% %DEBUG_OPT% %OTHER_OPT%


echo clean...
nmake -f Make_mvc.mak clean %ALL_OPT%
FOR /D %%d IN (ObjGO*) DO rd /s/q %%d


echo building with options:
echo %ALL_OPT%

nmake -f Make_mvc.mak all %ALL_OPT%

rem ren link.sh.old link.sh
echo done.

echo on

