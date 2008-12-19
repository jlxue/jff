(1) LaTeX + CJK
from
http://bj.soulinfo.com/~hugang/tex/tex2007/YueWang-zhfonts-final_1.01.tar.bz2

http://bbs.ctex.org/viewthread.php?tid=45752

cd ~/
mv .texlive2008 .texlive2008_backup
tar -jxf YueWang-zhfonts-final_1.01.tar.bz2
mv .texlive2007 .texlive2008
# 需要重新产生所有的map文件，因为字体包中的是TL07的map
rm -rf .texlive2008/texmf-var/web2c
updmap

此字体包默认用了 simsun 一套字体，想用方正书宋那套字体需要修改
.texlive2008\texmf-var\fonts\map\dvipdfm\cid-x.map，或者直接在
源文件里指定字体。
==========================================
(2) XeLaTeX + xeCJK

将 xecjk-2.2.9.zip 解压放到 ~/.texlive2008/texmf-var/ 下面
执行 texhash
把下面代码保存为 test.tex

% # -*- coding: utf-8 -*-
\documentclass[11pt]{article}
\usepackage{xeCJK}         
\setCJKmainfont{SimSun}         
                    
%\setCJKmainfont[BoldFont=Adobe Heiti Std,ItalicFont=Adobe Kaiti %Std]{Adobe Song Std}
%\setCJKsansfont{Adobe Heiti Std}         
%\setCJKmonofont{Adobe Fangsong Std}  

\begin{document}
中文
\end{document}

然后执行:
xelatex test.tex
=====================================
PATH=/usr/local/texlive/2008/bin/i386-linux:$PATH; export PATH
MANPATH=/usr/local/texlive/2008/texmf/doc/man:$MANPATH; export MANPATH
INFOPATH=/usr/local/texlive/2008/texmf/doc/info:$INFOPATH; export INFOPATH

