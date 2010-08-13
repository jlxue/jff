安装vim 7.2 for windows以及附加程序:


vim 7.2; (自带diff 2.7, 似乎就是取自unxutils)
	http://www.vim.org

Exuberant Ctags 5.6; (vim\vim72\ctags.exe)
	http://ctags.sourceforge.net

cscope 16.0a; (vim\vim72\cscope.exe, vim\vim72\sort.exe)
	http://iamphet.nm.ru/cscope/index.html

grep 2.5.1; (vim\vim72\grep.exe)
	http://unxutils.sourceforge.net
	(or http://gnuwin32.sourceforge.net)

iconv.dll (vim\vim72\iconv.dll, charset.dll, iconv.exe)
	http://sourceforge.net/projects/gettext	[libiconv-1.9.1.bin.woe32.zip]


plugins: (www.vim.org)
bufexplorer		按F7显示缓冲区列表
mark			用不同颜色标记某些词
omnicppcomplete		C++的omni complete，注意 ctags 需要特定选项(参考_vimrc中
                    注释), 且需要先生成 tags 文件并设置好 vim 的 tags 选项
SearchComplete		搜索时按Tab键补全
taglist			    按F8显示函数、变量等列表
vcscommand		    CVS, SVN 集成
word_complete		单词自动完成，选择“工具”->“Word Complete”才激活

关于各插件的使用见vimfiles\doc中帮助文档和vimfiles\plugins下各个插件开头的注释。
安装完插件后记得在vim里头:helptags path_to_vimfiles\doc以便刷新帮助索引。


两个批处理教本doMinGW.bat, doVC.bat是用于自动编译CVS或者SVN版VIM，需要视
情况修改路径。

编译完后的安装方法：
将vim7\runtime里面所有文件拷贝到c:\vim\vim72\;
将vim7\src下以及其子目录下所有dll和exe文件拷贝到c:\vim\vim72\下;
如果以前没有安装vim71或者装在别的目录下则执行c:\vim\vim72\install.exe

然后将上面的可执行程序以及插件等拷贝到c:\vim\下面对应目录里。

