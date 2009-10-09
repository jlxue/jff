http://www.newsmth.net/bbscon.php?bid=392&id=738368

发信人: Dieken (风催草低 - 明月何尝不照人), 信区: LinuxApp
标  题: sumatraPDF 对简体中文文档使用外部字体的补丁
发信站: 水木社区 (Fri Oct  9 13:57:37 2009), 站内

没有这个补丁时会用默认的 Mincho, Gothic，这俩包含的汉字
很不全，当 pdf 使用系统不存在的字体时汉字会显示成小黑点。

见附件，写死了 SimSun-18030, simhei, simkai, simfang。

可执行程序是拿 VS 2008 编译的。mupdf 执行时有调试信息，
如下方式查看：

设置环境变量  MULOG 为 a (a 是 all，f 表示只显示字体相关信息)
执行 sumatraPDF.exe a.pdf >a.log，在 Windows 下不重定向的
话是看不到调试信息的。


【 在 Dieken (风催草低 - 明月何尝不照人) 的大作中提到: 】
: 标  题: Re: 有什么 pdf viewer 是支持只显示窗体的?
: 发信站: 水木社区 (Fri Oct  9 13:21:15 2009), 站内
:
: 不是只能用 STSong，我那么搞只是临时测试下，要能支持外部
: 中文字体，有如下三种办法：
:
: 1) 装那个字体；
: 2) 没办法得到指定字体时，修改 pdf 文件，替换为有的字体，
: 注意是 font face name，不是字体文件名。而且需要那个 || 1
: 的暴力办法。
: 3) pdf 文件加密了没法改，修改 sumatraPDF 源码，让其选择
: 正确的替换字体，现在的状态是不管什么编码、粗体啥的都是
: serif 用 MS Mincho, 否则用 MS Gothic。
:
: 我在按第三种方式改，由于 mupdf 没有去支持 otf，所以用
: 不了 Adobe 的四款中文字体了。一会我发补丁和编译好的可执行
: 程序上来。
:

