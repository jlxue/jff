= tex-auto =
dieken at newsmth
2007-11-22

利用 LD_PRELOAD 拦截 open 等系统调用，以实现 TeX 宏包的按需安装。


== 原理 ==

执行 tex-auto.pl 会建立两个 fifo 文件： `$HOME/.tex-auto-i` ，用于 tex
从 tex-auto.pl 读取反馈，`$HOME/.tex-auto-o` ，用于 tex 向 tex-auto.pl
写入请求;

使用 LD_PRELOAD 动态库拦截机制拦截 tex 对 open()、access() 等函数
的调用，如果发现访问文件失败，则打开 `$HOME/.tex-auto-o` 写入需要访问
的文件名，被 tex-auto.pl 读取后查找宏包并安装，将安装情况（成功为
ok，失败为 failed）写入 `$HOME/.tex-auto-i` ，tex 进程得到这个反馈后，
被拦截的函数调用内部会决定是否需要再次访问文件。


== 编译 ==

* `make` 或者 `make D=y` ，后者会在生成的动态库中加入一些 log;
* 在一个单独的终端中执行 `./tex-auto.pl` ，以免此脚本输出的 log 干扰
  被拦截的应用的正常输出;
* `./cat.sh file1 file2 file3` ，cat.sh 调用 cat 输出这些事先并不存在
  的文件;
* 使用完后用 `Ctrl-C` 终止 tex-auto.pl。


== 问题 ==

* 由于对两个 fifo 文件的使用没有加锁，因此不能同时有多个 tex
  线程或进程执行
* 如果一个宏包没有安装，tex 会在当前目录下查找此宏包的 .sty 文件，
  而宏包一般安装到 `$TEXMF` 或者 `$TEXMFLOCAL` 下，在自动安装之后如何
  让 tex 读取新安装的文件，而无需重新执行 tex 命令或者再次输入包名？
  如果修改 kpathsea 库代码(`kpathsea/tex-file.c:kpse_find_file()`)，可以
  让 kpathsea 调用 tex-auto.pl 自动安装后再返回正确的路径。
* 只适用于 Linux


== 计划 ==

在 tex-auto.pl 中实现自动安装宏包的逻辑，目前只是一个示例。


