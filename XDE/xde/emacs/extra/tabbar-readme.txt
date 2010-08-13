enhanced by zslevin at newsmth:

http://www.newsmth.net/bbstcon.php?board=Emacs&gid=49730

发信人: zslevin (Levin Du), 信区: Emacs
标  题: 改了个自己觉得比较顺手的 tabbar，欢迎大家试用
发信站: 水木社区 (Mon Jan  8 17:37:54 2007), 转信

增加的功能有：
 1. 在 tab 上按中键会关闭 buffer，按右键则弹出切换菜单，可以在当前 tabgroup
或其它 tabgroup 中切换。

 2. 加入移动 tab 功能。在原先 scroll 按钮的基础上加入鼠标中键和右键的处理，
例如，在 ">" 上按右键，会将 tab 右移，按中键则移到最右处。

 3. 增加快捷键来切换 buffer。定义了 0~9, a~z 这些快捷键来快速地切换 buffer。

第 3 项功能默认是关闭的，要打开的话，参考下面的设置：

    (require 'tabbar)
    (setq tabbar-speedkey-use t)
    (setq tabbar-speedkey-prefix (kbd "<f1>"))
    (tabbar-mode 1)

这样，在 tab 中就会加入快捷键显示，如 "3'tabbar.el"，这时可以通过 <F1> 3 来
切换到该 buffer。

 4. 增加 M-x tabbar-goto-group，配合自动完成来快速切换到某 tab group。


改进的功能有(以前发过一个版本的)：
 1. 让选中的 tab 始终可见。

希望对大家有所帮助。
--
一度迷茫，几许春秋

※ 修改:·zslevin 于 Jan  8 17:39:06 修改本文·[FROM: 61.234.125.31]
※ 来源:·水木社区 http://newsmth.net·[FROM: 61.234.125.31]

