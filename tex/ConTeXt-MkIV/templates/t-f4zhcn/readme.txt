## seems broken:
##
## http://liyanrui.is-programmer.com/posts/10360.html
## "忙活了很久，现在才发现只要使用 pre_linebreak_filter 回调函数，那么
## fallback 字体机制就会失去作用，看样子是跟 Hans 的代码有所冲突。 
## 心灰意冷，不搞了，跟那么一堆很恶心的代码搏斗，实在没有意思。还是把心思放
## 回 Conote 上来。"



from http://bbs.ctex.org/viewthread.php?tid=50440


[ConTeXt] MkIV 一个简单的中文字体模块 f4zhcn

Wolfgang 写的 simplefonts 模块 简化了 ConTeXt MkIV 的字体配置。为了进一
步简化中文字体的使用，我计划基于 simplefonts 模块实现一个 f4zhcn（fonts
for zh_cn）模块。现在先放出一个很简单的版本，具有以下功能：

    1. 默认使用 4 款 Adobe 中文字体
    2. 具有标点压缩及边界对齐功能

不过，现在 simplefonts 模块还不能很好地设置默认字体尺寸。与 Wolfgang 沟
通了一下，现在这个功能基本上有了。但是由于他还没有彻底弄清楚 MkIV 的字体
机制，因此没有正式发布最新版本。

现在我将 Wolfgang 给我的一份最新的 t-simplefonts.tex 与我做的 f4zhcn 模
块打包在一起，见附件。

使用方法如下：

    1. 解包，将所得目录复制到 $TEXROOT/texmf-local/tex/context/third 目录下
    2. 执行以下命令：

        $ luatools --generate
        $ mktexlsr

    3. 测试示例：

\usemodule[f4zhcn][size=12pt]

\starttext

如果你开始做一个项目，最重要的事情是写代码。你必须要写足够的代码让
程序更为有用、漂亮；这可能要数月或多年孤军奋战，除非一些可爱的人们帮助你
来做而不是自行其是。你必须经常发布新版本、快速修正 bug，并且保持着开发的
兴奋。一路走下来，做一个自由软件是一项很繁重的工作。如果你单干，每周起码
要干 10－20 个小时。当然，你可以在现有项目的基础上来做，可以省许多力气，
并且可以让你每周工作 10－20 个小时后，总能看到光明的未来。如果你不能付出
这多时间，就不要自讨苦吃。如果你不能写代码，同上。

\stoptext

[ 本帖最后由 LiYanrui 于 2009-6-5 14:18 编辑 ]

