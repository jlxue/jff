https://github.com/kissyteam/kissy/tree/master/src/cssgrids/

http://lifesinger.org/blog/2009/12/flying-swing-layout-v1/

双飞翼布局 v1.0

Posted on December 7th, 2009 in 开发 by lifesinger

双飞翼布局的由来：渐进增强式布局探讨（上）[1]（下）[2]

源码文件：grids.css
压缩版本：grids-min.css
合并版本：reset-grids-min.css
测试页面：grids-taobao.html

命名规则和样式生成工具：css-generator.html
更新说明

   1. 精简了 grids.css 文件，只保留布局基础样式。需要某种具体布局时，可以用
      css-generator.html 生成。
   2. 更改了命名规则，使其能见名知意。详细命名规则请阅读 css-generator.html

使用说明

   1. 在页面中引入 grids-min.css 或 reset-grids-min.css
   2. 利用生成工具 css-generator.html, 生成需要的布局样式。
   3. 所有布局的 DOM 结构是一样的，可参考 grids-taobao.html
   4. 在布局的 className 中添加 layout grid-sXm0eY

--
[1] http://lifesinger.org/blog/2008/11/pe-layout-1/
[2] http://lifesinger.org/blog/2008/11/pe-layout-2/

