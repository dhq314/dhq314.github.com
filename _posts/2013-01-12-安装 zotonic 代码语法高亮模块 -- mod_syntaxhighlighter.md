---
layout: page
title: 安装 zotonic 代码语法高亮模块 -- mod_syntaxhighlighter
---
{% include JB/setup %}

对于一个对代码有严重洁癖的我来说，一直想给文章里的代码弄个语法高亮显示，起码看代码能看得舒服些吧。

还好，在 [zotonic模块库](http://modules.zotonic.com/) 里找到了一个代码高亮的模块 --
[mod_syntaxhighlighter](http://modules.zotonic.com/page/352/mod-
syntaxhighlighter)（也可以说是zotonic的插件吧）。其实这个模块可以说是现在广为流行使用的语法高亮显示
[SyntaxHighlighter](http://alexgorbatchev.com/SyntaxHighlighter/) 在 zotonic
里的集成，也就是说这个模块是基于 SyntaxHighlighter 做的。

**安装 mod_syntaxhighlighter 模块**

安装zotonic官方模块库里模块很简单，一行命令就能安装（下面命令是在 bin
目录下执行，如果bin目录已经export出来，则可以在终端任何位置执行）：

    
    
    zotonic installmodule mod_syntaxhighlighter
    

当然，上面的“一键安装”是有前提的，必须需要 zotonic 版本在7.0版本以上，而且需要系统装有 [git](http://git-scm.com/)
或者 [mercurial](http://mercurial.selenic.com/) 环境，详细的
[zotonic模块安装要求可以看这里](http://modules.zotonic.com/page/326/howto)。

上面的安装方法只适合安装官方模块库里的模块，因为它只默认检索官方模块网站（[modules.zotonic.com](http://modules.zoto
nic.com/)）上的模块，如果存在，则用 git 或者 mercurial 把模块默认下载到 priv/modules
目录下，并且重新编译模块，刷新模块缓存。

这里，我把 mod_syntaxhighlighter
模块迁移到自己网站目录的模块目录下(/priv/sites/网站名/modules)，为了以后升级zotonic版本好方便管理已安装的模块吧。

PS：其实，zotonic 会默认扫描 "modules/"、"priv/modules"、"/priv/sites/网站名/modules"
这3个目录下的模块，所以，你可以把自己的模块放在这3个目录下的其中之一。

安装完毕后，登陆后台打开到模块页面("System - Modules")，找到以 "SyntaxHighlighter"
为名(title)的模块（如果找不到，可以再重新编译make一下，再在状态页面("system - Status")点一下"Rescan
modules"，重新扫描一下模块），点"Activate"激活它。

这样，mod_syntaxhighlighter 模块就安装好了。

**mod_syntaxhighlighter 使用**

由于 mod_syntaxhighlighter 是 SyntaxHighlighter 的 zotonic 集成（其实就是重写override了
base.tpl 下的_html_body.tpl, 把SyntaxHighlighter所需要的一些js、css文件加载进来），所以用法跟
SyntaxHighlighter 是一样的，就是把代码放在<pre>标签里，然后给<pre>标签加上一个class属性，标明上是哪种语言代码类型的着色刷子
(brush)别名，例如下面erlang代码的写法：

    
    
    <pre class="brush: erl">
        %% Just a test
        io:format("Just a test.~n").
    </pre>
    

更多的 SyntaxHighlighter 着色刷子（brush）别名可以看[这里](http://alexgorbatchev.com/SyntaxHig
hlighter/manual/brushes/)。

上面添加<pre>标签的方法是要在源代码里编辑，而且还要记住各种语言代码的着色刷子别名，有没有更方便的操作呢？例如可以弹出一个对话框，在对话框里面黏贴代码，
然后选择相应的语言代码类型就行了。

答案是肯定的。zotonic 使用 [TinyMCE](http://www.tinymce.com/)
作为后台所见即所得(WYSIWYG)的文本编辑器，而 [SyntaxHL](https://github.com/RichGuk/syntaxhl)
则是一个使用 SyntaxHighlighter 来高亮显示代码的 TinyMCE 插件。

**安装 SyntaxHL 插件**

这里把 SyntaxHL 插件安装在当前网站目录下，所以下面操作的目录起点都是在 "/priv/sites/网站名/" 下。

在网站根目录(/priv/sites/网站名/)下创建 TinyMCE 的插件目录：

    
    
    mkdir -p ./lib/js/modules/tinymce3.5.0/plugins
    

上面的"tinymce3.5.0"是指当前 zotonic 使用 TinyMCE 的版本，具体的 TinyMCE 版本可以去目录
/modules/mod_base/lib/js/modules/ 下查看对应的 TinyMCE 文件夹名。

进入刚创建的插件目录，并用 git 下载 SyntaxHL:

    
    
    cd ./lib/js/modules/tinymce3.5.0/plugins
    git clone https://github.com/RichGuk/syntaxhl.git 
    

在 templates 目录下创建 SyntaxHL 插件的加载脚本文件：

    
    
    vi ./templates/_admin_tinymce_overrides_js.tpl

在 _admin_tinymce_overrides_js.tpl 里加上以下语句：

    
    
    tinyInit.plugins += ",syntaxhl"; 
    tinyInit.theme_advanced_buttons1 += ",|,syntaxhl";
    

PS：也可以直接在 /modules/mod_admin/lib/js/modules/tiny-init.js 文件上对 plugins 和
theme_advanced_buttons1 这两个变量进行修改。

OK，刷新一下页面重新进来，就会看到多出一个类似黄色笔头的图标。对！它就是 SyntaxHL 的功能图标，点击进去试用一下吧。

