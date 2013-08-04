---
layout: page
title: 在 Mac 上搭建可托管在 github 的轻博客 -- Jekyll Bootstrap
---
{% include JB/setup %}     

 [Jekyll Bootstrap][7] 是一个使用 twitter 的 [Bootstrap][8] 作为其前端 CSS 框架的 [Jekyll][9] 主题。它允许你以 [markdown][10] 的形式在编辑器上写作，有新文章发布到服务器上会自动转成静态的 HTML 文件，而且不用担心页面主题的制作，Bootstrap 已经为你解决了不同浏览终端的显示样式和兼容问题。

**安装 Ruby**

由于 Jekyll 是由 [ruby][11] 编写而成，所以需要先安装 ruby 开发环境。虽然 Mac 预装了 ruby，不过是已经不再维护的 1.8.7 老版本，同时运行 Jekyll 需要 1.9 以上版本，因此不得不重新装。这里使用 [RVM（Ruby Version Manager）][12]来安装，在终端输入一下命令一键安装 RVM：


    curl -L https://get.rvm.io | bash -s stable --rails --autolibs=enabled 
PS：以上命令需要用到 git，所以最好先安装 git。

命令执行完后，可以用以下查看可安装的 ruby 版本：


    rvm list 
这里选择安装 ruby-2.0.0-p247 这个版本：


    rvm install ruby-2.0.0-p247 
安装完后还需要指定使用哪个 ruby 版本才能正常利用到你要安装的版本，这里就指定上面安装的版本：


    rvm use ruby-2.0.0-p247 
上面的指定不是长久的默认指定，如果重新打开终端后还会复原回原来的状态，如果想长久默认使用当前版本，可以执行：


    rvm --default use ruby-2.0.0-p247 
由于 GFW 的原因，默认的 [RubyGems][13] 镜像上有些程序包是安装不了的，不过可以把默认的镜像地址改为淘宝的 RubyGems 镜像地址，这样可以畅通无阻安装你想安装的程序包：


    gem sources -a http://ruby.taobao.org/ gem update --system 
**安装 Jekyll Bootstrap**

在 github 上创建一个存放你的 Github Pages 网站的仓库，这里把该仓库的名字命名为 [dhq314.github.com][14]。

把 Jekyll Bootstrap 的源代码拷贝到本地：


    git clone https://github.com/plusjade/jekyll-bootstrap.git dhq314.github.com cd dhq314.github.com 
把原来的 git 源地址改为先前创建的仓库源地址：


    git remote set-url origin git@github.com:dhq314/dhq314.github.com.git 
设置 Github 的 ssh 访问 key：


    ssh-keygen -t rsa -C &quot;邮件名@youremail.com&quot; 
把产生的 id\_rsa.pub 文件里的值复制到 github 的 SSH Keys 配置那里（右上角的 Account setting - SSH Keys - Add SSH Keys）

SSH Keys 配置完成后，就可以发布到 github 上去了：


    git push origin master 
成功发布后，过几分后就可以通过 [http://dhq314.github.io/][15] 访问最原始 Jekyll Bootstrap 页面。

**发布一篇文章**

在发布文章之前，可以先设置一下根目录下的 \_config.yml 文件里的网站配置参数。

用 Jekyll Bootstrap 发布文章很简单，只要把写好的 markdown 文章文件放在根目录下的 \_post 文件夹，然后提交到 github 上就可能发布一遍文章。

PS：文章文件的命令是有格式的，一定要符合 &quot;分类-年-月-日-文章标题.md&quot; 的形式命名，这是由配置文件 \_config.yml 里的参数 permalink 决定，默认的参数值是这样：


    permalink: /:categories/:year/:month/:day/:title 
例如在 \_post 文件夹下创建一个名为 &quot;2013-08-03-just-a-test.md&quot; 的文件（&quot;just-a-test&quot; 是文章的标题），内容如下：


    --- 
    layout: page 
    title: just a test 
    --- 
    \{\% include JB/setup \%\} 
    
    \*\*以上是一些通用的格式数据，每个 markdown 文章文件都需要有这几行数据，其标明了文章是放在哪个菜单，文章的标题是上面\*\* 

    ## 以下才是写自己的 markdown 文章内容 
    
    \*just a test\* 

可以在本地启动 Jekyll 服务，预览所写 markdown 的页面效果，或是检查文章的内容有没有错误。在根目录下输入以下命令，启动 Jekyll 服务：


    jekyll server
启动完后，就可从打开 http://localhost:4000/，访问本地的 Jekyll Bootstrap。

文章写完后，本地也预览测试没问题了，就 git push 发布到 github 上吧:)

  [1]: /
  [2]: /about
  [3]: /erlshell
  [4]: /luashell
  [5]: /erlample
  [6]: /status
  [7]: http://jekyllbootstrap.com/
  [8]: http://twitter.github.io/bootstrap/
  [9]: http://jekyllrb.com/
  [10]: http://dhq.me/markdown-syntax
  [11]: http://www.ruby-lang.org/
  [12]: https://rvm.io/
  [13]: http://rubygems.org/
  [14]: http://dhq314.github.com/
  [15]: http://dhq314.github.io/
  [16]: /by_keyword/418/markdown
  [17]: /by_keyword/420/github
  [18]: /by_keyword/421/jekyll
  [19]: /markdown-syntax
  [20]: http://disqus.com/?ref_noscript
  [21]: http://disqus.com
  [22]: /archives/2013/8
  [23]: /archives/2013/7
  [24]: /archives/2013/6
  [25]: /archives/2013/5
  [26]: /archives/2013/4
  [27]: /archives/2013/3
  [28]: /archives/2013/2
  [29]: /archives/2013/1
  [30]: /archives/2012/12
  [31]: /by_keyword/391/awk
  [32]: /by_keyword/383/centos
  [33]: /by_keyword/345/dict
  [34]: /by_keyword/332/eaccelerator
  [35]: /by_keyword/344/erlang
  [36]: /by_keyword/367/erlydtl
  [37]: /by_keyword/384/fedora
  [38]: /by_keyword/329/ftp
  [39]: /by_keyword/371/heroku
  [40]: /by_keyword/351/homebrew
  [41]: /by_keyword/379/javascript
  [42]: /by_keyword/327/linux
  [43]: /by_keyword/397/lua
  [44]: /by_keyword/403/luarocks
  [45]: /by_keyword/405/luasocket
  [46]: /by_keyword/350/mac
  [47]: /by_keyword/339/memcache
  [48]: /by_keyword/366/mochiweb
  [49]: /by_keyword/356/mysql
  [50]: /by_keyword/355/nginx
  [51]: /by_keyword/381/node-js
  [52]: /by_keyword/331/php
  [53]: /by_keyword/336/php-fpm
  [54]: /by_keyword/394/postgresql
  [55]: /by_keyword/334/python
  [56]: /by_keyword/369/rebar
  [57]: /by_keyword/342/sed
  [58]: /by_keyword/341/shell
  [59]: /by_keyword/337/spawn-fcgi
  [60]: /by_keyword/373/ssh
  [61]: /by_keyword/358/syntaxhighlighter
  [62]: /by_keyword/328/ubuntu
  [63]: /by_keyword/388/vpn
  [64]: /by_keyword/365/webmachine
  [65]: /by_keyword/416/websocket
  [66]: /by_keyword/386/wordpress
  [67]: /by_keyword/375/xml
  [68]: /by_keyword/353/zotonic
  [69]: http://dhq.me/install-jekyll-bootstrap-on-mac
  [70]: http://dhq.me/the-interaction-experiment-between-erlang-and-websocket
  [71]: http://dhq.me/an-implementation-of-lua-in-erlang-luerl
  [72]: http://dhq.me/lua-learning-notes-string
  [73]: http://dhq.me/lua-learning-notes-c-api
  [74]: http://dhq.me/lua-learning-notes-function
  [75]: http://dhq.me/lua-learning-notes-object-oriented-programming
  [76]: http://dhq.me/lua-learning-notes-metatable-metamethod
  [77]: http://dhq.me/lua-learning-notes-table
  [78]: http://zotonic.com
  [79]: http://dhq.me/sitemap.html
  [80]: http://www.dailybuilding.com/
