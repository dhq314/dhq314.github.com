---
layout: page
title: 在 Mac 上搭建可托管在 Github 的轻博客 -- Jekyll Bootstrap
---
{% include JB/setup %}

[Jekyll Bootstrap](http://jekyllbootstrap.com/) 是一个使用 twitter 的
[Bootstrap](http://twitter.github.io/bootstrap/) 作为其前端 CSS 框架的
[Jekyll](http://jekyllrb.com/) 主题。它允许你以 [markdown](http://dhq.me/markdown-
syntax) 的形式在编辑器上写作，有新文章发布到服务器上会自动转成静态的 HTML 文件，而且不用担心页面主题的制作，Bootstrap
已经为你解决了不同浏览终端的显示样式和兼容问题。

**安装 Ruby**

由于 Jekyll 是由 [ruby](http://www.ruby-lang.org/) 编写而成，所以需要先安装 ruby 开发环境。虽然 Mac
预装了 ruby，不过是已经不再维护的 1.8.7 老版本，同时运行 Jekyll 需要 1.9 以上版本，因此不得不重新装。这里使用 [RVM（Ruby
Version Manager）](https://rvm.io/)来安装，在终端输入一下命令一键安装 RVM：

    
    
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
    

由于 GFW 的原因，默认的 [RubyGems](http://rubygems.org/)
镜像上有些程序包是安装不了的，不过可以把默认的镜像地址改为淘宝的 RubyGems 镜像地址，这样可以畅通无阻安装你想安装的程序包：

    
    
    gem sources -a http://ruby.taobao.org/
    gem update --system
    

**安装 Jekyll Bootstrap**

在 Github 上创建一个存放你的 Github Pages 网站的仓库，这里把该仓库的名字命名为
[dhq314.github.com](http://dhq314.github.com/)。

把 Jekyll Bootstrap 的源代码拷贝到本地：

    
    
    git clone https://github.com/plusjade/jekyll-bootstrap.git dhq314.github.com
    cd dhq314.github.com
    

把原来的 git 源地址改为先前创建的仓库源地址：

    
    
    git remote set-url origin git@github.com:dhq314/dhq314.github.com.git
    

设置 Github 的 ssh 访问 key：

    
    
    ssh-keygen -t rsa -C "邮件名@youremail.com"
    

把生成的 id_rsa.pub 文件里的值拷贝到 Github 的 SSH Keys 配置页面那里（右上角的 Account setting - SSH
Keys - Add SSH Keys）。

SSH Keys 配置完成后，就可以发布到 Github 上去了：

    
    
    git add .
    git commit -m "push posts"
    git push -u origin master
    

成功发布后，过几分后就可以通过 <http://dhq314.github.io/> 访问最原始 Jekyll Bootstrap 页面。

**发布一篇文章**

在发布文章之前，可以先设置一下根目录下的 _config.yml 文件里的网站配置参数。

用 Jekyll Bootstrap 发布文章很简单，只要把写好的 markdown 文章文件放在根目录下的 _post 文件夹，然后提交到 Github
上就可能发布一遍文章。

PS：文章文件的命令是有格式的，一定要符合 "分类-年-月-日-文章标题.md" 的形式命名，这是由配置文件 _config.yml 里的参数
permalink 决定，默认的参数值是这样：

    
    
    permalink: /:categories/:year/:month/:day/:title 
    

例如在 _post 文件夹下创建一个名为 "2013-08-03-just-a-test.md" 的文件（"just-a-test"
是文章的标题），内容如下：

    
    
    ---
    layout: page
    title: just a test
    ---
    {% include JB/setup %}
    
    **以上是一些通用的格式数据，每个 markdown 文章文件都需要有这几行数据，其标明了文章是放在哪个菜单，文章的标题是上面**
    
    ## 以下才是写自己的 markdown 文章内容
    
    *just a test*
    
    

可以在本地启动 Jekyll 服务，预览所写 markdown 的页面效果，或是检查文章的内容有没有错误。在根目录下输入以下命令，启动 Jekyll 服务：

    
    
    jekyll server

启动完后，就可从打开 <http://localhost:4000/>，访问本地的 Jekyll Bootstrap。

文章写完后，本地也预览测试没问题了，git push 就可以直接发布到 Github 上。

