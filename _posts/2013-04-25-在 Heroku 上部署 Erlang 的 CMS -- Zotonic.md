---
layout: page
title: 在 Heroku 上部署 Erlang 的 CMS -- Zotonic
---
{% include JB/setup %}

对于 [Zotonic](http://dhq.me/unbuntu-install-zotonic) 部署的环境,
[Heroku](https://www.heroku.com/) 是最合适不过了，提供免费的 Erlang 环境，还有对 PostgreSQL
数据库的支持，市面上是鲜有这种云服务平台。这得益于 Heroku 的
[Cedar](https://devcenter.heroku.com/articles/cedar) 平台架构，让 WEB
应用程序运行在任何语言的运行时栈（[runtime
stack](https://devcenter.heroku.com/articles/stack)）成为了可能。 其实之前在 twitter
上也有人提过在 Heroku 上部署 Zotonic 的问题，不过官方给出的回复是觉得 Zotonic 的架构不适合在 Heroku 上部署，还是建议在
VPS 上独立搭建比较好些，也许官方没时间做这种移植吧（前段时间人手不足，在讨论组里发广告招一个开发者进核心开发团队）。

不过有趣的事总会激发人去做，一位来自法国的 Zotonic 开发者 -- [Eric](http://eric.cestari.info/)，就做了一个
Zotonic 在 Heroku 上的[构建包（buildpack）](https://github.com/cstar/heroku-buildpack-
zotonic)，成功地把 Zotonic 移植到 Heroku 上。出于好奇，我也去看了下构建包的使用说明，不过要吐槽下构建包的介绍
[README](https://github.com/cstar/heroku-buildpack-
zotonic/blob/master/README.md) 写得相当简单，很多安装细节都省略了，不折腾还不给你搭起来了。这里先奉上 demo
地址：<http://genevent.herokuapp.com/>，下面就是这个折腾过程的大概记录：

**安装配置 Heroku 的开发环境**

其实之前《[在 heroku 上部署 webmachine + mochiweb + erlydtl 组合的Erlang
Web应用](http://dhq.me/heroku-deploy-erlang-web-webmachine-mochiweb-
erlydtl)》也提到过 heroku 开发环境的配置，这里就不赘述了，无非就是：

  * [注册](https://id.heroku.com/signup/devcenter) heroku 账号
  * 下载安装开发工具 -- [Heroku Toolbelt](https://toolbelt.heroku.com/)
  * 在本地终端里登陆账号
  * 上传 ssh publickey

详细的配置细节可以查看之前的介绍，也可以参看官方的配置指南 -- 《[Getting Started with
Heroku](https://devcenter.heroku.com/articles/quickstart)》。

**部署 zotonic**

打开到一个你想部署应用的目录，初始一个 git 版本

    
    
    git init

从这个 zotonic 构建包的[编译安装文件](https://github.com/cstar/heroku-buildpack-
zotonic/blob/master/bin/compile#L71)可以知道，该构建包需要提交的文件是 zotonic
网站文件（假如你之前创建了一个名为 site 的网站，那么需要提交的是 priv/sites/site 下的所有文件和文件夹）。

如果你安装了 zotonic ，可以创建一个网站（下面指令是在 zotonic 根目录下的 bin 文件夹里执行）：

    
    
    ./zotonic addsite -s blog site

site 就是网站名，创建好了之后把 priv/sites/site 下的文件和文件夹复制到上面所说的应用部署目录里。

如果没安装有 zotonic，或者觉得上面操作也很麻烦，并且觉得该网站的主题样式还能入眼，也可以拷贝托管在
[github](https://github.com/dhq314/zotonic-site) 上的该网站主题样式的 zotonic
网站文件，执行以下指令，把网站文件拷贝下来：

    
    
    git clone git://github.com/dhq314/zotonic-site.git && rm -rf zotonic-site/.git && cp -r zotonic-site/* ./ && rm -rf zotonic-site/

接着创建一个名为 genevent 的 heroku 应用：

    
    
    heroku create genevent -s cedar

设置该应用的 erlang 构建包：

    
    
    heroku config:add BUILDPACK_URL="https://github.com/cstar/heroku-buildpack-zotonic.git"

把应用的时区时间改为北京时间：

    
    
    heroku config:add TZ="Asia/Shanghai"

设置一个名为 ADMIN_PASSWORD 的环境变量，该应用初始安装时，会把这个环境变量值作为你用管理员账户（admin）登陆后台（<http://gen
event.herokuapp.com/admin/>）时的密码，具体详看[这里](https://github.com/cstar/heroku-
buildpack-zotonic/blob/master/opt/zotonic_config#L7)：

    
    
    heroku config:set ADMIN_PASSWORD=password

选择一个 Erlang/OTP 发行版本，应用选择哪个 Erlang/OTP 发行版本是由一个名为 .preferred_otp_version
的文件来决定，现在可支持的版本有：

  * master (R15B02 pre)
  * master-pu (R16B pre)
  * OTP_R15B
  * OTP_R15B01
  * OTP_R15B02

这里选择的是 OTP_R15B02（当前 zotonic 0.9.1 版本还不支持 Erlang R16B）：

    
    
    echo OTP_R15B02 > .preferred_otp_version

设置网站访问的域名，修改根目录下的 config 文件里的 hostname 和 hostalias 的值，改成网站的访问域名，heroku
给的访问域名一般是 "http://你的应用名.herokuapp.com/"，所以该应用的访问域名是
<http://genevent.herokuapp.com/>，修改后的值如下：

    
    
    %% Hostname on which this site runs
    {hostname, "genevent.herokuapp.com"},
    
    %% Aliases which should redirect to the primary hostname
    {hostalias, "genevent.herokuapp.com"},
    

添加 [Heroku PostgreSQL](https://devcenter.heroku.com/articles/heroku-
postgresql) 数据库，这里使用免费的开发者版（只允许1万行数据，20个并发连接）：

    
    
    heroku addons:add heroku-postgresql:dev

数据库添加好之后，一般会在终端里显示类似以下的信息：

    
    
    Adding heroku-postgresql:dev on genevent... done, v4 (free)
    Attached as HEROKU_POSTGRESQL_PURPLE_URL
    Database has been created and is available
     ! This database is empty. If upgrading, you can transfer
     ! data from another database with pgbackups:restore.
    Use `heroku addons:docs heroku-postgresql:dev` to view documentation.
    

上面显示数据库连上的地址是 HEROKU_POSTGRESQL_PURPLE_URL，接着调用 heroku pg:promote，设置
DATABASE_URL 环境变量（初始安装时需要，详看[这里](https://github.com/cstar/heroku-buildpack-
zotonic/blob/master/opt/zotonic_config#L6)）:

    
    
    heroku pg:promote HEROKU_POSTGRESQL_PURPLE_URL

上面的 HEROKU_POSTGRESQL_PURPLE_URL 数据库地址是随机分配的，因此 pg:promote 设置的值以添加完数据库后终端显示的
"Attached as" 后面的值为准

这时在终端输入 "heroku config" 查看数据库配置信息，一般会出现类似以下格式的 PostgreSQL 数据库连接信息：

    
    
    postgres://uytmqpepiztefb:UmafUnaw8PheEtN6Ox7MQ6Yc6h@ec2-54-225-84-29.compute-1.amazonaws.com:5432/df0tgat4uvbc47
    

而 PostgreSQL 数据库连接命名约定是这种格式：

    
    
    postgres://username:password@host:port/dbname
    

因此，连接 PostgreSQL 数据库所需要的用户名、密码、数据库名、主机名、端口都可以匹配上面得到，把以上数据库连接信息替换 config
文件上原来的数据库连接信息，替换后如下：

    
    
    %% PostgreSQL database connection
    {dbhost, "ec2-54-225-84-29.compute-1.amazonaws.com"},
    {dbport, 5432},
    {dbuser, "uytmqpepiztefb"},
    {dbpassword, "UmafUnaw8PheEtN6Ox7MQ6Yc6h"},
    {dbdatabase, "df0tgat4uvbc47"},
    {dbschema, "public"},
    

数据库配置完后，就可以把当前部署目录里的文件和文件夹添加到版本里，并 git push 发布到远程 heroku 上：

    
    
        git add .preferred_otp_version config controllers/ dispatch/ lib/ modules/ resources/ site.erl templates/ translations/
        git commit -m "deploy zotonic on heroku"
        git push heroku master
    

上面命令会把本地应用的代码提交到远程 heroku 服务器上，并编译代码，启动应用 genevent。当在终端里出现
"http://genevent.herokuapp.com deployed to Heroku" 的字样时，则 zotonic 在 heroku
上搭建成功，可以立即在浏览器里打开 <http://genevent.herokuapp.com> 访问了。

**一些说明**

**激活“站点地图”模块**

默认后台的站点地图模块 -- HTML Sitemap
是没有激活打开的，登陆后台（<http://genevent.herokuapp.com/admin/>），打开 System - Modules，找到名为
"HTML Sitemap" 的模块，点击最右边的 "Activate" 按钮就行。

如果需要代码高亮，可以像上面那样激活 SyntaxHighlighter 模块，该代码高亮模块的用法可以参考这篇《[安装 zotonic 代码语法高亮模块
-- mod_syntaxhighlighter](http://dhq.me/zotonic-install-
mod_syntaxhighlighter)》文章。

**站内搜索修改**

右上角的站内搜索使用的是 [google
自定义搜索引擎](http://www.google.com/cse/)，因此你想站内搜索能搜到你新创网站的内容，你需要去创建一个指向你网站地址的
google 自定义搜索引擎，创建好之后，把你的搜索引擎ID（Search engine ID）替换 templates/_sidebar.tpl
文件里站内搜索代码（如下代码）里的原来默认的搜索引擎ID就行。

    
    
    <form action="http://{{ m.config.site.hostalias.value }}/search" id="cse-search-box">
        <div id="dhq_search">
            <input type="hidden" name="cx" value="这里填你的自定义搜索引擎ID" />
            <input type="hidden" name="cof" value="FORID:10" />
            <input type="hidden" name="ie" value="UTF-8" />
            <input id="s_text" type="text" name="q" size="31" />
            <input id="s_submit" type="submit" name="sa" value="搜 索" />
        </div>
    </form>
    

**DISQUS 评论修改**

跟站内搜索一样，网站的评论也是使用第三方管理工具 -- [DISQUS](http://disqus.com/)，因此你需要创建一个 DISQUS
账号来获取一个网站评论管理的 disqus_shortname，然后替换 templates/article.tpl 文件里 DISQUS
评论代码（如下代码）原来的 disqus_shortname。

    
    
    <div id="disqus_thread"></div>
    <script type="text/javascript">
        /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
        var disqus_shortname = '这里填你的 DISQUS 评论管理 disqus_shortname'; // required: replace example with your forum shortname
    
        /* * * DON'T EDIT BELOW THIS LINE * * */
        (function() {
            var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
            dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
            (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        })();
    </script>
    <noscript>Please enable JavaScript to view the <a href="http://disqus.com/?ref_noscript">comments powered by Disqus.</a></noscript>
    <a href="http://disqus.com" class="dsq-brlink">comments powered by <span class="logo-disqus">Disqus</span></a>

