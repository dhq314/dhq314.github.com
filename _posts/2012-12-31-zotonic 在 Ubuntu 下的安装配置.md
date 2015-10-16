---
layout: page
title: zotonic 在 Ubuntu 下的安装配置
---
{% include JB/setup %}

[zotonic](http://zotonic.com/)
是一个用erlang写的、高效、实时的WEB框架，也可以当作一个内容管理系统（CMS）用，[据说比一般的PHP
CMS要快10倍以上](http://zotonic.com/page/614/speed)，刚好工作上也用到 erlang，于是就有了把
wordpress 转为 zotonic 的念头。

zotonic 由以下开源项目搭建而成：

  * [Webmachine](https://github.com/basho/webmachine)：一种基于 REST 构建 WEB 应用程序的系统，zotonic 用它来处理 HTTP 协议请求
  * [MochiWeb](https://github.com/mochi/mochiweb)：一个用来构建轻量高效的HTTP应用程序的 Erlang 库
  * [Nitrogen](http://nitrogenproject.com/)：一个构建高效动态网站的 Erlang 框架
  * [epgsql](https://github.com/wg/epgsql)：PostgreSQL 的 Erlang 驱动
  * [Bootstrap](http://twitter.github.com/bootstrap/)：Twitter推出的一个用于前端开发的开源工具包
  * [ErlyDTL](https://github.com/evanmiller/erlydtl)：Django 模板语言Erlang的实现
  * [jQuery](http://jquery.com/)：一个快速、简单的 javascript 库
  * [gen_smtp](https://github.com/Vagabond/gen_smtp)：一个可以通过回调模块进行扩展的通用的 Erlang SMTP客户端和服务器端
  * [erlang-oauth](https://github.com/tim/erlang-oauth)：Oauth的 Erlang 库

由于 zotonic 依赖 [erlang](http://www.erlang.org/) 的运行环境，需要
[ImageMagick](http://www.imagemagick.org/) 来处理图片，并以
[postgres](http://www.postgresql.org/) 作为数据库，所以安装zotonic前需要先安装配置好以上东西，[具体安装条件可
以查看这里](http://zotonic.com/docs/latest/tutorials/preinstall.html)。

**安装erlang**

zotonic 需要erlang的版本至少是R14B03。在Ubuntu上安装 erlang
还是比较简单，一条命令就把erlang安装所需的GCC编译环境、Ncurses开发库、OpenSSL开发库、ODBC开发库、Java等依赖包一同安装好：

    
    
    sudo apt-get install erlang

Ubuntu12.10下默认的erlang版本是R15B01，如果需要安装最新的erlang版本，也可以下载最新版本源码安装。

首先安装一些所需的依赖包：

    
    
    sudo apt-get install build-essential gcc g++ make libncurses5-dev \
     openssl libssl-dev m4 unixodbc unixodbc-dev freeglut3-dev \ 
     libwxgtk2.8-dev xsltproc fop tk8.5 openjdk-6-jdk openjdk-6-dbg
    

最后下载最新源代码，编译安装：

    
    
    wget http://www.erlang.org/download/otp_src_R15B03-1.tar.gz
    tar xzf otp_src_R15B03-1.tar.gz
    cd otp_src_R15B03-1
    ./configure
    make
    sudo make install
    

**安装postgresql**
    
    
    sudo apt-get install postgresql

安装初始 postgres 数据库

    
    
    sudo su postgres
    psql
    CREATE USER zotonic WITH PASSWORD 'zotonic';
    CREATE DATABASE zotonic WITH OWNER = zotonic ENCODING = 'UTF8';
    GRANT ALL ON DATABASE zotonic TO zotonic;
    \c zotonic
    CREATE LANGUAGE "plpgsql";
    \q
    exit
    

**安装imagemagick**
    
    
    sudo apt-get install imagemagick

**安装 zotonic**

zotonic 的源代码可以用 git 在 [github](https://github.com/zotonic) 上面下载，

    
    
    git clone git://github.com/zotonic/zotonic.git

也可以在官方托管的 [google code](http://code.google.com/p/zotonic/) 下载，不过上面用 git
下的话默认是最新的开发版本。如果想体验最新的开发功能的话，可以直接用 git
下载。不过说了是最新的开发版，难免会遇到一些安装的问题（本人就刚好中枪，安装了一个开发版，结果页面的 CSS 显示不出来，去用户组反应，得到的反馈是[他们昨
天已经把这个BUG修复了](https://groups.google.com/forum/?fromgroups=#!topic/zotonic-
users/ScO81R1uGx0)...）。

把 zotonic 的源代码下载下来，解压放在一个你想放的目录下，进入zotonic 文件夹的根目录（这里的一些操作命令就不多说了），然后在终端上输入：

    
    
    make

编译 zotonic 的源代码。如果上面所说的安装条件都正确安装，一般都能成功编译。

成功编译完后，就可以启动 zotonic 了。在根目录下有个"start.sh"的文件，运行这个文件的话，就是以 DEBUG
模式启动zotonic（就是显示一个erlang shell），这个跟在bin目录下执行“bin/zotonic
debug”是一样，更多的操作命令说明，可以查看[这里](http://zotonic.com/docs/latest/manuals/cli.html)。

zotonic 运行起来后，可以在浏览器打开 "<http://127.0.0.1:8000/>"(默认访问端口是8000，由 /priv/config
文件里的 listen_port 值决定)，登陆zotonic的总后台，查看 zotonic 的一些运行状态数据，登陆所需的密码可以在
"priv/config" 文件上的password字段找到，同时，要修改登陆密码的话，修改这个字段就行。

其实，如果是类 [Debian](http://www.debian.org/) 系统（例如Ubuntu）可以直接运行 zotonic 根目录下的
zotonic_install，可以更快更方便的安装配置以上软件包。

**建立第一个zotonic网站**

进入bin目录，在终端下输入：

    
    
    ./zotonic addsite -s blog test

上面的“-s blog”是指网站使用的是blog的架构(skeleton)，“test”就是网站的名字。输入后会出现关于数据库和网站一些参数的确认数据，按回
车后网站的初始数据就建立好。

当然，也可以自己指定一些数据库和网站的一些参数，例如：

    
    
    ./zotonic addsite -s blog -h 127.0.0.1 -p 5432 -u zotonic -P zotonic -d zotonic -n public -a admin test

参数解释：

    
    
    "-h": 指数据库的主机 
    "-p": 指连接数据库的端口
    "-u": 指数据库的用户名
    "-P": 指该用户的密码
    "-d": 指调用的数据库名
    "-n": 指数据库使用的模式(schema)
    "-a": 指网站后台超级用户‘admin’的登陆密码。
    

以上参数都可以在文件“priv/sites/test/config”文件查看或者修改（每创建一个网站，都会在sites目录下创建一个以该网站名命名的文件夹）
。更多参数说明可以查看[这里](http://zotonic.com/docs/latest/manuals/cli.html)。

接着修改"/etc/hosts"文件，把网站的名字指定到本机上，在hosts文件添加下面一行：

    
    
    127.0.0.1 test

最后，登陆 zotonic 的总后台(http://127.0.0.1:8000/)，启动刚才建立的网站，在浏览器上输入
“http://test:8000/”，就可以打开查看新建立的网站，也可以打开 “http://test:8000/admin” 登陆网站的管理后台。

OK，zotonic 就在 Ubuntu 下安装配置完:)

