---
layout: page
title: Mac 系统下类似于 apt-get 的软件包管理器 -- Homebrew
---
{% include JB/setup %}

对于一个习惯了在 Ubuntu 的终端上通过 apt-get 来安装工具软件的我来说，也希望在Mac上找到类似的工具，能很方便的一条命令就能安装所需的软件，
而不用手工的去查找下载编译，或者是折腾安装所需的一些依赖关系。很惊喜发现，Mac上也有类似的工具软件包管理器 --
[Homebrew](http://mxcl.github.com/homebrew/)。

Homebrew 并不是什么软件包都能装，它只是能装一些系统缺省的软件包，例如：wget、nginx、mysql等等。不过随着
[homebrew0.9版](https://github.com/mxcl/homebrew/wiki/Homebrew-0.9) 新加了 tap
操作，支持安装第三方数据源的软件包，以后支持安装的软件包会更多、更丰富。

**Homebrew安装**

安装 Homebrew 很简单，只需在终端上输入一行 [Ruby](https://www.ruby-lang.org/) 脚本（所以要先搭建 Ruby
运行环境，Mac 下已经预装了 Ruby）就行：

    
    
    ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
    

**Homebrew使用**

Homebrew 跟 Ubuntu 的里的 apt-get
一样，update是更新软件包列表，install是安装软件包，uninstall是卸载软件包，用法都差不多，熟悉 apt-get 使用的话是很容易上手。

下面是 Homebrew 的一些常用操作（package替换成你想安装的软件名，例如：nginx、mysql，就能正常安装你想要安装的软件）：

显示帮助信息

    
    
    brew -h

Homebrew的版本

    
    
    brew -v

列出Homebrew的建议或警告信息

    
    
    brew doctor

列出已安装的软件包

    
    
    brew list 

更新Homebrew软件包

    
    
    brew update(up)

用浏览器打开package主页（package 为空则打开 Homebrew 主页）

    
    
    brew home package

显示软件包内容信息

    
    
    brew info package

显示包依赖

    
    
    brew deps package

查找有没有想要安装的软件包（支持模糊查找）

    
    
    brew search package

查看软件包的信息

    
    
    brew info package

软件包的安装选项

    
    
    brew options package

安装软件包

    
    
    brew install package

如果想查看安装过程中执行的命令或者是编译信息，可以在 "install" 后面加一个 "-v" 参数：

    
    
    brew install -v package

卸载软件包

    
    
    brew uninstall(remove) package 

用 Homebrew 安装第三方工具软件包，例如用 homebrew 安装官方缺省的php

    
    
    brew tap josegonzalez/php

如果软件包出了新版本，可以用 upgrade 更新过时的软件包（缺省 package 参数，则为全部更新）：

    
    
    brew upgrade package

清理之前安装的旧版本数据：

    
    
    brew cleanup --force -s
    rm -rf $(brew --cache)
    

更多详细的用法说明可以在终端输入"man brew"查看。

PS：Mac 下类似 Homebrew 的软件管理工具还有
[MacPorts](http://www.macports.org/)、[Fink](http://fink.thetis.ig42.org/)。

Homebrew 的一些配置目录：

  * /Library/Caches/Homebrew: 安装所下载文件的存放的位置

