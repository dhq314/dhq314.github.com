---
layout: page
title: Lua 的模块安装和部署工具 - LuaRocks
---
{% include JB/setup %}

Lua 是一个短小精悍的脚本语言，很多功能官方都不内置支持，需要添加第三方模块库来支持。但是，这些第三方模块是由不同开发者制作，分散的放在不同地方，如果想要
安装一个模块还要到处去找，况且下载完模块后，有时还需要手动编译，设置环境变量，把模块放到指定的 Lua 模块加载目录。反正，安装一个第三方 Lua
模块需要执行上述一连串操作，也是一个繁琐的过程。

对于这些繁琐，[LuaRocks](http://www.luarocks.org/) 可以简化以上安装 Lua 模块的操作。 LuaRocks 是 Lua
模块的安装和部署工具，类似于 [Ruby](http://www.ruby-lang.org/) 的
[gem](http://rubygems.org/)，[Python](http://www.python.org/) 的
[egg](http://wiki.python.org/moin/egg) 和 [Perl](http://www.perl.org/) 的
[cpan](http://www.cpan.org/)，它可以很方便的安装第三方 Lua
模块，而且你不需要关心模块之间的依赖关系，一条命令就可以很轻松地把模块安装部署好，省心，省力，不用去折腾安装 Lua 模块的细节。

**安装 LuaRocks**

喜闻乐见的是，Mac 下的 [Homebrew](http://dhq.me/mac-apt-get-homebrew) 居然内置了 LuaRocks
的安装包（之前安装 Lua，用 "brew search lua" 搜 Lua 安装包时无意间发现），因此，在 Mac 下安装 LuaRocks
很简单，一条指令就行：

    
    
    brew install luarocks -v
    

用 Homebrew 安装 LuaRocks 的版本是
luarocks-2.0.12，不过当前最新[发布的版本](http://luarocks.org/releases/)是
luarocks-2.0.13，如果你想安装最新版，或者是非 Mac 用户，也可以源代码自定义安装 LuaRocks：

    
    
    wget http://luarocks.org/releases/luarocks-2.0.13.tar.gz
    cd luarocks-2.0.13
    ./configure --prefix=/usr/local/luarocks/ --rocks-tree=/usr/local --sysconfdir=/usr/local/etc/luarocks
    make
    make install
    

"rocks-tree" 是指所要安装的 Lua 模块的默认安装目录，"sysconfdir" 是指 LuaRocks 的配置文件存放的地方，更多的安装设置
参数可以查看[这里](http://www.luarocks.org/en/Installation_instructions_for_Unix)。

如果是在 Win 下安装 LuaRocks，可以参考官方的这篇介绍 -- [《Installation instructions for
Windows》](http://www.luarocks.org/en/Installation_instructions_for_Windows)

**使用 LuaRocks**

LuaRocks 也不是什么模块都能安装得到，它只汇集了一些比较出名的常用的第三方 Lua 模块。它有一个可安装的 [Lua
模块列表](http://luarocks.org/repositories/rocks/)，如果被安装的 Lua 模块在这个可安装列表里，那么就可以用
LuaRocks 来安装部署。

它的用法跟 Homebrew 差不多，例如想安装一个解析 [JSON(JavaScript Object
Notation)](http://www.json.org/) 的模块，可以用 search 参数先搜索一下有什么可安装的解析 JSON 的模块：

    
    
    luarocks search json
    

假设想安装一个名为 [json4lua](http://json.luaforge.net/) 模块，可以用 install 参数来安装：

    
    
    luarocks install json4lua
    

上面指令执行完后，模块就安装好了。可以写个 JSON 字符串转 Lua table 的小例子来测试下模块是否安装成功

    
    
    local json = require("json")
    
    local json_str = '{"key1" : "val1", "key2" : "val2", "key3" : "val3"}'
    local json_tab = json.decode(json_str)
    for k, v in pairs(json_tab) do
    	print(k, v)
    end
    --输出
    --key1	val1
    --key3	val3
    --key2	val2
    
    local tab = {a = "b", "c", 123, d = 456}
    io.write(json.encode(tab) .. "\n")
    --输出
    --{"1":"c","2":123,"a":"b","d":456}
    

把上面代码保存在一个 lua 文件里执行，如果运行无错，有数据在终端里输出，则模块安装成功。

如果想查看 json4lua 模块的信息，可以调用 show 参数：

    
    
    luarocks show json4lua
    

或者查看已安装模块的列表

    
    
    luarocks list
    

如果想卸载 json4lua 这个模块，可以用 remove 参数来操作：

    
    
    luarocks remove json4lua
    

更多的命令参数用法可以查看：

    
    
    luarocks --help
    

