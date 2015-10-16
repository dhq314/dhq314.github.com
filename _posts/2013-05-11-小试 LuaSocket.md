---
layout: page
title: 小试 LuaSocket
---
{% include JB/setup %}

[LuaSocket](http://w3.impa.br/~diego/software/luasocket/) 是 Lua
的网络模块库，它可以很方便地提供 [TCP](http://zh.wikipedia.org/wiki/%E4%BC%A0%E8%BE%93%E6%8E%A
7%E5%88%B6%E5%8D%8F%E8%AE%AE)、[UDP](http://zh.wikipedia.org/wiki/%E7%94%A8%E6%
88%B7%E6%95%B0%E6%8D%AE%E6%8A%A5%E5%8D%8F%E8%AE%AE)、[DNS](http://zh.wikipedia.
org/wiki/%E5%9F%9F%E5%90%8D%E7%B3%BB%E7%BB%9F)、[FTP](https://zh.wikipedia.org/
wiki/%E6%96%87%E4%BB%B6%E4%BC%A0%E8%BE%93%E5%8D%8F%E8%AE%AE)、[HTTP](http://zh.
wikipedia.org/wiki/%E8%B6%85%E6%96%87%E6%9C%AC%E4%BC%A0%E8%BE%93%E5%8D%8F%E8%A
E%AE)、[SMTP](https://zh.wikipedia.org/wiki/%E7%AE%80%E5%8D%95%E9%82%AE%E4%BB%B
6%E4%BC%A0%E8%BE%93%E5%8D%8F%E8%AE%AE)、[MIME](https://zh.wikipedia.org/wiki/MI
ME) 等多种网络协议的访问操作。它由两部分组成：一部分是用 C 写的核心，提供对 TCP 和 UDP 传输层的访问支持。另外一部分是用 Lua
写的，负责应用功能的网络接口处理。

**安装 LuaSocket**

如果你安装有 Lua 模块的安装和部署工具 -- [LuaRocks](http://dhq.me/luarocks-a-deployment-and-
management-system-for-lua-modules)，那么一条指令就能安装部署好 LuaSocket：

    
    
    luarocks install luasocket
    

如果没安装有 LuaRocks，也可以源码安装。先把 LuaRocks 下载下来，当前可用的版本是 luasocket-2.0.2（在官方下载
LuaSocket 时报 404 错，还好有第三方托管能下）：

    
    
    wget http://pkgs.fedoraproject.org/repo/pkgs/lua-socket/luasocket-2.0.2.tar.gz/41445b138deb7bcfe97bff957503da8e/luasocket-2.0.2.tar.gz
    tar -zxvf ./luasocket-2.0.2.tar.gz
    cd luasocket-2.0.2
    

进入到 LuaSocket 的安装目录里面，在安装目录下有个 config 文件，它是 LuaSocket 的安装配置文件，用来设置一些自定义安装。

例如你想把 LuaSocket 模块的文件集中放在 "/data/lua/luasocket" 目录里，那么需要修改 config 文件里的
INSTALL_TOP_SHARE 和 INSTALL_TOP_LIB （这 2 个参数的值默认是 "/usr/local/share/lua/5.1" 和
"/usr/local/lib/lua/5.1"），修改之后如下：

    
    
    INSTALL_TOP_SHARE=/data/lua/luasocket/share    
    INSTALL_TOP_LIB=/data/lua/luasocket/lib  
    

还有一点是，Mac 跟 Linux 下的编译链接设置（Compiler and linker settings）是不一样的，如果你是 Mac 用户，需要在
config 配置文件里把 Mac 的编译链接设置打开（把行前 # 号去掉），关上 Linux 的（在行前加上 # 号）。

安装配置设置好后，就可以编译安装 LuaSocket：

    
    
    make && make install 
    

由于 LuaSocket 模块文件的存放位置不在系统模块加载路径范围内，因此需要修改 Lua 的环境变量 LUA_PATH 和 LUA_CPATH，来把
LuaSocket 的位置目录添加到系统模块的加载路径上。

在当前用户根目录下打开 .profile 文件（没有则创建，打开 .bashrc 文件也可以），加上以下内容：

    
    
    export LUA_PATH=/data/lua/luasocket/share/?.lua\;?.lua;;
    export LUA_CPATH=/data/lua/luasocket/lib/?.so\;?.so;;
    

最后，更新环境变量参数，LuaSocket 就安装完毕：

    
    
    source ~/.profile

**LuaSocket 使用**

使用 LuaSocket 很简单，直接用 require 函数加载进来就行，例如输出一个 LuaSocket 版本信息：

    
    
    local socket = require("socket")
    print(socket._VERSION)
    

模块 LuaSocket 内置的常量、函数的结构图如下：

    
    
     - sleep [function: 0x7feeeb40f940]
     - source [function: 0x7feeeb413570]
     - newtry [function: 0x7feeeb40f8c0]
     - _VERSION [LuaSocket 2.0.2]
     - connect [function: 0x7feeeb4122f0]
     - sink [function: 0x7feeeb410ea0]
     - __unload [function: 0x7feeeb4107e0]
     - bind [function: 0x7feeeb413380]
     - _M {.}
     - _DEBUG [true]
     - skip [function: 0x7feeeb4107b0]
     - dns - gethostname [function: 0x7feeeb410af0]
     |     - tohostname [function: 0x7feeeb410b20]
     |     - toip [function: 0x7feeeb410aa0]
     - gettime [function: 0x7feeeb40f8f0]
     - select [function: 0x7feeeb412290]
     - BLOCKSIZE [2048]
     - sinkt - default [function: 0x7feeeb410e20]
     |       - close-when-done [function: 0x7feeeb410dc0]
     |       - keep-open [function: 0x7feeeb410e20]
     - sourcet - by-length [function: 0x7feeeb410e50]
     |         - default [function: 0x7feeeb413440]
     |         - until-closed [function: 0x7feeeb413440]
     - tcp [function: 0x7feeeb412020]
     - _NAME [socket]
     - choose [function: 0x7feeeb410ce0]
     - try [function: 0x7feeeb410ca0]
     - protect [function: 0x7feeeb410760]
     - _PACKAGE []
     - udp [function: 0x7feeeb410fd0]
    

以 socket 的方式访问获取度娘首页数据：

    
    
    local socket = require("socket")
    
    local host = "www.baidu.com"
    local file = "/"
    
    -- 创建一个 TCP 连接，连接到 HTTP 连接的标准端口 -- 80 端口上
    local sock = assert(socket.connect(host, 80))
    sock:send("GET " .. file .. " HTTP/1.0\r\n\r\n")
    repeat
    	-- 以 1K 的字节块来接收数据，并把接收到字节块输出来
    	local chunk, status, partial = sock:receive(1024)
    	print(chunk or partial)
    until status ~= "closed"
    -- 关闭 TCP 连接
    sock:close()
    

或者使用模块里内置的 http 方法来访问：

    
    
    local http = require("socket.http")
    local response = http.request("http://www.baidu.com/")
    print(response)
    

**一个简单的 client/server 通信连接**

本来想写成单 server 多 client 的 socket 聊天服务器，不过最后还是卡在客户端的数据更新上，单进程的 while 轮询（poll），一个
[io.read](http://www.lua.org/manual/5.1/manual.html#pdf-io.read)
就把服务器数据接收给截断了。仅靠现有的 LuaSocket 模块不装其他第三方模块，也是很难做一个实时的聊天，虽然有 [socket.select](htt
p://w3.impa.br/~diego/software/luasocket/socket.html#select)
在苦苦支撑，但是这还是一个填不平的坑来了。

可能用上面向并发的 [concurrentlua](https://github.com/lefcha/concurrentlua)
模块会解决这个数据接收阻塞问题，这个以后再看看，现阶段的成果是：在客户端的终端上敲一些东西后回车会通过 socket
给服务器发送数据，服务器接收到数据后再返回显示在客户端的终端上。一个简单的东西，纯属练手，代码如下：

    
    
    -- server.lua
    local socket = require("socket")
    
    local host = "127.0.0.1"
    local port = "12345"
    local server = assert(socket.bind(host, port, 1024))
    server:settimeout(0)
    local client_tab = {}
    local conn_count = 0
    
    print("Server Start " .. host .. ":" .. port)  
    
    while 1 do
    	local conn = server:accept()
      	if conn then
      		conn_count = conn_count + 1
      		client_tab[conn_count] = conn 
         	print("A client successfully connect!")  
     	end
     
    	for conn_count, client in pairs(client_tab) do
    		local recvt, sendt, status = socket.select({client}, nil, 1)
    		if #recvt > 0 then
    	  		local receive, receive_status = client:receive()
    	  		if receive_status ~= "closed" then
    		      	if receive then
    		      		assert(client:send("Client " .. conn_count .. " Send : "))
    		      		assert(client:send(receive .. "\n"))
    		      		print("Receive Client " .. conn_count .. " : ", receive)    
    		      	end
    	      	else
    	      		table.remove(client_tab, conn_count)  
                	client:close()  
                	print("Client " .. conn_count .. " disconnect!")  
    	      	end
          	end
          	
    	end
    end
    
    
    
    -- client.lua
    local socket = require("socket")
    
    local host = "127.0.0.1"
    local port = 12345
    local sock = assert(socket.connect(host, port))
    sock:settimeout(0)
     
    print("Press enter after input something:")
    
    local input, recvt, sendt, status
    while true do
    	input = io.read()
    	if #input > 0 then
    		assert(sock:send(input .. "\n"))
    	end
    	
    	recvt, sendt, status = socket.select({sock}, nil, 1)
    	while #recvt > 0 do
    		local response, receive_status = sock:receive()
    		if receive_status ~= "closed" then
    		 	if response then 
    		  		print(response)
    		  		recvt, sendt, status = socket.select({sock}, nil, 1)
    		  	end
    	  	else 
    	  		break
    	  	end
      	end
    end
    

