---
layout: page
title: 利用 Luerl 在 Heroku 上搭建基于 Erlang 的 Lua Web Shell
---
{% include JB/setup %}

其实刚开始接触 Lua 时，就想着能不能也做一个像之前的 [Erlang Web Shell](http://dhq.me/deploy-erlang-
web-shell-with-webmachine-erlydtl-on-heroku) 那样方便调试 Erlang 代码的东西出来。一番了解后，Lua
里的 [loadstring](http://www.lua.org/manual/5.1/manual.html#pdf-loadstring) 函数和
[loadfile](http://www.lua.org/manual/5.1/manual.html#pdf-loadfile) 函数可以动态解释
Lua 的表达式，不过经过一些测试，发现需要的执行结果却很难返回来，例如
[print](http://www.lua.org/manual/5.2/manual.html#pdf-print)
函数输出的结果是捕获不到的，解析的结果返回不了给前端，那也没用。喜闻乐见的是，后来无意中发现了 Luerl 这么一个东西。

**What's Luerl**

[Luerl](https://github.com/rvirding/luerl) 是一个用纯 Erlang 写的 Lua 解释器，你可以在 Erlang
里执行符合 Lua 语法的表达式和调用其内置的模块函数方法，或者是执行调用一个外部的 Lua 文件。可以说，它纯粹是一个在 Erlang 里实现的 Lua
VM。

Luerl 的安装很简单，直接从 github 上载下来，然后到根目录 make 一下就行：

    
    
    git clone git://github.com/rvirding/luerl.git
    make
    

编译完后，就可以在终端里启动 Luerl：

    
    
    erl -pa ./ebin

Luerl 的接口方法都集中放在
[luerl.erl](https://github.com/rvirding/luerl/blob/master/src/luerl.erl)
这个文件里：例如解析一个 Lua 表达式：

    
    
    luerl:eval("print(\"Hello Luerl!\")").
    

调用解析一个 Lua 文件（例如当前目录有个名为 "hello_luerl.lua" 的文件）：

    
    
    luerl:evalfile("./hello_luerl.lua").
    

更多的例子可以查看 example 文件夹下的示例，在终端里执行 "make examples" 便可对这些例子进行编译测试。

Luerl 的代码也很简单，总共不足 2000 行的 Erlang 代码就实现一个 Lua VM，只能说其作者很 NB！

**部署到 Heroku 上**

对于这个 Lua Web Shell 的模型，可以打开上面导航 [LuaShell](http://dhq.me/luashell)
的链接先感观的认识下，或者点击下面代码框的运行按钮，动态解析执行下面的 Lua 代码（代码框里的 Lua 代码可编辑）：

    
    
    print("clink the button to run the lua code!")
    

其实这个 Lua Web Shell 是建立在之前的 [genfsm](http://dhq.me/heroku-deploy-erlang-web-
webmachine-mochiweb-erlydtl) 应用上，它跟之前的
[ErlShell](http://genfsm.herokuapp.com/erlshell/) 也差不多，都有以下这些相同特性：

  * LuaShell 是一个独立的进程，不依附在其他进程上，只负责解析 Lua 表达式字符串
  * 采用一种心跳包机制，客户端定时发心跳包信息，服务端的 LuaShelll 进程定时检测心跳包时间，心跳包超时则自动关闭 LuaShell
  * 数据与表现分离，每一个 LuaShell 的操作请求，服务端只返回 json 形式的数据

可以说，LuaShell 是在 ErlShell 的基础上搭建的，它的创建进程、关闭进程、心跳包机制的代码跟 ErlShell
的是一样，唯一改的就是解析的函数方法。因此，这里只重点介绍 Lua 代码的解析方法，像如何搭建 heroku 开发环境，如何在之前 genfsm
应用的基础上继续开发，如何使用 heroku 命令发布应用等问题这里就不赘述了，这些问题的答案可从下面这两篇文章获得：

  * [在 heroku 上部署 webmachine + mochiweb + erlydtl 组合的Erlang Web应用](http://dhq.me/heroku-deploy-erlang-web-webmachine-mochiweb-erlydtl)
  * [在 heroku 上部署基于 webmachine + erlydtl 的 erlang web shell -- ErlShell](http://dhq.me/deploy-erlang-web-shell-with-webmachine-erlydtl-on-heroku)

**Lua 表达式解析**

对于这个 LuaShell 的解析方法，直接调用
[luerl:do/2](https://github.com/rvirding/luerl/blob/master/src/luerl.erl#L64)
这个接口方法就行，它的用法描述如下：

    
    
    luerl:do(String|Binary|Form[, State]) -> {Result, NewState}.
    

luerl:do/2 跟上面的 [luerl:eval/1](https://github.com/rvirding/luerl/blob/master/s
rc/luerl.erl#L43) 方法不同的地方就是多了一个 State 参数，并最后会返回执行结果和一个 NewState。而这个 State
参数是一个名为 [luerl 的记录（record）](https://github.com/rvirding/luerl/blob/master/src/
luerl.hrl#L35)，它的值通过 [luerl:init/0](https://github.com/rvirding/luerl/blob/mas
ter/src/luerl.erl#L97) 初始获得，它保存着一些解析数据，例如一些已声明的 Lua 变量和函数，它跟 ErlShell 进程状态里的
bindings 字段一样，都是保存着已声明的变量和函数。

因此，把这个 State 的数据保存下来，就能把整个 Lua 的执行环境保存起来。同样，这个 State
的值也是保存在进程状态的记录里，修改后的进程状态记录如下：

    
    
    -record(state, {
        heart_time = 0,                     %% 心跳包时间
        heart_timer = undefined,            %% 检测心跳包的定时器 
        heart_time_interval = 10,           %% 心跳包的检测间隔
        lua_state = {},                     %% lua 状态信息
        line_num = 1                        %% 行号
    }).
    

LuaShell 进程启动的时，会初始这个进程状态记录的数据：

    
    
    init([HeartTimeInterval]) ->
        HeartTime = util:unixtime(),
    	%% 启动一个心跳包检测定时器
        HeartTimer = erlang:send_after(HeartTimeInterval * 1000, self(), 'DETECT_HEART'),
    	LuaState = luerl:init(),
        State = #state{
            heart_time = HeartTime,
            heart_timer = HeartTimer,
            heart_time_interval = HeartTimeInterval,
            lua_state = LuaState
        },
        {ok, State}.
    

**print 输出问题**

还有一个问题就是，Luerl 里的 print 输出是调用 Erlang 里的
[io:format/2](http://dhq.me/erlample/modules/io/format_2.html?search=io:)
函数来模拟输出操作的，就是说，Luerl 里的输出是不会返回获得的。

不过开源的东西就是你能够按照你的需求去修改。这里我的解决方案是，在 luerl 记录里加一个捕获输出的字段 print_str，在 [luerl_basic
:print/2](https://github.com/rvirding/luerl/blob/master/src/luerl_basic.erl#L1
85) 的方法（print 在 Luerl 里的执行方法）捕获输出内容。

修改后的 luerl 记录如下：

    
    
    -record(luerl, {tabs,free,next,			%Table structure
    		meta=[],			%Data type metatables
    		env,				%Environment
    		locf=false,			%Started local function
    		tag,				%Unique tag
            print_str = ""		% print string
    }).
    

修改后的 luerl_basic:print/2 方法如下：

    
    
    print(Args, St0) ->
    	{PrintList, St1} = lists:foldl(fun (A, {PL, S0}) ->
    				{Str, S1} = tostring([A], S0),
    			 	io:format("~s ", [Str]),
    				[Str1] = Str,
    				case is_binary(Str1) of
    					true ->
    						{PL ++ [binary_to_list(Str1)], S1};
    					false ->
    						{PL ++ [Str1], S1}
    				end
    		  	end, {[], St0}, Args),
        io:nl(),
    	PrintStr = string:join(PrintList, "    "),
        {[], St1#luerl{ print_str = PrintStr }}.
    

这样，就能通过 luerl:do/2 方法里返回的 NewState 来获取 Lua 里的输出。

不过这里我还是在 luerl.erl 里封装一个名为 evalua 的接口方法，免去在 LuaShell 的进程模块里引入
[luerl.hrl](https://github.com/rvirding/luerl/blob/master/src/luerl.hrl)
头文件。新添加的 luerl:evalua 方法如下：

    
    
    %% @doc 解析 Lua 表达式（LuaChunk），并返回一个 NewLuaState 和输出结果
    evalua(LuaChunk) ->
    	evalua(LuaChunk, init()).
    evalua(LuaChunk, LuaState) ->
    	try do(LuaChunk, LuaState) of
    		{_Ret, NewLuaState} ->
    			NewLuaState1 = NewLuaState#luerl{ print_str = "" },
    			case NewLuaState#luerl.print_str == "" orelse NewLuaState#luerl.print_str == [] of
    				false ->
    					{NewLuaState1, NewLuaState#luerl.print_str};
    				true ->
    					{NewLuaState1, ok}
    			end
        catch
      		_E:R -> {LuaState, R}
        end.
    

上面的 luerl:evalua 方法会返回一个新的 State 和执行输出结果，而新的 State 保存在进程状态字典上，输出结果则返回给前端。

至此，变动最大的 LuaShell 的解析方法函数就修改完成，其他部分跟 ErlShell 差不多。

由于修改了 Luerl 的一些代码，所以我自己在 github 上独立 [fork 了一个 Luerl
版本](https://github.com/dhq314/luerl)，作为这个 Lua Web Shell
的开发版，把需要修改的部分添加上去，以适应修改需求。对于 Luerl 如何嵌入之前的 genfsm 应用，很简单，直接作为 genfsm 的依赖，在
rebar.config 加上就行：

    
    
    %%-*- mode: erlang -*-
    {sub_dirs, ["rel"]}.
    {deps_dir, ["deps"]}.
    {erl_opts, [debug_info]}.
    
    {deps, [
                {webmachine, ".*", {git, "git://github.com/dhq314/webmachine.git", "master"}},
                {erlydtl, ".*", {git, "git://github.com/evanmiller/erlydtl.git", "master"}},
                {egeoip, ".*", {git, "git://github.com/mochi/egeoip.git", "master"}},
                {luerl, ".*", {git, "git://github.com/dhq314/luerl.git", "master"}}
            ]
    }.
    

最后，像之前发布 genfsm 应用那样 git push 到 heroku 上就收工搞完。惯例，完整的代码实现可从
[github](https://github.com/dhq314/heroku-genfsm) 上查看得到。

