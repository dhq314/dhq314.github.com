---
layout: page
title: 用 rebar 来构建、编译、测试、发布 Erlang 应用程序
---
{% include JB/setup %}

[rebar](https://github.com/rebar/rebar) 是一个遵循 Erlang/OTP 原则的 Erlang
项目构建工具，使用它可以减少构建标准 Erlang/OTP 项目架构配置的工作量，并且可以很容易的编译、测试、发布 Erlang
应用程序。更强大的是，rebar 提供一种依赖管理机制，它可以使开发者很方便地通过 [Git](http://git-
scm.com/)、[Hg](http://mercurial.selenic.com/) 等方式重用常见的第三方 Erlang 模块或库。

**安装 rebar**

你可以从 <https://github.com/rebar/rebar/wiki/rebar> 下载编译好的版本，也可以自己下载 rebar
的源代码，自己编译一个：

    
    
    git clone git://github.com/rebar/rebar.git
    cd rebar
    ./bootstrap
    

上面编译好之后，在当前目录下就会生成一个名为 "rebar" 独立的 erlang 脚本(escript)，把它放在你想创建标准 Erlang/OTP
项目的目录路径下即可使用，或者把 rebar 放在系统目录的 Path 下，方便在终端使用：

    
    
    sudo mv rebar /usr/local/bin

在终端输入 "rebar -c" 将列出所有可执行的 rebar 命令。或者输入 "rebar -h" 查看更多的 rebar 参数信息。

**用 rebar 构建项目**

创建一个名为 rebarapp 的文件夹

    
    
    mkdir rebarapp
    cd rebarapp
    

创建名为 rebarapp 项目：

    
    
    rebar create-app appid=rebarapp

rebar 会根据默认模板（template）在当前目录下生成一个 src 文件夹，里面包含下面3个文件：

  * rebarapp.app.src 应用的资源描述文件，影响后面编译生成的 rebarapp.app 里的内容
  * rebarapp_app.erl 应用的 Application Behaviour 代码文件
  * rebarapp_sup.erl 应用的 Supervisor Behaviour 代码文件

rebar 还内置了 [gen_server](http://www.erlang.org/doc/design_principles/gen_server
_concepts.html)、[gen_fsm](http://www.erlang.org/doc/design_principles/fsm.html
)、[application](http://www.erlang.org/doc/design_principles/applications.html)
等 Erlang/OTP 行为模式的模板，可以自动生成这些行为模式的框架代码。这里以 gen_server 为例，给应用添加一个名为
rebarapp_server 的 gen_server 行为模式。在应用根目录执行以下命令：

    
    
    rebar create template=simplesrv srvid=rebarapp_server

执行完后自动会在 src 文件夹里生成一个 rebarapp_server.erl 的 gen_server 框架格式的文件，simplesrv 是
gen_server 模板的名称(gen_fsm、application对应的是simplefsm、simpleapp)，srvid 则是该
gen_server 模板的ID（gen_fsm、application对应的是fsmid、appid）。

为了测试，这里对 rebarapp_server.erl 进行修改，export 一个 hello 方法，并添加一个 cast 的消息输出，修改后的
rebarapp_server.erl 文件内容如下：

    
    
    -module(rebarapp_server).
    -behaviour(gen_server).
    -define(SERVER, ?MODULE).
    
    %% ------------------------------------------------------------------
    %% API Function Exports
    %% ------------------------------------------------------------------
    
    -export([start_link/0, hello/0]).
    
    %% ------------------------------------------------------------------
    %% gen_server Function Exports
    %% ------------------------------------------------------------------
    
    -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
             terminate/2, code_change/3]).
    
    %% ------------------------------------------------------------------
    %% API Function Definitions
    %% ------------------------------------------------------------------
    
    start_link() ->
        gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    
    %% @doc just a test
    hello() ->
        gen_server:cast(?SERVER, 'HELLO').
    
    %% ------------------------------------------------------------------
    %% gen_server Function Definitions
    %% ------------------------------------------------------------------
    
    init(Args) ->
        {ok, Args}.
    
    handle_call(_Request, _From, State) ->
        {reply, ok, State}.
    
    handle_cast('HELLO', State) ->
        io:format("Hello World!~n"),
        {noreply, State};
    
    handle_cast(_Msg, State) ->
        {noreply, State}.
    
    handle_info(_Info, State) ->
        {noreply, State}.
    
    terminate(_Reason, _State) ->
        ok.
    
    code_change(_OldVsn, State, _Extra) ->
        {ok, State}.
    
    %% ------------------------------------------------------------------
    %% Internal Function Definitions
    %% ------------------------------------------------------------------
    

修改 rebarapp_sup.erl 的 init 函数，把 rebarapp_server 作为应用管理者 rebarapp_sup
的工作进程启动，修改如下：

    
    
    init([]) ->
        RebarappServer = ?CHILD(rebarapp_server, worker),
        {ok, { {one_for_one, 5, 10}, [RebarappServer]} }.
    

**编译应用**
    
    
    rebar compile

编译完后，会在根目录下生成一个 ebin 的文件夹，里面存放的是该应用的资源文件 rebarapp.app 和应用的 beam
文件，也可以执行以下命令对编译生成的应用文件进行清理：

    
    
    rebar clean

**使用 Edoc 生成应用文档**
    
    
    rebar doc

命令执行完后，会在根目录生成一个 doc 的文件夹，打开里面的 index.html 就可以很直观地看到该应用的模块 API 概览。

**eunit 测试**

rebar 会根据一个名为 rebar.config 的文件里的
[eunit](http://www.erlang.org/doc/man/eunit.html) 配置选项来对应用进行测试，rebar.config
详细地配置选项信息可以查看官方上的 [rebar.config.sample](https://github.com/basho/rebar/blob/ma
ster/rebar.config.sample)。在应用的根目录下创建一个 rebar.config，填入以下内容：

    
    
    %%-*- mode: erlang -*-
    
    %% Erlang compiler options
    {erl_opts, [debug_info,
                {i, "test"},
                {src_dirs, ["src"]}]}.
    
    {eunit_opts, [verbose, {report, {eunit_surefire, [{dir, "."}]}}]}.
    
    {cover_enabled, true}.
    

上面的配置将会加载根目录下的 test 文件夹里的文件，所以需要在根目录下创建一个 test 文件夹：

    
    
    mkdir -p test

这里 test 文件夹将存放 eunit 的测试用例，在 test 文件夹里新建一个名为 rebarapp_test.hrl 的测试用例文件，内容如下：

    
    
    -include_lib("eunit/include/eunit.hrl").
    
    
    my_test() ->
        ?assert(1 + 2 =:= 3).
    
    simple_test() ->
        ok = application:start(rebarapp),
        ?assertNot(undefined =:= whereis(rebarapp_sup)).
    

然后在 rebarapp_server.erl 的文件末尾加上以下测试代码：

    
    
    -ifdef(TEST).
    -include("rebarapp_test.hrl").
    -endif.
    

当然，如果有必要的话也可以在每个模块文件上加上面测试代码。执行以下命令进行 eunit 测试：

    
    
    rebar compile eunit

如果应用文件没什么变化修改，也可以直接运行 "rebar eunit"。这时终端出现以下类似显示，则 eunit 测试完成：

    
    
    ==> rebarapp (eunit)
    ======================== EUnit ========================
    module 'rebarapp_app'
    module 'rebarapp_server'
      rebarapp_server: my_test...ok
      rebarapp_server: simple_test...[0.014 s] ok
      [done in 0.019 s]
    module 'rebarapp_sup'
    =======================================================
      All 2 tests passed.
    Cover analysis: /Users/dengjoe/erlang/rebarapp/.eunit/index.html
    

可以打开根目录下的.eunit/index.html 查看测试报告。

**发布应用**

在应用根目录下创建一个名为 rel 的文件夹，用来作为应用发布的文件夹：

    
    
    mkdir -p rel
    cd rel
    

在当前 rel 文件夹里创建一个名为 rebarapp 的独立的 Erlang VM 节点：

    
    
    rebar create-node nodeid=rebarapp

修改 rel/reltool.config 里的 lib_dirs 的值，默认是一个空列表 "[]"，改为应用所在的目录路径
'["../../"]'，不然到后面编译发布时会报 "Missing application directory" 的错误出来，修改后的
reltool.config 配置内容如下所示：

    
    
    {sys, [
           {lib_dirs, ["../../"]},
           {erts, [{mod_cond, derived}, {app_file, strip}]},
           {app_file, strip},
           {rel, "rebarapp", "1",
            [
             kernel,
             stdlib,
             sasl,
             rebarapp
            ]},
           {rel, "start_clean", "",
            [
             kernel,
             stdlib
            ]},
           {boot_rel, "rebarapp"},
           {profile, embedded},
           {incl_cond, derived},
           {mod_cond, derived},
           {excl_archive_filters, [".*"]}, %% Do not archive built libs
           {excl_sys_filters, ["^bin/.*", "^erts.*/bin/(dialyzer|typer)",
                               "^erts.*/(doc|info|include|lib|man|src)"]},
           {excl_app_filters, ["\.gitignore"]},
           {app, rebarapp, [{mod_cond, app}, {incl_cond, include}]}
          ]}.
    
    {target_dir, "rebarapp"}.
    
    {overlay, [
               {mkdir, "log/sasl"},
               {copy, "files/erl", "\{\{erts_vsn\}\}/bin/erl"},
               {copy, "files/nodetool", "\{\{erts_vsn\}\}/bin/nodetool"},
               {copy, "files/rebarapp", "bin/rebarapp"},
               {copy, "files/rebarapp.cmd", "bin/rebarapp.cmd"},
               {copy, "files/start_erl.cmd", "bin/start_erl.cmd"},
               {copy, "files/install_upgrade.escript", "bin/install_upgrade.escript"},
               {copy, "files/sys.config", "releases/\{\{rel_vsn\}\}/sys.config"},
               {copy, "files/vm.args", "releases/\{\{rel_vsn\}\}/vm.args"}
              ]}.
    

返回应用的根目录，在 rebar.config 加上以下一行，把新建的 rel 文件夹放入到 rebar 可访问的子文件夹里，作为应用内容发布文件夹：

    
    
    {sub_dirs, ["rel"]}.

再重新编译下应用 rebarapp

    
    
    rebar compile

如果报什么错，应用 rebarapp 就可以发布了：

    
    
    rebar generate

在终端上看到 "==> rel (generate)" 且没报什么错，应用 rebarapp 发布成功，并在 rel/rebarapp/bin
目录下生成一个用来启动应用或停止应用等操控动作的 shell 文件 rebarapp。

操控文件 rel/rebarapp/bin/rebarapp 用法：

    
    
    rebarapp {start|start_boot |foreground|stop|restart|reboot|ping|console|console_clean|console_boot |attach|remote_console|upgrade}

例如：

启动应用 rebarapp

    
    
    rel/rebarapp/bin/rebarapp start

停止应用 rebarapp

    
    
    rel/rebarapp/bin/rebarapp stop

或者启动应用 rebarapp 后返回一个 erlang shell 的控制台

    
    
    rel/rebarapp/bin/rebarapp console

OK，在 erlang shell 的控制台上调用 rebarapp_server:hello() 输出一个 "Hello World!" 吧。

