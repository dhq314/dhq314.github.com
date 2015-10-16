---
layout: page
title: 在 Heroku 上部署 Webmachine + Mochiweb + ErlyDTL 组合的 Erlang Web 应用
---
{% include JB/setup %}

最近在看 [Zotonic](http://dhq.me/unbuntu-install-zotonic) 的
[Webmachine](https://github.com/basho/webmachine) 的代码，恰好在网上碰到一篇关于在
[Heroku](http://www.heroku.com/) 上部署 webmachine
的[文章](http://zianet.dk/blog/2011/12/16/running-erlang-webmachine-on-
heroku/)。本着学习一样东西最好就是直接使用它的精神，也加上之前也没玩过 Heroku 这个 Paas(Platform as a service)
云服务应用平台，所以就参考那篇文章，在 Heroku 也部署了自己的一个 [Erlang Web
应用](http://genfsm.herokuapp.com/)。

**安装配置 Heroku 的开发环境**

如果还没有 Heroku 账号，先[注册](https://api.heroku.com/signup/devcenter)一个。接着安装 Heroku
的本地开发工具 -- Toolbelt，打开 [Heroku Toolbelt](https://toolbelt.heroku.com/)
主页，下载相应操作系统（以下操作都是在 mac 系统下进行）的安装文件，根据安装提示和自己的安装癖好一直点就行。

Toolbelt 安装好后，在终端上输入：

    
    
    heroku login

根据提示输入刚才注册的邮箱名和密码，接着出现一个提示上传 ssh 密钥的提示框。如果你的 ~/.ssh/
文件夹里有已经生成好的密钥，会列出当前已有的密钥，输入密钥前的数字按回车就行。如果 ~/.ssh/
文件夹里没找到密钥，会有问你是否要生成密钥提示，直接按回车生成就行。生成会自动上传你的密钥，最后在终端里出现 "Authentication
successful" 后，本地的 Heroku 开发环境算配置完成。

如果出现以下错误：

    
    
    Warning: Permanently added the RSA host key for IP address '50.19.85.156' to the list of known hosts.
    Permission denied (publickey).
    fatal: Could not read from remote repository.
    Please make sure you have the correct access rightsand the repository exists.
    

则 ssh 的 publickey 没有上传成功，可以用 heroku 的
[keys](https://devcenter.heroku.com/articles/keys) 指令重新再上传一次。

清除之前的 ssh key：

    
    
    heroku keys:clear

指定要添加的 ssh key：

    
    
    ssh-keygen -t rsa -C "some comment"
    heroku keys:add ~/.ssh/id_rsa.pub
    

上面的 id_rsa.pub 是指用 ssh-keygen 生成的 publickey。

详细的 heroku 开发环境配置，可以参看官方的配置指南 -- 《[Getting Started with
Heroku](https://devcenter.heroku.com/articles/quickstart)》。

**安装webmachine**

在终端上 cd 到你想安装的文件目录里（这里以当前用户目录下的 erlang 文件夹 "~/erlang/" 为例），并确保已经安装了 git 环境

    
    
    git clone git://github.com/basho/webmachine
    cd webmachine
    make
    

这里以 genfsm 为应用名，创建一个 webmachine 实例应用，并把实例应用放在新创建的 Heroku 文件夹里：

    
    
    mkdir -p ./heroku
    ./scripts/new_webmachine.sh genfsm ./heroku
    cd ./heroku/genfsm
    make
    

在终端里运行 "./start.sh"，然后在浏览器上输入 "<http://localhost:8000/>"，就可以看到 Webmachine
的欢迎页面了。

**部署应用到 Heroku 上**

在 genfsm 的应用根目录（"~/erlang/webmachine/heroku/genfsm"）里初始一个新的 git 版本仓库：

    
    
    git init

创建以 genfsm 为应用名的 Heroku 应用：

    
    
    heroku create genfsm -s cedar
    

这里选择 [cedar](https://devcenter.heroku.com/articles/cedar) 作为该应用的运行时环境(runtime
stack)，这里的 "-s" 等同于 "--stack"。当在终端出现以下显示时，应用创建成功：

    
    
    Creating genfsm... done, stack is cedar
    http://genfsm.herokuapp.com/ | git@heroku.com:genfsm.git
    Git remote heroku added
    

"<http://genfsm.herokuapp.com/>" 是该应用的访问地址，"git@heroku.com:genfsm.git" 是该应用的
git 地址。

设置该应用 genfsm 为 Erlang
的[构建包(buildpack)](https://devcenter.heroku.com/articles/buildpacks)，这里使用官方发布的
[erlang 构建包](https://github.com/heroku/heroku-buildpack-erlang)：

    
    
    heroku config:add BUILDPACK_URL=http://github.com/heroku/heroku-buildpack-erlang.git
    

把应用的时区时间改为北京时区时间：

    
    
    heroku config:add TZ="Asia/Shanghai"

PS："Asia/Shanghai" 是北京时间的时区数据格式值(tz database timezone)，更多的时区数据值可以查看[这里](http:/
/en.wikipedia.org/wiki/List_of_tz_database_time_zones)。

修改 rebar.config 文件配置，添加 [ErlyDTL](https://github.com/evanmiller/erlydtl) 模板依赖：

    
    
    %%-*- mode: erlang -*-
    {sub_dirs, ["rel"]}.
    {deps_dir, ["deps"]}.
    {erl_opts, [debug_info]}.
    
    {deps, [
                {webmachine, "1.9.*", {git, "git://github.com/basho/webmachine", "HEAD"}},
                {erlydtl, ".*", {git, "git://github.com/evanmiller/erlydtl.git", "master"}}
            ]
    
    }.
    

在 genfsm 应用根目录下创建一个 templates 文件夹，用来存放应用的模板文件。

    
    
    mkdir -p ./templates

在 templates 文件夹里创建一个 genfsm.dtl 模板文件，输入以下内容：

    
    
    <!DOCTYPE html>
    <html>
    <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>{{application_name}}</title>
    <style type="text/css">
    body { margin: 0; padding: 0; }
    p { margin: 3px 0; font-size: 22px; }
    a, a:active { color: #00A9DA; }
    a:hover { text-decoration: none; }
    </style>
    </head>
    <body>
        <div style="text-align: center;">
            <h1>Hi, welcome to my heroku erlang app!<h1>
            <div style="width: 650px; margin: 0 auto; text-align: left;">
                <h2 style="text-align: center;">Status</h2>
                <p>AppName：{{application_name}}</p>
                <p>Port：{{port}}</p>
                <p>SchedulerId：{{scheduler_id}}</p>
                <p>SchedulerNum：{{scheduler_num}}</p>
                <p>ProcessCount：{{process_count}}</p>
                <p>ProcessLimit：{{process_limit}}</p>
                <p>Memory used by erlang processes：{{processes_used}}</p>
                <p>Memory allocated by erlang processes：{{processes}}</p>
                <p>The total amount of memory allocated：{{memtotal}}</p>
                <p>OTP Version：{{otp_release}}</p>
                <p>OS：{{os}}</p>
                <p>ClientIp：{{client_ip}}</p>
                <p>IntranetIp：{{intranet_ip}}</p>
                <p>RequestTime：{{request_time}}</p>
                <p>Blog：<a href="http://dhq.me/">D.H.Q的烂笔头</a></p>
            </div>
        </div>
    </body>
    </html>
    

修改 src/genfsm_resource.erl 里的 to_html 函数，调用上面创建的 genfsm.dtl 模板文件作为返回显示，修改如下：

    
    
    %% @author author <author@example.com>
    %% @copyright YYYY author.
    %% @doc Example webmachine_resource.
    
    -module(genfsm_resource).
    -export([init/1, to_html/2]).
    
    -include_lib("webmachine/include/webmachine.hrl").
    -include_lib("webmachine/include/wm_reqstate.hrl").
    
    init([]) -> {ok, undefined}.
    
    to_html(ReqData, State) ->
        %io:format("~p~n~n~n", [ReqData]),
        {ok, ApplicationName} = application:get_application(?MODULE),
        Port = 
            case os:getenv("PORT") of
                false ->
                    case os:getenv("WEBMACHINE_PORT") of
                        false -> 8000;
                        AnyPort -> AnyPort
                    end;
                AnyPort -> list_to_integer(AnyPort)
            end,
    
        SchedulerId = erlang:system_info(scheduler_id),
        SchedulerNum = erlang:system_info(schedulers),
        ProcessCount = erlang:system_info(process_count),
        ProcessLimit = erlang:system_info(process_limit),
        ProcessesMemUsed = erlang:memory(processes_used),
        ProcessesMemAlloc = erlang:memory(processes),
        MemTotal = erlang:memory(total),
    
        OTP = erlang:system_info(otp_release),
        OS = io_lib:format("~p", [erlang:system_info(os_type)]),
    
        {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
        RequestTime = io_lib:format("~p-~p-~p ~p:~p:~p", [Year, Month, Day, Hour, Minute, Second]),
    
        Socket = ReqData#wm_reqdata.wm_state#wm_reqstate.socket,
        Ip = get_ip(Socket),
    
        HtmlData = [
            {application_name, ApplicationName}, 
            {port, Port},
            {scheduler_id, SchedulerId},
            {scheduler_num, SchedulerNum},
            {process_count, ProcessCount},
            {process_limit, ProcessLimit},
            {processes_used, ProcessesMemUsed},
            {processes, ProcessesMemAlloc},
            {memtotal, MemTotal},
            {otp_release, OTP},
            {os, OS},
            {client_ip, ReqData#wm_reqdata.peer}, 
            {intranet_ip, io_lib:format("~p", [Ip])},
            {request_time, RequestTime}
        ],
    
        {ok, Html} = genfsm_dtl:render(HtmlData),
        {Html, ReqData, State}.
    
    
    get_ip(Socket) ->
        case inet:peername(Socket) of
            {ok, {Ip, _Port}} -> Ip;
            {error, _Reason} -> {0,0,0,0}
        end.
    

修改 src/genfsm_sup.erl 里的 init 函数，设置应用监听的端口(Port)，把应用 ip 设置为
"0.0.0.0"，并把日志目录选项注释掉，修改如下：

    
    
    %% @spec init([]) -> SupervisorTree
    %% @doc supervisor callback.
    init([]) ->
        %Ip = case os:getenv("WEBMACHINE_IP") of false -> "0.0.0.0"; Any -> Any end,
        {ok, App} = application:get_application(?MODULE),
        {ok, Dispatch} = file:consult(filename:join([priv_dir(App),
                                                     "dispatch.conf"])),
        %Port = case os:getenv("WEBMACHINE_PORT") of
         %      false -> 8000;
          %     AnyPort -> AnyPort
           %   end,
        Port = list_to_integer(os:getenv("PORT")),
        WebConfig = [
                     {ip, "0.0.0.0"},
                     %{ip, Ip},
                     {port, Port},
                     %{log_dir, "priv/log"},
                     {dispatch, Dispatch}],
        Web = {webmachine_mochiweb,
               {webmachine_mochiweb, start, [WebConfig]},
               permanent, 5000, worker, [mochiweb_socket_server]},
        Processes = [Web],
        {ok, { {one_for_one, 10, 10}, Processes} }.
    

在 genfsm 的应用根目录下创建一个以
[Procfile](https://devcenter.heroku.com/articles/procfile)
命名的文件，填入以下内容，声明应用在启动前该执行的指令：

    
    
    web: erl +K true -pa ebin deps/*/ebin -noshell -boot start_sasl -s reloader -s genfsm
    

在终端里输入以下编译命令编译下应用，检查是否有语法之类的错误：

    
    
    ./rebar get-deps compile

这里先把应用依赖的一些库（例如 webmachine 和 erlydtl，而
[mochiweb](https://github.com/mochi/mochiweb) 已经依赖在 webmachine
里面了）载下来，然后再执行编译。如果编译成功，则应用的本地配置完成，可以把要发布的文件或文件夹添加到版本库里：

    
    
    git add Makefile Procfile README rebar rebar.config start.sh src/* priv/dispatch.conf templates/*
    git commit -m "heroku genfsm"
    

最后，把应用发布到 Heroku 上：

    
    
    git push heroku master
    

上面命令会把本地应用的代码提交到远程 Heroku 服务器上，并编译代码，启动应用 genfsm。当在终端里出现
"http://genfsm.herokuapp.com deployed to Heroku" 的字样时，则应用 genfsm 在 Heroku
上发布启动成功，可以在浏览器里打开 <http://genfsm.herokuapp.com> 访问，也可以在终端里输入以下指令直接打开：

    
    
    heroku open

**一些 Heroku 命令**

查看应用信息

    
    
    heroku info

查看应用的配置信息

    
    
    heroku config

查看日志信息

    
    
    heroku logs -t

打开一个可以执行 Linux 命令的远程服务器终端

    
    
    heroku run bash

**遇到的问题**

**"'heroku' does not appear to be a git repository"**

可能是 .git 文件夹里的配置文件出错了吧，执行 "git push heroku master" 就报这个错，不过执行以下命令，重新指定 git
源后问题解决：

    
    
    git remote add heroku git@heroku.com:genfsm.git
    git remote -v
    

**bash: erl: command not found**

在服务器终端上发现 Erlang 安装在 /app/otp 目录下，所在的目录不在系统的环境变量 PATH 下，觉得的方法就是把 /ap/otp 添加到
PATH：

    
    
    heroku config:set PATH=bin:/usr/bin:/bin:/usr/local/lib/erlang/bin:/app/otp/bin
    

接着重启下服务就行

    
    
    git commit --allow-empty -m 'reboot'
    git push heroku master
    

