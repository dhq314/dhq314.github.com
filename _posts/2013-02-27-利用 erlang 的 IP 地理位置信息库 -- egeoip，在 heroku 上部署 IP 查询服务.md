---
layout: page
title: 利用 erlang 的 IP 地理位置信息库 -- egeoip，在 heroku 上部署 IP 查询服务
---
{% include JB/setup %}

[egeoip](https://github.com/mochi/egeoip) 是一个用来读取
[MaxMind](http://www.maxmind.com/) 地理位置数据库的纯 erlang 写的模块库，而 MaxMind 是一个类似于纯真
IP 数据库 的 IP 地址信息数据库，只不过 MaxMind 返回的数据是英文，也可以说，MaxMind 是英文版的纯真 IP 数据库。这个 IP
地址信息查询服务已经放在 heroku 上，如果有兴趣，也可以打开 <http://genfsm.herokuapp.com/tool/> 感观地去看下
demo :)

**安装 egeoip**

直接从 github 上下载：

    
    
    git clone git://github.com/mochi/egeoip.git

下载完之后，在根目录下的 priv 文件夹里有个名为 GeoLiteCity.dat 的文件，这就是要被 egeoip
读取地理位置数据的文件。不过项目内置的 GeoLiteCity.dat 一般都不是最新的，如果你想下载最新的地理位置数据，也可以从
<http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz> 上下载。

由于 egeoip 也是用 [rebar](http://dhq.me/build-compile-eunit-release-erlang-
application-with-rebar) 来构建的，所以可以用 rebar 来编译安装：

    
    
    rebar clean
    rebar get-deps compile
    

编译完后，在根目录下输入以下命令，启动该应用的 erlang shell：

    
    
    erl -pa ebin

在 erlang shell 里启动应用 egeoip：

    
    
    application:start(egeoip).

或者直接在起 erlang shell 时直接启动 egeoip 应用：

    
    
    erl -pa ebin -s egeoip

在 erlang shell 里调用 egeoip:lookup/1 查下度娘的 IP 吧：

    
    
    4> egeoip:lookup("115.239.210.27").
    {ok,{geoip,"CN","CHN","China",<<"22">>,<<"Beijing">>,<<>>,
               39.9289,116.38830000000002,0,0}}
    

egeoip:lookup/1 返回的是一个 "{ok, GeoipRecord}" 的格式，而 GeoipRecord 是一个 geoip
记录（记录信息详看 /include/egeoip.hrl）。

嗯，安装 egeoip 还是很简单的，直接下载编译就行。

PS：在 github 上找 egeoip 项目时意外地发现有人做了一个基于 egeoip 查找 IP 地理位置信息的
[zotonic](http://dhq.me/unbuntu-install-zotonic) 模块 --
[mod_geopip](https://github.com/mmzeeman/mod_geoip)，以后有需要可以直接拿来用咧:^)。

**部署到 heroku**

由于这里的代码框架是在《[在 heroku 上部署 webmachine + mochiweb + erlydtl 组合的Erlang
Web应用](http://dhq.me/heroku-deploy-erlang-web-webmachine-mochiweb-
erlydtl)》的基础上开发的，所以最好先看下之前的一些关于 heroku 应用部署命令介绍，也很简单，因此之类的如何在 heroku
注册和发布应用等等的操作说明这里就不多说了。

这个应用的全部代码已经托管在 [github](https://github.com/dhq314/heroku-genfsm)
上，或者也可以把该应用的代码 clone 下来，再 reset 到之前的代码版本：

    
    
    git clone git://github.com/dhq314/heroku-genfsm.git
    cd ./heroku-genfsm/
    git reset --hard 20417f090ac8d7678c3165707de2ba1e85e6320f
    

代码 clone 下来后，修改 genfsm_sup.erl 的 init 函数，把应用监听端口的变量 Port 的获取方式改为如下：

    
    
    Port = 
        case os:getenv("PORT") of
            false ->
                case os:getenv("WEBMACHINE_PORT") of
                    false -> 8000;
                    AnyPort -> AnyPort
                end;
            AnyPort -> list_to_integer(AnyPort)
        end,
    

改完后就可以直接用 rebar 编译了：

    
    
    rebar clean
    rebar get-deps compile
    

调用根目录下的 "start.sh" 启动应用 genfsm，就可以在浏览器里打开 <http://localhost:8000/> 访问该应用了。

OK，基础代码框架部署完后，就可以在这基础上开发我们的 IP 地理信息查询服务了。

在 rebar.config 添加 egeoip 的依赖，把 egeoip 嵌入到应用 genfsm 里，修改后的内容如下：

    
    
    %%-*- mode: erlang -*-
    {sub_dirs, ["rel"]}.
    {deps_dir, ["deps"]}.
    {erl_opts, [debug_info]}.
    
    {deps, [
                {webmachine, "1.9.*", {git, "git://github.com/basho/webmachine", "HEAD"}},
                {erlydtl, ".*", {git, "git://github.com/evanmiller/erlydtl.git", "master"}},
                {egeoip, ".*", {git, "git://github.com/mochi/egeoip.git", "master"}}
            ]
    }.
    

修改 genfsm_sup.erl 的 init 函数，把 egeoip 作为应用管理者 genfsm_sup 的工作进程启动，init 函数修改如下：

    
    
    %% @spec init([]) -> SupervisorTree
    %% @doc supervisor callback.
    init([]) ->
        Ip = 
    	case os:getenv("WEBMACHINE_IP") of 
    		false -> "0.0.0.0"; 
    		Any -> Any 
    	end,
        {ok, App} = application:get_application(?MODULE),
        {ok, Dispatch} = file:consult(filename:join([priv_dir(App), "dispatch.conf"])),
    	Port = 
            case os:getenv("PORT") of
                false ->
                    case os:getenv("WEBMACHINE_PORT") of
                        false -> 8000;
                        AnyPort -> AnyPort
                    end;
                AnyPort -> list_to_integer(AnyPort)
            end,
        WebConfig = [
                     {ip, Ip},
                     {port, Port},
                     %{log_dir, "priv/log"},
                     {dispatch, Dispatch}],
    	Egeoip = {egeoip, {egeoip, start_link, [egeoip]},
                    permanent, 5000, worker, [egeoip]},
        Web = {webmachine_mochiweb,
               {webmachine_mochiweb, start, [WebConfig]},
               permanent, 5000, worker, [mochiweb_socket_server]},
        Processes = [Egeoip, Web],
        {ok, { {one_for_one, 10, 10}, Processes} }.
    

这里 egeoip:start_link/1 只开了一个 IP 查询的服务工作进程，而默认的 egeoip 启动
application:start(egeoip) 会开8个服务工作进程（[详看这里](https://github.com/mochi/egeoip/bl
ob/master/src/egeoip_sup.erl)），egeoip:lookup/1 却会在8个服务工作进程中随机调用一个服务工作进程来处理 IP
查询，因此这里不能用 egeoip:lookup/1 来查询 IP，下面会说到自写一个查询函数来处理。没办法，内存有限，一个服务进程加载一份
GeoLiteCity.dat 数据，伤不起:(

添加格式是 "http://hostname/ip/IP地址" 的 URL 路由规则（这里的 hostname
指的是你的域名，例如：[genfsm.herokuapp.com](http://genfsm.herokuapp.com/)），在
priv/dispatch.conf 加上：

    
    
    {["ip", '*'], ip_resource, []}.

这里 "http://hostname/ip/IP地址" 的 HTTP 请求都会返回一个格式是 json 的地理位置信息数据。

上面指定了 ip_resource 来处理新加的 URL 路由请求，因此要在 src 创建一个 ip_resource.erl 的 erl
文件，用来接收处理 IP 查询，内容如下：

    
    
    -module(ip_resource).
    -export([init/1, content_types_provided/2, resource_exists/2, to_json/2]).
    
    -include_lib("webmachine/include/webmachine.hrl").
    
    init(_) -> {ok, undefined}.
    
    %% @doc 定义返回数据的格式，以及最终对返回数据进行操作的函数 to_json
    content_types_provided(ReqData, Context) ->
        {[{"application/json", to_json}], ReqData, Context}.
    
    
    %% @doc 处理 HTTP 请求的函数
    resource_exists(ReqData, _Context) ->
        IpStr = wrq:disp_path(ReqData),
        Ip =
    		case re:run(IpStr, "(\\d{1,3}\\.){3}\\d{1,3}", [{capture, first, list}]) of
    			{match, [IpStr1]} ->
    				IpStr1;
    			_ ->
    				ReqData#wm_reqdata.peer
    		end,
    	Result = egeoip_lookup(Ip),
        {true, ReqData, Result}.
    
    
    %% @doc 对返回数据进行操作的函数
    to_json(ReqData, Result) ->
        {mochijson:encode(Result), ReqData, Result}.
    
    
    
    %% ------------------------------------------------------------------
    %% Internal Function Definitions
    %% ------------------------------------------------------------------
    
    %% @doc 上面所说的自定义的 IP 查询函数
    egeoip_lookup(Ip) ->
    	Result = 
    		case egeoip:ip2long(Ip) of
    	        {ok, Ip1} ->
    				case whereis(egeoip) of
    			        undefined ->
    					    [{result, 3}];
    					Pid ->	
    					    {ok, EgeoIP} = gen_server:call(Pid, {lookup, Ip1}),
    					    Country = egeoip:get(EgeoIP, country_name),
    					    City = egeoip:get(EgeoIP, city),
    					    [{result, 1}, {ip, Ip}, {country, Country}, {city, City}]
    				end;
    	        _Error ->
    				[{result, 2}]
    	    end,
    	{struct, Result}.
    

content_types_provided：定义返回数据的格式，以及指定对返回数据进行操作的函数，这里指定的是 to_json 函数。

resource_exists：先通过 wrq:disp_path/1 获取请求过来的数据，然后调用 erlang 的正则匹配函数 re:run/3 匹配
IP 地址，匹配不成功的话取当前客户端的 IP 地址，最后交由自定义的 IP 查询函数 egeoip_lookup/1 来处理。

to_json：调用 mochiweb 的内置 json 编码函数 mochijson:encode/1，对最后返回客户端的数据进行 json 编码。

egeoip_lookup：之前所说的自定义 IP 查询函数，其实就是通过原子 egeoip 找到 IP 查询服务的工作进程 Pid，再通过进程 Pid
调用默认的查询方法查出该 IP 地址的地理位置信息。

OK，获取 IP 地址信息 json 格式数据的 URL 路由规则完成，重新获取应用的依赖编译下应用吧：

    
    
    rebar clean
    rebar get-deps compile
    

编译完后启动应用 genfsm，就可以通过请求
"http://hostname/ip/IP地址（例如：<http://genfsm.herokuapp.com/ip/115.239.210.27>）"
获取 IP 地址的 json 数据格式，返回类似于以下的 json 格式数据：

    
    
    {"result":1,"ip":"115.239.210.27","country":"China","city":"Beijing"}

当然，也可以打开 <http://genfsm.herokuapp.com/tool/> 查看该 IP 地址查询服务的 demo。这里通过 ajax
的方式请求 "http://genfsm.herokuapp.com/ip/IP地址"，然后把返回的 JSON
数据显示出来。详细的代码这里不给出了，该应用的全部代码已经托管在 [github](https://github.com/dhq314/heroku-
genfsm) 上，详细的代码或者最新的代码修改可以在 github 上获取查看。

