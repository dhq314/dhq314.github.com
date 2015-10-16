---
layout: page
title: 在 heroku 上部署基于 webmachine + erlydtl 的 erlang web shell -- ErlShell
---
{% include JB/setup %}

其实很久之前就在网上留意到一个 erlang web shell --
[erlwsh](https://github.com/killme2008/erlwsh)，粗看了一下，实现的方法也很简单。恰好自己在 heroku
上也瞎搞了一个应用 -- [genfsm](http://dhq.me/heroku-deploy-erlang-web-webmachine-
mochiweb-erlydtl)，想把它像之前 [egeoip](http://dhq.me/heroku-erlang-ip-server-with-
egeoip) 那样通过 rebar 依赖进来。

不过 erlwsh 的启动会同时启动 mochiweb，erlwsh 的数据存放在 mochiweb 进程上，而 genfsm 里的 mochiweb 是由
webmachine 控制启动的，这跟  genfsm 里的 mochiweb 启动先后有冲突。同时 erlwsh 是数据跟样式（html）混合写在一起，
这跟使用数据与样式分离的 webmachine + mochiweb + erlydtl 组合的 genfsm 也有冲突。

没办法，架构上的冲突，只能根据自己的需求，重造一个适合自己的轮子 --
[ErlShell](http://genfsm.herokuapp.com/erlshell/)。

**功能需求**

  * ErlShell 是一个独立的进程，不依附在其他进程上，只负责解析 erlang 表达式字符串
  * 采用一种心跳包机制，客户端定时发心跳包信息，服务端的 ErlShelll 进程定时检测心跳包时间，心跳包超时则自动关闭 ErlShell
  * 数据与表现分离，每一个 ErlShell 的操作请求，服务端只返回 json 形式的数据

**独立进程**

这里 ErlShell 进程采用 [gen_server](http://www.erlang.org/doc/design_principles/gen_
server_concepts.html) 的行为模式独立启动，该进程除了心跳包消息接口之外，只提供解析 erlang
表达式字符串的消息（'EVAL_ERLSTR'）接口。进程信息保存在进程字典里，进程的状态信息 State，除了存储被赋值绑定的变量 bindings
之外，还保存有心跳包时间 heart_time、检测心跳包的定时器 heart_timer、心跳包的检测间隔 heart_time_interval、行号
line_num，具体如下：

    
    
    -record(state, {
        heart_time = 0,                     %% 心跳包时间
        heart_timer = undefined,            %% 检测心跳包的定时器 
        heart_time_interval = 10,           %% 心跳包的检测间隔
        bindings = [],                      %% 已经被绑定赋值的变量列表
        line_num = 1                        %% 行号
    }).
    

而客户端 javascript 也定义一个名为 ErlShell 的对象字面量，用来保存客户端的状态数据：

    
    
    var ErlShell = {
        "status" : 1,                                       //ErlShell状态，1表示没启动，2已启动
        "pid" : undefined,                                  //ErlShell的进程标识     
        "timer" : undefined,                                //心跳包定时
        "interval" : 10,                                    //心跳包定时间隔
        "line_num" : 1,                                     //ErlShell的行数
        "process" : 0,                                      //0标识当前没请求要处理，1反之
        "url" : "http://" + Domain + "/erlshellaction/"     // POST请求的地址
    };
    

**心跳包机制**

ErlShell 的心跳包机制，其实是 ErlShell 的进程 init 启动的时候，会调用 erlang:send_after/3 启动一个给自身发送
'DETECT_HEART' 消息的定时器：

    
    
    init([HeartTimeInterval]) ->
        HeartTime = util:unixtime(),
        HeartTimer = erlang:send_after(HeartTimeInterval * 1000, self(), 'DETECT_HEART'),
        Bindings = erl_eval:new_bindings(),
        State = #state{
            heart_time = HeartTime,
            heart_timer = HeartTimer,
            heart_time_interval = HeartTimeInterval,
            bindings = Bindings
        },
        {ok, State}.
    

进程 ErlShell 收到 'DETECT_HEART' 消息，会有一个策略检测当前的 ErlShell 的连接是否正常：当前时间（Now）跟上一次心跳包
时间（State#state.heart_time）相差大于2倍心跳包定时间隔时间（State#state.heart_time_interval），则关闭
该 ErlShell 进程，否则继续开一个 'DETECT_HEART' 消息的定时器：

    
    
    %% @doc 心跳包检测
    handle_info('DETECT_HEART', State) ->
        Now = util:unixtime(),
        case Now - State#state.heart_time > State#state.heart_time_interval * 2 of
            true ->
                {stop, normal, State};
            false ->
                HeartTimer = erlang:send_after(State#state.heart_time_interval * 1000, self(), 'DETECT_HEART'),
                NewState = State#state{
                    heart_timer = HeartTimer
                },
                {noreply, NewState}
        end;
    

客户端接收到服务器 ErlShell 进程启动成功的消息后。会初始客户端的一些状态数据，创建命令行（其实是 id 值为 es_command_line 的
div 块），绑定命令行事件：

    
    
    //启动ErlShell
    ErlShell.erlshell_init = function(rs) {
        ErlShell.pid = rs.pid;
        ErlShell.interval = rs.interval;
        ErlShell.line_num = rs.line_num;
        ErlShell.action = 2;
        ErlShell.process = 0,
        $("#es_div").html("");
        //创建命令行
        ErlShell.create_es_line(ErlShell.line_num);
        //绑定命令行事件
        ErlShell.bind_es_command_line_keypress();
        $("#es_div").css({"background-color" : "#FFF"});
        $("#erlshell_action").html("Stop");
        //开启 ErlShell 心跳包定时器
        ErlShell.timer = setInterval(ErlShell.erlshell_heart, ErlShell.interval * 1000);
        ErlShell.erlshell_heart();
        $(window).bind('beforeunload', function() {
            return "确定要退出 ErlShell ？";
        });
    };
    

同时，也会初始一个定时器，定时调用 ErlShell.erlshell_heart 函数向服务器发送 POST 请求，更新服务器端的心跳包时间数据：

    
    
    // ErlShell 的心跳包函数
    ErlShell.erlshell_heart = function() {
        //ErlShell如果已经关闭，则关停定时器
        if ( ErlShell.status != 2 )
        {
            if ( ErlShell.timer )
            {
                clearTimeout(ErlShell.timer);
            }
            ErlShell.timer = undefined;
            return false;
        }
        var data = { "action" : 4, "pid" : ErlShell.pid };
        $.post(ErlShell.url, data, function(rs) {
            if ( rs.result == 41 )                      //进程异常关闭
            {
                ErlShell.erlshell_stop();
                alert("进程异常已关闭，请重新启动 ErlShell！"); 
            }
        }, "json");
    };
    

**命令行事件**

绑定命令行事件其实是监听命令行的回车（Enter）按键（keypress）事件，如果捕捉到回车按键，则把当前命令行的非空值以 POST
的方式传到服务器解析，并把返回的解析结果显示出来，然后再生成一个新的命令行，再绑定新的命令行回车事件继续重复以上的逻辑操作：

    
    
    //绑定命令行事件
    ErlShell.bind_es_command_line_keypress = function() {
        $("#es_command_line").bind("keypress", function(event) {
            var keycode = event.keyCode ? event.keyCode : event.which;
            if ( keycode == 13 )        //回车事件 
            {
                var erl_str = "", data = {};
                // 获取命令行里的 erlang 表达式字符串
                erl_str = $.trim($("#es_command_line").text());
                if ( erl_str )
                {
                    data = { "action" : 3, "pid" : ErlShell.pid, "erl_str" : erl_str };
                    $("#es_div").css({"background-color" : "#EDEDED"});
                    $.post(ErlShell.url, data, function(rs) {
                        if ( parseInt(rs.action) == 3 )
                        {
                            $("#es_div").css({"background-color" : "#FFF"});
                            var es_result = "#es_result_" + ErlShell.line_num;
                            $(es_result).html(rs.value);
                            if ( rs.result == 1 )
                            {
                                ErlShell.reset_es_keypress();
                                ErlShell.line_num = rs.line_num;
                                ErlShell.create_es_line(ErlShell.line_num);
                                ErlShell.bind_es_command_line_keypress();
                            }
                            else if ( rs.result == 31 )         //进程异常关闭
                            {
                                ErlShell.erlshell_stop();
                                alert("进程异常已关闭，请重新启动 ErlShell！"); 
                            }
                        }
                    }, "json");
                } 
                return false;
            }
        });
    };
    

**URL 路由规则**

dispatch.conf 添加了两条路由规则：

    
    
    {["erlshell", '*'], erlshell_resource, []}.
    {["erlshellaction", '*'], erlshell_action_resource, []}.
    

erlshell_resource 只负责 ErlShell 的页面布局渲染，返回的是 html 格式数据。

erlshell_action_resource 只返回 json 格式的数据，负责处理客户端 POST 过来的各种 HTTP 请求。而该模块里的
process_post/2 函数则是上面所说的处理客户端以 POST 方式传送过来的请求的核心函数。每一个 POST 请求都有一个 "action"
操作码，1表示创建 ErlShell（erlshell_create/1），2表示关闭
ErlShell（erlshell_stop/2），3表示解析从客户端传过来的字符串（erlshell_eval/2），4表示 ErlShell
的心跳包请求（erlshell_heart/2）。

**erlang 动态解析**

当服务器接受到操作码 action 是 3 时，会把从客户端传过来的 erlang 表达式字符串掉给 ErlShell 进程去分析解释，具体是调用
erlshell_server 模块里的内部函数 eval：

    
    
    %% @doc 解析函数
    eval(ErlStr, Bindings) ->
        {ok, Tokens, _EndLocation} = erl_scan:string(ErlStr),
        %% 表达式字符串后面要以点号结束
        NewTokens = 
            case lists:reverse(Tokens) of
                [{dot, _} | _] -> Tokens;
                TokensReverse -> lists:reverse([{dot, 1} | TokensReverse])
            end,
        {ok, ExprList} = erl_parse:parse_exprs(NewTokens),
        erl_eval:exprs(ExprList, Bindings).
    

这个函数负责解析从客户端传过来的 erlang 表达式字符串 ErlStr，Bindings 则是已赋值被绑定的变量列表。其过程主要是通过
[erl_scan:string/1](http://www.erlang.org/doc/man/erl_scan.html#string-1)
函数先把传过来的 erlang 表达式字符串转换成 erlang 的语法标记（tokens），然后再通过 [erl_parse:parse_exprs/1]
(http://www.erlang.org/doc/man/erl_parse.html#parse_exprs-1) 函数把语法标记解析成 erlang
的表达式（expression），最后通过
[erl_eval:exprs/2](http://www.erlang.org/doc/man/erl_eval.html#exprs-2) 函数分析解释
erlang 表达式，并最终返回解析结果。其实这个 eval 函数是 erlang 源代码里的 init.erl 文件里的 start_it
函数（详见霸爷的《[erlang动态解释](http://blog.yufeng.info/archives/tag/erl_eval)》）。

当然，从客户端传过来的 erlang 表达式字符串会调用 check_valid/1 函数做一个合法验证，如果含有一些非法操作字符，则不与分析处理：

    
    
    %% @doc 检查表达式是否含有非法语句
    check_valid(ErlStr) ->
        REList = ["application(.*)stop", "os(.*)cmd(.*)rm"],
        check_valid(REList, ErlStr, true).
    check_valid([], _ErlStr, Bool) ->
        Bool;
    check_valid([RE | R], ErlStr, Bool) ->
        case re:run(ErlStr, RE) of
            {match, _Captured} ->
                false;
            _ ->
                check_valid(R, ErlStr, Bool)
        end.
    

以上所述只是 ErlShell 的大概逻辑流程，详细的操作可以打开 <http://genfsm.herokuapp.com/erlshell/> 体验。

同时，以上代码片段也只是当前代码状态的一个快照，详细的代码细节和最新的代码修改可以在 [github](https://github.com/dhq314
/heroku-genfsm) 上找到，功能主要涉及到以下这几个文件：

  * [erlshell_action_resource.erl](https://github.com/dhq314/heroku-genfsm/blob/master/src/mod/erlshell/erlshell_action_resource.erl)：处理客户端以 POST 方式传送过来的各种请求
  * [erlshell_server.erl](https://github.com/dhq314/heroku-genfsm/blob/master/src/mod/erlshell/erlshell_server.erl)：ErlShell 的进程文件
  * [erlshell_resource.erl](https://github.com/dhq314/heroku-genfsm/blob/master/src/mod/erlshell/erlshell_resource.erl)：负责页面的布局渲染
  * [erlshell.js](https://github.com/dhq314/heroku-genfsm/blob/master/priv/www/js/erlshell.js)：负责客户端各种 javascript 操控

**改进的地方**

代码并没有完美到没有改进的地方，这个 erlang web shell 的代码会不定时的更新提交到 github
上，或者是一个注释，或者是一个清晰易懂的变量或逻辑结构，或者是一个良好的体验修改。不过以下这几点会优先尝试去修改：

  * 在命令行里，空行按回车键光标跳到下一行
  * 在命令行里按上/下方向键显示使用过的历史命令
  * 用 WebSocket 代替现在的数据传送方式

嗯，先挖几个坑在这里，以后再慢慢填~

