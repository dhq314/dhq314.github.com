---
layout: page
title: 利用 jsonp 突破 ajax 同源策略限制进行跨域请求
---
{% include JB/setup %}

最近瞎搞了一个 Erlang Web Shell -- [ErlShell](http://dhq.me/deploy-erlang-web-shell-
with-webmachine-erlydtl-on-heroku)，一直想把它的前端框架移植到敝站上，然后用 ajax 的方式调用部署在 heroku
上的远程接口。不过大多数浏览器出于安全考虑，ajax 都有个同源策略（[same origin
policy](http://en.wikipedia.org/wiki/Same_origin_policy)）限制，就是 ajax 的
[XmlHttpRequest](http://www.w3.org/TR/XMLHttpRequest/) 方法只能在当前域名下被调用，不能以 ajax
的方式进行跨域请求。

还好，问题终该有其解决的方法。对于 ajax 跨域请求的问题，可以用 jsonp 的方式解决。

**JSONP 原理**

jsonp（json with padding的简称）是一种利用 javascript
来进行通信交互的技术，它提供一种在浏览器同源策略的限制下，可以请求不同域名服务器数据的方法。

其实 jsonp 是利用 html 里没有跨域限制的 <script> 标签来动态请求其他域的 javascript 代码，而响应返回的
javascript 代码中可以包含 json 格式的 javascript 对象字面量。不过，返回的也不一定是 json
的对象字面量，也可以是其他数据或对象字面量，只是这种用法模式被称为 jsonp 而已了。

例如本地有这样一段 javascript 代码：

    
    
    <script type="text/javascript">
    function test(json) {
        alert(json.test);
    }
    </script>

一般都是这样调用这个 test 函数：

    
    
    test({test : 'test'});
    

假如其他域名服务器上 <http://genfsm.herokuapp.com/erlshell/test.js> 有这样一个 javascript
文件，并且返回内容就是上面的 "test({test : 'test'});"，那么利用 script 标签动态调用那个 javascript
文件，就会执行本地的 test 函数。

    
    
    var url = "http://genfsm.herokuapp.com/erlshell/test.js";
    var script = document.createElement('script');
    script.setAttribute('src', url);
    document.getElementsByTagName('head')[0].appendChild(script); 
    

嗯，其实 jsonp 最终返回的是被 javascript 解释器解析的 javascript 代码，而不是 json 格式的数据，只不过可以返回 json
格式的 javascript 对象字面量。

**移植方案**

这里使用 jQuery 封装的 jsonp 方法，客户端添加一个 get_jsonp 函数：

    
    
    //获取jsonp函数
    ErlShell.get_jsonp = function(data, callbackfun) {
        $.ajax({
            type : "get",
            async : false,
            url : ErlShell.url,
            dataType : "jsonp",
            jsonp : "callback",
            data : data,
            success : callbackfun
        });
    };
    

  * type：jsonp 不存在 POST 请求方式，所以上面的获取方式只可以写为 GET
  * async：异步方式
  * url：跨域的服务器地址
  * dataType：标明数据类型 dataType 为 jsonp
  * jsonp：标明服务器端获取 jsonp 回调函数名的参数名（默认也是 callback）
  * data：要传送给服务器端的参数（json格式）
  * success：成功响应返回时的回调函数

原本的 $.post 异步调用全部用 get_jsonp 函数替换，例如原来的心跳包 ErlShell.erlshell_heart 函数：

    
    
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
    

用 get_jsonp 函数替换后：

    
    
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
        ErlShell.get_jsonp(data, function(rs) {
            if ( rs.result == 41 )                      //进程异常关闭
            {
                ErlShell.erlshell_stop();
                alert("进程异常已关闭，请重新启动 ErlShell！"); 
            }
        });
    };
    

由于 HTTP 请求方式由原来的 POST 改为 GET，所以服务端对于客户端的 HTTP 请求处理函数从原来 process_post
函数改为负责最终返回数据进行操作处理的 to_jsonp 函数（content_types_provided 函数指定），to_jsonp 函数改为如下：

    
    
    to_jsonp(ReqData, Context) ->
        Return =
            case wrq:get_qs_value("callback", ReqData) of           %% 获取回调函数名
                undefined ->
                    "error";
                CallBack ->
                    JsonPropList =
                        case wrq:get_qs_value("action", ReqData) of  
                            "1" ->
                                erlshell_create(ReqData);
                            "2" ->
                                erlshell_stop(ReqData); 
                            "3" ->
                                erlshell_eval(ReqData);
                            "4" ->
                                erlshell_heart(ReqData);
                            _ ->
                                [{result, 2}]
                        end,
                    CallBack ++ "(" ++ encode_json(JsonPropList) ++ ")"
            end,
        {Return, ReqData, Context}.
    

上面的 CallBack 就是回调函数名，之前的创建 ErlShell、关闭 ErlShell、解析 erlang 表达式字符串、ErlShell
的心跳包等函数返回的数据格式没变，都是返回 json 格式的数据，并且返回的 json 数据作为 CallBack 参数传回客户端调用。

由于 jsonp 特殊的调用方式，因此新加了
[erlshell_action_jsonp_resource.erl](https://github.com/dhq314/heroku-genfsm/b
lob/master/src/mod/erlshell/erlshell_action_jsonp_resource.erl)、[erlshell_json
p.js](https://github.com/dhq314/heroku-
genfsm/blob/master/priv/www/js/erlshell_jsonp.js) 这两个文件以作 jsonp
特殊处理，详细的代码细节可以在 [github](https://github.com/dhq314/heroku-genfsm) 上查看。

**其他跨域方法**

1、crossdomain.xml

在请求域的根目录下创建一个名为 crossdomain.xml 的 XML 文件，内容如下：

    
    
    <?xml version="1.0"?>
    <cross-domain-policy>
    <allow-access-from domain="*" />
    </cross-domain-policy>
    

2、iframe

在 当前页面里生成一个请求域的 iframe，在 iframe 里通过 ajax 方式获取请求域服务器上的数据

