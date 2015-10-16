---
layout: page
title: Erlang 与 WebSocket 之间的通讯交互
---
{% include JB/setup %}

[WebSocket](http://zh.wikipedia.org/wiki/WebSocket) 是
[HTML5](https://zh.wikipedia.org/wiki/HTML5)
引入的新功能，它提供了一种数据可以在浏览器与服务器间双向、按序到达的实时通讯技术，可以说，它是 WEB 应用程序上的传输协议。

相对于传统的的 [Comet](http://en.wikipedia.org/wiki/Comet_%28programming%29)、[Ajax](h
ttp://zh.wikipedia.org/wiki/AJAX) 轮询（polling）、长轮询（long-
polling）以及流（streaming）等数据通讯方案，WebSocket
跟服务器连接时所发送的数据量更少，数据传输速度更快，而且它是一种服务器向客户端主动推送数据的技术，因此，它比前两者更具时效性。

**WebSocket 握手协议**

WebSocket 通讯的建立，需要浏览器向服务器发出 WebSocket 连接请求，然后服务器会返回相应的响应，这个 WebSocket
连接过程被称为“握手”（handshaking）。

一般，浏览器会先向服务器发一个类似以下的 HTTP 请求报头数据：

    
    
    GET / HTTP/1.1
    Host: localhost:12345
    User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:22.0) Gecko/20100101 Firefox/22.0
    Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
    Accept-Language: zh-cn,zh;q=0.8,en-us;q=0.5,en;q=0.3
    Accept-Encoding: gzip, deflate
    Sec-WebSocket-Version: 13
    Origin: null
    Sec-WebSocket-Key: QbGS1EqgNlt+T16EbWS9Mg==
    Connection: keep-alive, Upgrade
    Pragma: no-cache
    Cache-Control: no-cache
    Upgrade: websocket
    

  * "Upgrade:wesocket" -- 表示该信息是 WebSocket 协议
  * "Host:localhost:12345" -- 表示请求的主机地址和连接端口号
  * "Sec-WebSocket-Version：8" -- 表示连接的 WebSocket 的版本号是 8
  * "Sec-WebSocket-Key: FcJ21uh+iiDs7haoaG1cKQ==" -- 表示客户端连服务器时握手所需的验证 key，服务器端根据该 key 作相应计算后返回一个对应的 key 来完成 WebSocket 握手

而服务器收到客户端浏览器发送的请求连接后，会返回类似以下的数据：

    
    
    HTTP/1.1 101 Switching Protocols
    Upgrade: websocket
    Connection: Upgrade
    Sec-WebSocket-Accept: JDxTT0FtVIjTGCKC/2Si17aQfus=
    

其实这是一个 WebSocket 验证过程，客户端会随机发一个名为 "Sec-WebSocket-Key" 的 WebSocket Key，服务器接收到这个
WebSocket Key 后，会把这个 WebSocket Key 跟一个魔幻字符串（"258EAFA5-E914-47DA-95CA-
C5AB0DC85B11"）相加，再使用 SHA-1 加密，接着进行 BASE-64 编码，最后把结果做为 “Sec-WebSocket-Accept”
头的值返回给客户端那边，客户端接收到这个加密转码过的验证 Key 后，会跟自身的期望值比较，如果一样，则 WebSocket 连接成功。

对于这个 WebSocket 的服务器响应验证过程，Erlang 的实现如下：

    
    
    HeaderList = binary:split(HeaderData, <<"\r\n">>, [global]),
    HeaderList1 = [list_to_tuple(binary:split(Header, <<": ">>)) || Header >) /= nomatch],
    {_, SecWebSocketKey} = lists:keyfind(<<"Sec-WebSocket-Key">>, 1, HeaderList1),
    Sha1 = crypto:sha([SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
    Base64 = base64:encode(Sha1),
    Handshake = [
        <<"HTTP/1.1 101 Switching Protocols\r\n">>,
        <<"Upgrade: websocket\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
        <<"\r\n">>
    ],
    gen_tcp:send(Socket, Handshake),
    

**WebSocket API**

WebSocket 使用自有的 ws 协议（加密的是 wss）来进行客户端浏览器与服务器间的通讯，它是这样一个写法格式：

    
    
    ws://服务器地址:端口/
    

例如想连接本地 localhost 的 12345 端口，可以写成这样：

    
    
    ws://localhost:12345/
    

建立一个 WebSocket 连接，只需实例一个 WebSocket 对象，并把一个 ws 协议参数传入：

    
    
    var ws = new WebSocket("ws://localhost:12345");
    

当 WebSocket 连接成功，则会触发 onopen 函数，例如：

    
    
    ws.onopen = function() {
        $("#content").append("websocket connected!");
    };
    

当 WebSocket 断开，则会触发 onclose 函数，例如：

    
    
    ws.onclose = function() {
        $("#content").append("websocket closed!");
    };
    

当收到服务器传送过来的数据，则会触发 onmessage 函数，例如：

    
    
    ws.onmessage = function(evt) {
        $("#content").append(evt.data);
    };
    

当 WebSocket 连接报错，则会触发 onerror 函数，例如：

    
    
    ws.onerror = function(error) {
        $("#content").append(error);
    };
    

如果要向服务器发送信息，可以调用 send 函数：

    
    
    ws.send(data);
    

WebSocket 支持接收 Blob 或 ArrayBuffer 这两种二进制格式的数据，默认格式为 Blob，如果要指定接收的二进制数据的格式，可把
WebSocket 对象的 binaryType 属性改为 "blob" 或 "arraybuffer"，例如：

    
    
    ws.binaryType = "arraybuffer";
    

**一个简单 Erlang 连接 WebSocket Demo**

前端浏览器连接代码 ws.html：

    
    
    <!DOCTYPE html>
    <html>
    <head>
    <meta http-equiv="Content-type" content="text/html;charset=UTF-8" />
    <title>Erlang WebSocket Demo</title>
    <style ="text/css">
    #main {
    	margin: 0 auto;
    	width: 500px;
    	height: 300px;
    }
    #main h1 {
    	text-align: center;
    }
    #content {
    	height: 300px;
    	border: 1px solid #EDEDED;
    	padding-left: 5px;
    	overflow: auto;
    }
    </style>
    </head>
    <body>
    
    <div id="main">
    	<h1>Erlang WebSocket Demo</h1>
    	<div id="content">
    	
    	</div>
    	<div>
    		<p>
    			<input id="msg"size="42" />
    			<input type="submit" value="发 送" id="smt" />
    		</p>
    	</div>
    </div>
    
    <script src="http://code.jquery.com/jquery-1.10.0.min.js" type="text/javascript"></script>
    <script type="text/javascript">
    $(document).ready(function() {
    	if ("WebSocket" in window) 
    	{
    		var ws = new WebSocket("ws://localhost:12345");
    		ws.onopen = function() {
    			$("#content").append("<p style='color: #80ff00;'>websocket connected!</p>");
    		};
    		ws.onmessage = function (evt) {
    			var data = evt.data;
    			$("#content").append("<p>" +  data + "</p>");
    		};
    		ws.onclose = function() {
    			$("#content").append("<p style='color: #ff3737;'>websocket closed!</p>");
    		};
    		$("#msg").change(function() {
    			var val = $(this).val();
    			if ( val )
    			{
    				ws.send(val);
    			}
    			return false;
    		});
    		$("#smt").click(function() {
    			var val = $("#msg").val();
    			if ( val )
    			{
    				ws.send(val);
    			}
    			return false;
    		});
    	} 
    	else 
    	{
    		$("#content").append("<p style='color: #ff3737;'>Your browser don't support WebSocket!</p>");
    	};
    });
    </script>
    </body>
    </html>
    

后台 Erlang 代码 ws.erl

    
    
    -module(ws).
    -export([start/0]).
    
    -define(PORT, 12345).
    
    start() ->
        {ok, Listen} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
    	io:format("listen on ~p~n", [?PORT]),
        par_connect(Listen).
    
    par_connect(Listen) ->
    	{ok, Socket} = gen_tcp:accept(Listen),
    	spawn(fun() -> par_connect(Listen) end),
    	wait(Socket).
    
    wait(Socket) ->
        receive
            {tcp, Socket, HeaderData} ->
                HeaderList = binary:split(HeaderData, <<"\r\n">>, [global]),
                HeaderList1 = [list_to_tuple(binary:split(Header, <<": ">>)) || Header >) /= nomatch],
                {_, SecWebSocketKey} = lists:keyfind(<<"Sec-WebSocket-Key">>, 1, HeaderList1),
                Sha1 = crypto:sha([SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
                Base64 = base64:encode(Sha1),
                Handshake = [
                    <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                    <<"Upgrade: websocket\r\n">>,
                    <<"Connection: Upgrade\r\n">>,
                    <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
                    <<"\r\n">>
                ],
                gen_tcp:send(Socket, Handshake),
                loop(Socket);
            Any ->
                io:format("Received: ~p~n", [Any]),
                wait(Socket)
        end.
    
    loop(Socket) ->
        receive
            {tcp, Socket, Data} ->
                handle_data(Data, Socket);
            {tcp_closed, Socket} ->
    			gen_tcp:close(Socket);
            Any ->
                io:format("Received:~p~n", [Any]),
                loop(Socket)
        end.
    
    unmask(Payload, Masking) ->
        unmask(Payload, Masking, <<>>).
    
    unmask(Payload, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
        case size(Payload) of
            0 -> Acc;
            1 ->
                <> = Payload,
                <<Acc/binary, (MA bxor A)>>;
            2 ->
                <<A:8, B:8>> = Payload,
                <<Acc/binary, (MA bxor A), (MB bxor B)>>;
            3 ->
                <<A:8, B:8, C:8>> = Payload,
                <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
            _Other ->
                <<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
                Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
                unmask(Rest, Masking, Acc1)
        end.
    
    handle_data(Data, Socket) ->
        <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7, Rest/binary>> = Data,
        <<Masking:4/binary, Payload:Len/binary, Next/binary>> = Rest,
        Line = unmask(Payload, Masking),
    	case unicode:characters_to_list(Line) of
    		{incomplete, _, _} ->
    			gen_tcp:close(Socket);
    		Str ->
    			Bin = unicode:characters_to_binary(Str),
    		 	Frame = <<1:1, 0:3, 1:4, 0:1, (size(Bin)):7, Bin/binary>>,
    		 	gen_tcp:send(Socket, Frame),
    		    case size(Next) of
    		        0 -> loop(Socket);
    		        _Other -> handle_data(Next, Socket)
    		    end
    	end.
    

