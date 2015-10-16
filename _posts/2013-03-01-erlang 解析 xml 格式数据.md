---
layout: page
title: erlang 解析 xml 格式数据
---
{% include JB/setup %}

对于 [xml](http://www.w3.org/XML/) 格式数据的操作，erlang 官方内置（built-in）了七个相应解析操作处理 xml 
的模块：[xmerl_scan](http://www.erlang.org/doc/man/xmerl_scan.html)、[xmerl](http:/
/www.erlang.org/doc/man/xmerl.html)、[xmerl_xs](http://www.erlang.org/doc/man/x
merl_xs.html)、[xmerl_eventp](http://www.erlang.org/doc/man/xmerl_eventp.html)、
[xmerl_xpath](http://www.erlang.org/doc/man/xmerl_xpath.html)、[xmerl_xsd](http
://www.erlang.org/doc/man/xmerl_xsd.html)、[xmerl_sax_parser](http://www.erlang
.org/doc/man/xmerl_sax_parser.html)。

以上每个模块都在相应的情景需求里发挥着相应的效用，不过这里不对这些模块做一一介绍，这里只对下面这种情景需求做说明：

假设有一个名为 fruit.xml 的文件，内容如下：

    
    
    <?xml version="1.0" encoding="utf-8" ?>
    
    <fruit>
        <apple price="1">10</apple>
        <orange price="2">20</orange>
        <pear price="3">30</pear>
    </fruit>

现在需要用 erlang 把它解析成如下格式的列表：

    
    
    [{apple, 1, 10}, {orange, 2, 20}, {pear, 3, 30}]

**xmerl_scan:file/1 和 xmerl_scan:file/2**

顾名思义，就是读取一个 xml 格式的文件，如果是一个 xml 格式的文档就会对它分析处理，并返回一个 xmlElement 记录（record）。

其实每一个 xml 标签都是一个 xmlElement 记录

    
    
    %% XML Element
    %% content = [#xmlElement()|#xmlText()|#xmlPI()|#xmlComment()|#xmlDecl()]
    -record(xmlElement,{
    	  name,			% atom()
    	  expanded_name = [],	% string() | {URI,Local} | {"xmlns",Local}
    	  nsinfo = [],	        % {Prefix, Local} | []
    	  namespace=#xmlNamespace{},
    	  parents = [],		% [{atom(),integer()}]
    	  pos,			% integer()
    	  attributes = [],	% [#xmlAttribute()]
    	  content = [],
    	  language = "",	% string()
    	  xmlbase="",           % string() XML Base path, for relative URI:s
    	  elementdef=undeclared % atom(), one of [undeclared | prolog | external | element]
    	 }).
    

标签里的属性是一个 xmlAttribute 记录

    
    
    %% Attribute
    -record(xmlAttribute,{
    	  name,		   % atom()
    	  expanded_name=[],% atom() | {string(),atom()}
    	  nsinfo = [],	   % {Prefix, Local} | []
    	  namespace = [],  % inherits the element's namespace
    	  parents = [],	   % [{atom(),integer()}]
    	  pos,		   % integer()
    	  language = [],   % inherits the element's language
    	  value,	   % IOlist() | atom() | integer()
    	  normalized       % atom() one of (true | false)
    	 }).
    

标签的文本值是一个 xmlText 记录

    
    
    %% plain text
    %% IOlist = [char() | binary () | IOlist]
    -record(xmlText,{
    	  parents = [],	% [{atom(),integer()}]
    	  pos,		% integer()
    	  language = [],% inherits the element's language
    	  value,	% IOlist()
    	  type = text   % atom() one of (text|cdata)
    	 }).
    

xmerl_scan:file 的第二个参数是设置指定分析处理 xml 时的一些选项，例如指定被读取分析 xml 的数据编码。下面是使用
xmerl_scan:file 函数返回上面所说的列表格式的代码：

    
    
    -module(xmerl_scan_file).
    -export([test/1]).
    
    -include_lib("xmerl/include/xmerl.hrl").
    
    
    test(FileName) ->
        {XmlElement, _Rest} = xmerl_scan:file(FileName, [{encoding, 'utf-8'}]),
        Ret = parse(XmlElement, []),
        lists:flatten(Ret).
    
    parse(XmlElement, Ret) when is_record(XmlElement, xmlElement) ->
        case XmlElement#xmlElement.name of
            apple ->
                Price = get_price_value(XmlElement#xmlElement.attributes),
                Num = lists:foldl(fun parse/2, 0, XmlElement#xmlElement.content),
                [{apple, Price, Num} | Ret];
            orange ->
                Price = get_price_value(XmlElement#xmlElement.attributes),
                Num = lists:foldl(fun parse/2, 0, XmlElement#xmlElement.content),
                [{orange, Price, Num} | Ret];
            pear ->
                Price = get_price_value(XmlElement#xmlElement.attributes),
                Num = lists:foldl(fun parse/2, 0, XmlElement#xmlElement.content),
                [{pear, Price, Num} | Ret];
            _ -> 
                lists:foldl(fun parse/2, Ret, XmlElement#xmlElement.content)
        end;
    parse(#xmlText{ parents = [{apple, _} | _], value = V}, _Ret) ->
        V;
    parse(#xmlText{ parents = [{orange, _} | _], value = V}, _Ret) ->
        V;
    parse(#xmlText{ parents = [{pear, _} | _], value = V}, _Ret) ->
        V;
    parse(_, Ret) -> 
        Ret.
    
    get_price_value(XmlElementAttr) ->
        Fun = fun(X) -> X#xmlAttribute.name == price end,
        XmlAttribute = hd(lists:filter(Fun, XmlElementAttr)),
        XmlAttribute#xmlAttribute.value.
    

理清了 xml 的嵌套关系，是很容易操作 xml 里的数据的（也无非是处理元组与列表的关系）。如果不清楚返回的 XmlElement 的数据格式的，可以
IO 输出一下，理清之间的嵌套关系之后，接下来的事就是根据数据的格式写相应的分析代码了。

**xmerl_scan:string/1 和 xmerl_scan:string/2**

跟 xmerl_scan:file 一样，都会返回一个 xmlElement 记录。不同的是，xmerl_scan:string 是读取格式是 xml
的文本字符。第二参数也是跟 xml_scan:file 一样，都是指定分析处理 xml 时的一些选项。

下面是使用 xmerl_scan:string 函数的代码：

    
    
    test() ->
        XmlString = "
    <fruit>
        <apple price=\"1\">10</apple>
        <orange price=\"2\">20</orange>
        <pear price=\"3\">30</pear>
    </fruit>   
        ",
        {XmlElement, _Rest} = xmerl_scan:string(XmlString),
        Ret = parse(XmlElement, []),
        lists:flatten(Ret).

上面的 parse 函数跟介绍 xmerl_scan:file 时的 parse 函数是一样的，这里就不多写了。

使用 xmerl_scan:string 要注意的是，平常 xml 文件里的第一行描述声明：

    
    
    <?xml version="1.0" encoding="utf-8" ?>

在 xmerl_scan:string里是不被看做是 xml 格式的字符的，所以文本字符里是不能这串 xml 的描述声明字符。

**xmerl_xpath:string**

利用 [XPath](http://www.w3.org/TR/xpath/) 路径表达式查询解析 xml 数据，例如获取符合标签名是 apple，价格是
"1" 且包含 "10" 的 xml 标签：

    
    
    > XmlString = "
    > <fruit>
    >     <apple price=\"1\">10</apple>
    >     <orange price=\"2\">20</orange>
    >     <pear price=\"3\">30</pear>
    > </fruit>   
    > ",
    > {XmlElement, _Rest} = xmerl_scan:string(XmlString),
    > xmerl_xpath:string("//apple[@price='1' and . = '10']", XmlElement).
    [{xmlElement,apple,apple,[],
                 {xmlNamespace,[],[]},
                 [{fruit,1}],
                 2,
                 [{xmlAttribute,price,[],[],[],
                                [{apple,2},{fruit,1}],
                                1,[],"1",false}],
                 [{xmlText,[{apple,2},{fruit,1}],1,[],"10",text}],
                 [],"/Users/dengjoe/temp/xml",undeclared}]

或者获取标签名是 apple 且包含 "10" 的标签文本：

    
    
    > xmerl_xpath:string("//apple[. = '10']/text()", XmlElement).
    [{xmlText,[{apple,2},{fruit,1}],1,[],"10",text}]

上面返回包含 xmlElement 记录的列表或者包含 xmlText 记录的列表（不符合条件则返回一个空列表
"[]"），但是想要获取上面所说的列表格式，还需要进一步处理，不过 xmerl_xpath:string
已经给出相应的记录数据，要分析出来也是很容易的，下面是利用 xmerl_xpath:string 的实现代码：

    
    
    -module(xmerl_xpath_string).
    -export([test/0]).
    
    -include_lib("xmerl/include/xmerl.hrl").
    
    
    test() ->
         XmlString = "
    <fruit>
        <apple price=\"1\">10</apple>
        <orange price=\"2\">20</orange>
        <pear price=\"3\">30</pear>
    </fruit>   
        ",
        {XmlElement, _Rest} = xmerl_scan:string(XmlString),
        Fun = fun(Atom, Ret) ->
            XPath = "//" ++ atom_to_list(Atom), 
            AtomList = xmerl_xpath:string(XPath, XmlElement),
            [parse(AtomList, Atom, []) | Ret]
        end,
        lists:flatten(lists:foldl(Fun, [], [apple, orange, pear])).
    
    parse([], _Atom, Ret) ->
        Ret;
    parse([XmlElement | X], Atom, Ret) ->
        case is_record(XmlElement, xmlElement) of
            true ->
                Price = get_price_value(XmlElement#xmlElement.attributes),
                XmlText = hd(XmlElement#xmlElement.content),
                parse(X, Atom, [{Atom, Price, XmlText#xmlText.value} | Ret]);
            false ->
                parse(X, Atom, Ret)
        end.
    
    get_price_value(XmlElementAttr) ->
        Fun = fun(X) -> X#xmlAttribute.name == price end,
        XmlAttribute = hd(lists:filter(Fun, XmlElementAttr)),
        XmlAttribute#xmlAttribute.value.
    

**后记**

感觉 erlang 解析 xml 还是很麻烦，如果遇到结构很复杂的 xml 数据，要理清 xmlElement 之间的嵌套关系，那是要吐血！虽然有
XPath 可以筛选，不过筛选出的数据还需要自己进一步分析处理，还是不是很方便。嗯，真怀念 javascript 里的文件对象模型
[DOM(Document Object Model)](http://www.w3.org/DOM/) 对 xml 数据的操作。

