---
layout: page
title: zotonic 下的 wordpress 版 more 标签实现
---
{% include JB/setup %}

对于文章摘要，zotonic 在后台写作页面那里专门有一个栏，给你填写文章的大概内容摘要，当然，摘要显示的格式也是纯文本来了。写完了文章内容还要去写文章的摘
要，挺麻烦的。对于一个之前体验过 wordpress 的用户来说，是很怀念 wordpress 下的 more 标签。

**什么是 more 标签**

其实 more 标签只是一个内容是 "<!--more-->" 的 html 注释，因此这个 more
标签的内容是不会在文章正文中显示出来的，它允许你在文章正文中选择一个断点，插入 more 标签内容 -- "<!--
more-->"，这个断点的以上的文章内容将作为文章的摘要显示在首页上。

**wordpress 下 more 标签的实现**

也不难实现，就是以 more 标签为标记，对文章正文内容做一个截取了。wordpress 就是用 php 的正则匹配方法 -- preg_match
来实现的：

    
    
    preg_match('/<!--more(.*?)?-->/', $content, $matches)
    

'/<!--more(.*?)?-->/' 是匹配 more 标签的正则表达式，$content 是文章的内容，$matches 就是匹配成功后返回的结果。

**zotonic 下的实现**

zotonic 下可以用 re:split 方法对文章正文进行正则截取：

    
    
    [Summary | _] = re:split(Content, "<!--more(.*?)?-->", [{return, list}])
    

Content 是文章的正文，Summary 则是匹配后返回的摘要。

上面的匹配方法可以放在 /modules/mod_base/filters/filter_summary.erl 文件里，修改原来获取文章摘要的方法。

不过为了避免以后发生版本升级被覆盖的情况，这里在当前网站模块目录下另外新加一个模块 -- mod_read_more，结构如下：

    
    
    |----mod_read_more
    | |----filters
    | | |----filter_read_more.erl
    | |----mod_read_more.erl
    

嗯，模块的结构很简单，就1个 filters 文件夹、2个 erl 文件。

mod_read_more.erl 是该模块的初始文件，内容也没上面，就是定义模块的一些基本信息（这些信息会显示在后台模块管理页面上）：

    
    
    -module(mod_read_more).
    -author("Joe Deng <dhq314@gmail.com>").
    
    -mod_title("Customizing the Read More content").
    -mod_description("Customizing the Read More content, use like the wordpress more tag.").
    -mod_prio(600).
    
    

filter_read_more.erl 定义了一个过滤器（filter） -- read_more。对于定义了 moren 标签的，就截取 more
标签以上的文章内容。如果没有定义的话，就获取文章原来添加上的摘要。原来也没添加上摘要的话，就按一定的截取值（CutNum），截取文章的内容作为摘要：

    
    
    %% @author Joe Deng <dhq314@gmail.com>
    %% @doc Customizing the Read More content, use like the wordpress more tag.
    
    -module(filter_read_more).
    -export([read_more/2, read_more/3]).
    
    
    read_more(undefined, _Context) ->
        undefined;
    read_more(RId, Context) ->
        read_more(RId, 200, Context).
    
    read_more(undefined, _CutNum, _Context) ->
        undefined;
    read_more(RId, CutNum, Context) ->
        case m_rsc:rid(RId, Context) of
            undefined ->
                undefined;
            Id ->
    			case z_trans:lookup_fallback(m_rsc:p(Id, body, Context), Context) of
    				undefined ->
    					[];
    				Content ->
    					case re:run(Content, "<!--more(.*?)?-->", [{capture, first, list}]) of
    			            {match, _} ->
    							[Summary | _] = re:split(Content, "<!--more(.*?)?-->", [{return, list}]),
    							z_string:trim(Summary);
    			            _ ->
    							get_summary_text(Content, Id, CutNum, Context)
    			        end
    			end
        end.
    
    
    %% @doc 获取文章原来的摘要内容
    get_summary_text(Content, Id, CutNum, Context) ->
    	Summary =
    		case m_rsc:p(Id, summary, Context) of
            	{trans, _} = T -> z_trans:lookup_fallback(T, Context);
            	Other -> Other
        	end,
        S1 = 
    		case z_utils:is_empty(Summary) of
          		true -> z_html:strip(Content);
           		false -> Summary
       		end,
        z_string:trim(z_string:truncate(S1, z_convert:to_integer(CutNum))).
    

使用的方法也是很简单，对于自定义的过滤器，直接在模板上调用就行。例如原来的文章摘要模板文件 _article_summary.tpl
是这样调用原来的截取过滤器 summary 的：

    
    
    {{ id | summary }}
    

没错，上面自定义的过滤器 read_more 可以写成这样：

    
    
    {{ id | read_more }}
    

最后文章摘要模板文件 _article_summary.tpl 修改如下：

    
    
    <section class="post clearfix">
    
    	<a href="{{m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
    		{% image m.rsc[id].media[1] height=108 width=120 crop %}
    	</a>
    	<h1><a href="{{m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h1>
    	{% include "_article_meta.tpl" id=id %}
    	<p class="summary">
    		{{ id | read_more }}
    	</p>
    	<div class="post-continue"><a href="{{ m.rsc[id].page_url }}">{_ 继续阅读 _}&nbsp;&raquo;</a></div>
    	
    </section>

OK，以后在文章内容里加上 more 标签后，自动截取你想截取的文章摘要了。

PS：对于 zotonic 的后台写作，一直就想吐槽。编辑器 tinyMCE 新版的 bug 一不小心会让你编辑格式全乱套，反正现在多数情况直接 html
源代码编写，靠谱、省心。

