---
layout: page
title: Zotonic 模块制作 -- HTML Sitemap
---
{% include JB/setup %}

其实 Zotonic 已经有一个sitemap 的模块 -- mod_seo_sitemap，在 /moudules/mod_seo_sitemap
目录下，不过这个模块只生成 xml 格式的 [sitemap](http://dhq.me/sitemap.xml)，不方便浏览。恰好之前了解了下[
Zotonic 模块结构](http://dhq.me/zotonic-module-structure) 一些相关知识，学以致用，所以就仿照
mod_seo_sitemap 模块，做一个 HTML 版的sitemap -- mod_html_sitemap。

下面是 mod_html_sitemap 模块的目录结构图：

    
    
    |----mod_sitemap_html
    | |----dispatch
    | | |----dispatch
    | |----mod_sitemap_html.erl
    | |----templates
    | | |----_sitemap_html.tpl
    | | |----sitemap_html.tpl
    

**mod_sitemap_html**:

以 mod_sitemap_html 为命名的模块文件夹。

**mod_sitemap_html.erl**:

模块的初始文件，跟模块名一样。定义模块的一些初始信息，例如模块标题、作者、描述、权值(priority)等等，这些信息将作为模块的描述信息出现在后台模块管理
页面上。

    
    
    -module(mod_sitemap_html).
    -author("dhq.me").
    
    -mod_title("HTML Sitemap").
    -mod_description("Generates sitemap for a friendly view.").
    -mod_prio(600).
    

**dispatch/dispatch**
    
    
    %% -*- mode: erlang -*-
    [
    	{sitemap_html, ["sitemap.html"], controller_template, [{template, "sitemap_html.tpl"}, {content_type, "text/html"}, {anonymous, true}]}
    ].
    

sitemap_html: 该路由调度规则的名称标识

["sitemap.html"]: 匹配来自 sitemap.html 的url请求

controller_template: 使用 controller_template 作为模板控制器

[{template, "sitemap_html.tpl"}, {content_type, "text/html"}, {anonymous,
true}]: 标识模板路径，以及该模板内容形式，并允许匿名访问

URL路由调度规则 dispatch
的更多信息可以查看[这里](http://zotonic.com/docs/latest/manuals/dispatch.html)。

**templates/sitemap_html.tpl**
    
    
    {% cache 3600 sitemap_html cat="article" %}
    	{% with m.search[{query cat="article" is_published='true' sort='-rsc.publication_start'}] as result %}
    		{% include "_sitemap_html.tpl" %}
    	{% endwith %}
    {% endcache %}
    

这里使用了 [ErlyDTL](http://zotonic.com/page/520/erlydtl) 的模板缓存 [cache](http://zoto
nic.com/docs/latest/ref/tags/tag_cache.html)，缓存时间是一个小时(3600秒)，sitemap_html是该缓存
名。并使用 [m_search](http://zotonic.com/documentation/726/m-search) 模型方法，把类型是
article
的已发布的文章，按文章发布日期(publication_start，rsc表的一个字段)的倒序查询出来。cat、is_published、sort是查询模式
query 的选项，更多的选项说明可以查看[这里](http://zotonic.com/documentation/761/the-query-
search-model)。

**templates/_sitemap_html.tpl**
    
    
    <!DOCTYPE html>
    <html>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>SiteMap - {{ m.config.site.title.value }}</title>
    <meta name="keywords" content="SiteMap,{{ m.config.site.title.value }}" />
    <link rel="profile" href="http://gmpg.org/xfn/11" />
    <style type="text/css">
    body {
    	margin: 0;
    	font-family: Verdana;
    	font-size: 12px;
    }
    a, a:visited {
    	color: #333333;
    }
    li {
    	margin-top: 8px;
    }
    #nav, #content, #footer {
    	padding: 8px; 
    	border: 1px solid #EEEEEE; 
    	clear: both; 
    	width: 95%; 
    	margin: auto; 
    	margin-top: 10px;
    }
    </style>
    </head>
    <body>
    
    {% with m.site.hostname|default:"localhost" as hostname %}
    
    <h2 style="text-align: center; margin-top: 20px">{{ m.config.site.title.value }}'s SiteMap </h2>
    <div id="nav">
    	<a href="http://{{ hostname }}"><strong>{{ m.config.site.title.value }}</strong></a>  &raquo; <a href="http://{{ hostname }}/sitemap.html">SiteMap</a>
    </div>
    <div id="content">
    <h3>文章列表</h3>
    <ul>
    
    	{% for id in result %}
    		{% if not m.rsc[id].seo_noindex %}
    			{% with m.rsc[id].page_url as page_url %}
    				{% ifnotequal page_url "/" %}
    	<li><a href="http://{{ hostname }}{{ page_url|escapexml }}" title="{{ m.rsc[id].title }}" target="_blank">{{ m.rsc[id].title }}</a></li>
    				{% endifnotequal %}
    			{% endwith %}
    		{% endif %}
    	{% endfor %}
    
    </ul>
    </div>
    
    <center>
    <br />
    <br />
    <div style="text-algin: center; font-size: 11px">
    	<strong><a href="http://{{ hostname }}/sitemap.xml" target="_blank">SiteMap</a></strong>
    </div>
    <br />
    <br />
    </center>
    
    {% endwith %}
    </body>
    </html>

sitemap 模板的 html 代码，主要使用了[Zotonic的模板标签(tag)](http://zotonic.com/template-tags)
\-- [for](http://zotonic.com/docs/latest/ref/tags/tag_for.html)，把文章数据循环输出来。

该模块可以从 [github](https://github.com/dhq314/mod_sitemap_html) 上下载，把模块文件夹放在/priv/
sites/网站名/modules(或者"modules/"、"priv/modules")目录下，打开后台模块页面(System/Modules)激活模块
，就可以通过 [http://你的域名/sitemap.html](http://dhq.me/sitemap.html) 访问了。

PS：mac 没有生成目录文件结构的 tree 命令，不过还好，在网上找到了一个生成目录结构的 shell 指令

    
    
    find . -print | sed -e 's;[^/]*/;|----;g;s;----|; |;g'

