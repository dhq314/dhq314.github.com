---
layout: page
title: 用 Nginx 作为 Zotonic 的反向代理
---
{% include JB/setup %}

之前的 [Zotonic 安装配置](http://dhq.me/unbuntu-install-zotonic)时也说过，Zotonic
的默认访问端口是8000，就是访问网站的时候要在网站域名后面加上端口值8000，例如本站：<http://dhq.me:8000/>。这个值由
"/priv/config" 文件里的 listen_port 值决定，如果要改为像平常访问网站那样只填域名不用写上端口值8000访问的话，可以把
listen_port 的值改为80，重启 zotonic 就行（PS：有些系统（例如Mac）使用1024以下端口的话需要管理员权限，不然启动
zotonic 会失败）。

不过，如果你像我在
[linode](http://www.linode.com/?r=4979bb47b3357142334154628b7d0176b3dff63f) 的
VPS 上用 nginx 架设有其他非 zotonic 的网站，那么就需要用 nginx 来作反向代理。还好，官方有一篇介绍用nginx作反向代理的文档
-- 《[Proxying Zotonic with
nginx](http://zotonic.com/docs/latest/manuals/deployment/nginx.html)》。下面是我用
nginx 配置作反向代理的配置：

    
    
    server {
      	listen 80;
       	server_name dhq.me www.dhq.me;
    
        access_log off;
    
      	keepalive_timeout 65;
      	gzip on;
    
        location / {
            proxy_pass http://127.0.0.1:8000/;
            proxy_redirect off;
    
            proxy_set_header   Host             $host;
            proxy_set_header   X-Real-IP        $remote_addr;
            proxy_set_header   X-Forwarded-For  $proxy_add_x_forwarded_for;
    
            client_max_body_size       50m;
            client_body_buffer_size    128k;
    
            proxy_connect_timeout      90;
            proxy_send_timeout         90;
            proxy_read_timeout         90;
    
            proxy_buffer_size          4k;
            proxy_buffers              4 32k;
            proxy_busy_buffers_size    64k;
            proxy_temp_file_write_size 64k;
        }
    
        location /close-connection {
            keepalive_timeout 0;
            empty_gif;
        }
    
    }
    

不要忘了把网站配置文件(/priv/sites/网站名/config)里的 hostname
值里的8000去掉，不然网站链接的域名后面还会有8000端口显示出来。

除了以上用 nginx 作反向代理的方法，官方文档上还列举了使用[authbind](http://en.wikipedia.org/wiki/Authbi
nd)、[Varnish](https://www.varnish-cache.org/)等方法，这里就不多说了，详细请看[这里](http://zoton
ic.com/docs/latest/manuals/deployment/index.html)。

最后，重启 Nginx 反向代理就生效。

