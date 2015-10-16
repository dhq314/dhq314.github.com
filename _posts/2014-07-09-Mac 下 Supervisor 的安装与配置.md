---
layout: page
title: Mac 下 Supervisor 的安装与配置
---
{% include JB/setup %}

[Supervisor](http://supervisord.org/) 是一个类 unix 操作系统下的进程监控管理工具。

** 安装 Supervisor **

[Supervisor](https://github.com/Supervisor/supervisor) 是由 Python 写成，可用 Python
的包安装管理工具 [pip(Python Package Index)](https://pypi.python.org/pypi/pip) 直接安装：

    
    
    sudo pip install supervisor
    

** 配置 Supervisor **

Supervisor 的配置文件命名为 supervisord.conf，它为 supervisord(Supervisor 的主服务命令) 和
supervisorctl(Supervisor 的监控管理命令) 提供配置选项设置。 Supervisor 并不规定配置文件
supervisord.conf 的存放位置，Supervisor 服务启动的时候默认会在：

  * $CWD/supervisord.conf
  * $CWD/etc/supervisord.conf
  * /etc/supervisord.conf

这几个目录位置查找配置文件 supervisord.conf。Supervisor 也提供参数 "-c" 来指定配置文件的目录路径。

在终端输入 "echo_supervisord_conf" 命令可查看 Supervisor 的默认配置的内容。

生成一份默认的配置文件：

    
    
    echo_supervisord_conf > /etc/supervisord.conf
    

这里有选择的设置了一些配置，基本够用，配置如下：

    
    
    [inet_http_server]
    port = 127.0.0.1:9001
    username = dhq
    password = 123456
    
    [unix_http_server]
    file = /tmp/supervisor.sock
    chmod = 0700
    
    [supervisord]
    logfile = /Users/dengjoe/.supervisor/supervisord.log
    logfile_maxbytes = 50MB
    logfile_backups=10
    loglevel = info
    pidfile = /tmp/supervisord.pid
    nodaemon = False
    minfds = 1024
    minprocs = 200
    umask = 022
    identifier = supervisor
    directory = /tmp
    nocleanup = true
    childlogdir = /tmp
    
    [supervisorctl]
    serverurl = unix:///tmp/supervisor.sock
    
    [rpcinterface:supervisor]
    supervisor.rpcinterface_factory = supervisor.rpcinterface:make_main_rpcinterface
    
    
    
    [program:shadowsocks]
    directory = /Users/dengjoe/shadowsocks
    command = /usr/bin/python /Users/dengjoe/shadowsocks/local.py
    autostart = true
    autorestart = true
    

**启动 Supervisor**
    
    
    supervisord -c /etc/supervisord.conf
    

参数 "-c" 表示指定 Supervisor 配置文件的路径

把 supervisord 加入系统启动服务

    
    
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
        <key>KeepAlive</key>
        <dict>
            <key>SuccessfulExit</key>
            <false/>
        </dict>
        <key>Label</key>
        <string>dengjoe.supervisord</string>
        <key>ProgramArguments</key>
        <array>
            <string>/usr/local/bin/supervisord</string>
            <string>-n</string>
            <string>-c</string>
            <string>/etc/supervisord.conf</string>
        </array>
        <key>RunAtLoad</key>
        <true/>
    </dict>
    </plist>
    

启动 Supervisor 服务：

    
    
    launchctl load ~/Library/LaunchAgents/dengjoe.supervisord.plist
    

**supervisorctl 监控命令**

supervisorctl 是 Supervisor 自带的后台进程控制工具，下面是该命令的一些用法：

启动应用：

    
    
    supervisorctl start program  
    

重新读取配置：

    
    
    supervisorctl update
    

