---
layout: page
title: CentOS（Fedora）下源代码编译安装 erlang 环境的 shell 脚本
---
{% include JB/setup %}

填上 erlang 源代码下载地址（erl_url）和安装路径（prefix），然后执行这个 shell 脚本就行。

    
    
    #!/bin/sh
    
    
    
    erl_url="http://www.erlang.org/download/otp_src_R16B.tar.gz"
    
    prefix="/usr/local/erlang" 
    
    
    yum update -y
    yum upgrade -y
    
    #yum groupinstall -y "Development Tools" "Perl Support"
    yum groupinstall -y "Development Tools"
    
    yum install -y kernel-devel m4 fop ncurses-devel openssl-devel tk unixODBC unixODBC-devel
    
    if [ ! -s "./erlang_otp.tar.gz" ]; then
        wget -c -O erlang_otp.tar.gz $erl_url
    fi
    
    tar zxvf erlang_otp.tar.gz
    cd ./otp_src*/
    
    ./configure --prefix=$prefix --without-javac \
        --enable-kernel-poll \
        --enable-threads \
        --enable-dynamic-ssl-lib \
        --enable-shared-zlib \
        --enable-smp-support \
        --enable-hipe 
    
    make && make install
    
    #if [ ! -s "~/.bashrc" ]; then
    #    touch ~/.bashrc
    #fi
    #echo 'export PATH="/usr/local/erlang/bin:$PATH"' >> ~/.bashrc
    #source ~/.bashrc
    ln -sfv $prefix/bin/* /usr/local/bin/
    
    yum clean all
    
    erl -noshell -eval "erlang:display('Erlang Success Install')" -s init stop
    

上面脚本在 CentOS 6.4 和 Fedora 17 下正常运行安装。

嗯，上面的脚本改下安装指令后，也同样适合在类debian（例如Ubuntu）系统下执行：

    
    
    #!/bin/sh
    
    
    
    erl_url="http://www.erlang.org/download/otp_src_R16B.tar.gz"
    prefix="/usr/local/erlang"
    
    apt-get update
    apt-get upgrade -y --show-upgraded
     
    apt-get install -y build-essential
    
    apt-get install -y m4 fop libncurses5-dev openssl libssl-dev tk unixodbc unixodbc-dev freeglut3-dev libwxgtk2.8-dev xsltproc
    
    if [ ! -s "./erlang_otp.tar.gz" ]; then
        wget -c -O erlang_otp.tar.gz $erl_url
    fi
    
    tar zxvf erlang_otp.tar.gz
    cd ./otp_src*/
    
    ./configure --prefix=$prefix --without-javac \
        --enable-kernel-poll \
        --enable-threads \
        --enable-dynamic-ssl-lib \
        --enable-shared-zlib \
        --enable-smp-support \
        --enable-hipe 
    
    make && make install
    
    ln -sfv $prefix/bin/* /usr/local/bin/
    
    apt-get autoclean -y           
    apt-get clean -y            
    apt-get autoremove -y
    
    erl -noshell -eval "erlang:display('Erlang Success Install')" -s init stop
    

PS：发现用上面脚本在类 debian 系统下安装，需要把系统 Swap 设到最大的 512M（linode 默认的 Swap 是 256M，最大是
512M），不然编译时会报 “g++: internal compiler error: Killed (program cc1plus)” 的错误:(

可能之后会在这基础上继续丰富该脚本的内容，或者是第三方 erlang 库的安装，或者是添加一些应用依赖，或者是其他运行环境的搭建，最新的修改可以从
[gist](https://gist.github.com/dhq314/5226368) 上查看得到。

