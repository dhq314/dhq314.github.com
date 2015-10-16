---
layout: page
title: Debian / CentOS 系统下的 IPSEC + L2TP VPN 一键安装部署
---
{% include JB/setup %}

最近的免费的 [VPN（Virtual Private
Network）](http://en.wikipedia.org/wiki/Virtual_private_network)
很不给力，之前在网上看到的由筑波大学免费提供的 VPN -- [VPN
Gate](http://www.vpngate.net/)，开始用着还好，不过用了一段时间后不知为什么，很难连接上去，况且用得也不爽，速度慢死！还好自己在
[Linode](http://www.linode.com/?r=4979bb47b3357142334154628b7d0176b3dff63f)
上有一个 [VPS（Virtual Private Server）](http://en.wikipedia.org/wiki/Virtual_privat
e_server)，免费的难用，那就自架一个呗，自力更生，丰衣足食。

对于 VPN 类型的选择，之前试装过 [PPTP](http://en.wikipedia.org/wiki/Point-to-
Point_Tunneling_Protocol)（点对点隧道协议，Point to Point Tunneling Protocol，默认使用 1723
端口，连接协议是 TCP）的 VPN，不过由于 Mac 下 [MTU（最大传输单元，Maximum Transmission
Unit）](http://en.wikipedia.org/wiki/Maximum_transmission_unit) 的配置问题，在 mac 下
PPTP 的 VPN 一直没链接成功，所以 VPN 类型选用的是
[L2TP](http://en.wikipedia.org/wiki/Layer_2_Tunneling_Protocol)（第二层隧道协议，Layer
Two Tunneling Protocol，默认使用 1701 端口，连接协议是 UDP）。因为 L2TP 没有数据加密功能，因此需要安装
[IPsec（Internet Protocol Security）](http://en.wikipedia.org/wiki/IPsec) 来加密
L2TP 的数据包。

其实以前也安装配置过 L2TP 的 VPN，不过上次被 GFW 封了 IP，Linode 换了新 IP 之后，之前配置的 VPN 就用不了，需要更换 VPN
配置里的 IP 参数值才能再次使用。每次都手动配置也很麻烦，所以搞了个懒人安装配置的脚本，省心，安逸，也方便以后安装配置，或者是像我这种被封 IP
后执行下脚本又能正常使用。嗯，能自动完成的东西就没必要手动折腾了。

详细的安装过程看下面注释，这里就不赘述了。

Debian 系统安装脚本（在 Linode 上的 Debian 6 和  Ubutun12.04、Ubuntu12.10 正常安装使用）：

    
    
    #!/bin/sh
    
    #VPN 账号
    vpn_name="test"
    
    #VPN 密码
    vpn_password="test123456"
    
    #设置 PSK 预共享密钥
    psk_password="test123456"
    
    #获取公网IP
    ip=`ifconfig | grep 'inet addr:' | grep -v '127.0.0.1' | cut -d: -f2 | awk '{ print $1}'`
    
    
    #安装 openswan、xl2tpd(有弹对话框的话直接按回车就行)
    apt-get install -y openswan xl2tpd screen
    
    
    #备份 /etc/ipsec.conf 文件
    ipsec_conf="/etc/ipsec.conf"
    if [ -f $ipsec_conf ]; then
        cp $ipsec_conf $ipsec_conf.bak 
    fi
    echo "
    version 2.0
    config setup
        nat_traversal=yes
        virtual_private=%v4:10.0.0.0/8,%v4:192.168.0.0/16,%v4:172.16.0.0/12
        oe=off
        protostack=netkey
    
    conn L2TP-PSK-NAT
        rightsubnet=vhost:%priv
        also=L2TP-PSK-noNAT
    
    conn L2TP-PSK-noNAT
        authby=secret
        pfs=no
        auto=add
        keyingtries=3
        rekey=no
        ikelifetime=8h
        keylife=1h
        type=transport
        left=$ip
        leftprotoport=17/1701
        right=%any
        rightprotoport=17/%any
        dpddelay=40
        dpdtimeout=130
        dpdaction=clear
    " > $ipsec_conf
    
    
    
    #备份 /etc/ipsec.secrets 文件
    ipsec_secrets="/etc/ipsec.secrets"
    if [ -f $ipsec_secrets ]; then
        cp $ipsec_secrets $ipsec_secrets.bak 
    fi
    echo "
    $ip   %any:  PSK \"$psk_password\"
    " >> $ipsec_secrets
    
    
    
    #备份 /etc/sysctl.conf 文件
    sysctl_conf="/etc/sysctl.conf"
    if [ -f $sysctl_conf ]; then
        cp $sysctl_conf $sysctl_conf.bak 
    fi
    echo "
    net.ipv4.ip_forward = 1
    net.ipv4.conf.all.send_redirects = 0
    net.ipv4.conf.default.send_redirects = 0
    net.ipv4.conf.all.accept_redirects = 0
    net.ipv4.conf.default.accept_redirects = 0
    " >> $sysctl_conf
    sysctl -p
    
    for each in /proc/sys/net/ipv4/conf/*
    do
        echo 0 > $each/accept_redirects
        echo 0 > $each/send_redirects
    done
    
    
    #设置 l2tp
    xl2tpd="/etc/xl2tpd/xl2tpd.conf"
    if [ -f $xl2tpd ]; then
        cp $xl2tpd $xl2tpd.bak 
    fi
    echo "
    [global]
    ipsec saref = yes
    
    [lns default]
    ip range = 10.1.2.2-10.1.2.255
    local ip = 10.1.2.1
    refuse chap = yes
    refuse pap = yes
    require authentication = yes
    ppp debug = yes
    pppoptfile = /etc/ppp/options.xl2tpd
    length bit = yes
    " > $xl2tpd
    
    
    #设置 ppp
    options_xl2tpd="/etc/ppp/options.xl2tpd"
    if [ -f $options_xl2tpd ]; then
        cp $options_xl2tpd $options_xl2tpd.bak 
    fi
    echo "
    require-mschap-v2
    ms-dns 8.8.8.8
    ms-dns 8.8.4.4
    asyncmap 0
    auth
    crtscts
    lock
    hide-password
    modem
    debug
    name l2tpd
    proxyarp
    lcp-echo-interval 30
    lcp-echo-failure 4
    " > $options_xl2tpd
    
    #添加 VPN 账号
    chap_secrets="/etc/ppp/chap-secrets"
    if [ -f $chap_secrets ]; then
        cp $chap_secrets $chap_secrets.bak 
    fi
    echo "
    $vpn_name * $vpn_password *
    " >> $chap_secrets
    
    
    #设置 iptables 的数据包转发
    iptables --table nat --append POSTROUTING --jump MASQUERADE
    echo 1 > /proc/sys/net/ipv4/ip_forward
    
    
    /etc/init.d/ipsec stop
    
    /etc/init.d/xl2tpd stop
    
    /etc/init.d/ipsec start
    
    screen -dmS xl2tpd xl2tpd -D
    
    ipsec verify
    
    echo "###########################################"
    echo "##    L2TP VPN SETUP COMPLETE!"
    echo "##    VPN IP          :   $ip"
    echo "##    VPN USER        :   $vpn_name"
    echo "##    VPN PASSWORD    :   $vpn_password"
    echo "##    VPN PSK         :   $psk_password"
    echo "###########################################"
    
    

CentOS 系统安装脚本（在 Linode 上的 CentOS6.2、CentOS6.4 正常安装使用）：

    
    
    #!/bin/sh
    
    
    
    #VPN 账号
    vpn_name="test"
    
    #VPN 密码
    vpn_password="test123456"
    
    #设置 PSK 预共享密钥
    psk_password="test123456"
    
    #获取公网IP
    ip=`ifconfig | grep 'inet addr:' | grep -v '127.0.0.1' | cut -d: -f2 | awk '{ print $1}'`
    if [ ! -n "$ip" ]; then
        ip=`ifconfig | grep 'inet' | grep -v '127.0.0.1' | cut -d: -f2 | awk '{ print $2}'`
    fi
    
    
    yum install -y ppp iptables make gcc gmp-devel xmlto bison flex xmlto libpcap-devel lsof screen
    
    #安装openswan
    if [ ! -f "./openswan.tar.gz" ]; then
        wget -c -O openswan.tar.gz http://www.openswan.org/download/openswan-2.6.33.tar.gz
    fi
    tar -zxvf openswan.tar.gz
    cd ./openswan*/
    make programs install
    
    
    #备份 /etc/ipsec.conf 文件
    ipsec_conf="/etc/ipsec.conf"
    if [ -f $ipsec_conf ]; then
        cp $ipsec_conf $ipsec_conf.bak 
    fi
    echo "
    version 2.0
    config setup
        nat_traversal=yes
        virtual_private=%v4:10.0.0.0/8,%v4:192.168.0.0/16,%v4:172.16.0.0/12
        oe=off
        protostack=netkey
    
    conn L2TP-PSK-NAT
        rightsubnet=vhost:%priv
        also=L2TP-PSK-noNAT
    
    conn L2TP-PSK-noNAT
        authby=secret
        pfs=no
        auto=add
        keyingtries=3
        rekey=no
        ikelifetime=8h
        keylife=1h
        type=transport
        left=$ip
        leftprotoport=17/1701
        right=%any
        rightprotoport=17/%any
        dpddelay=40
        dpdtimeout=130
        dpdaction=clear
    " > $ipsec_conf
    
    
    
    #备份 /etc/ipsec.secrets 文件
    ipsec_secrets="/etc/ipsec.secrets"
    if [ -f $ipsec_secrets ]; then
        cp $ipsec_secrets $ipsec_secrets.bak 
    fi
    echo "
    $ip   %any:  PSK \"$psk_password\"
    " >> $ipsec_secrets
    
    
    
    #备份 /etc/sysctl.conf 文件
    sysctl_conf="/etc/sysctl.conf"
    if [ -f $sysctl_conf ]; then
        cp $sysctl_conf $sysctl_conf.bak 
    fi
    
    
    sed -i 's/net.ipv4.ip_forward = 0/net.ipv4.ip_forward = 1/g' /etc/sysctl.conf
    sysctl -p
    iptables --table nat --append POSTROUTING --jump MASQUERADE
    for each in /proc/sys/net/ipv4/conf/*
    do
        echo 0 > $each/accept_redirects
        echo 0 > $each/send_redirects
    done
    
    /etc/init.d/ipsec restart
    
    
    #安装rp-l2tp
    cd ~
    if [ ! -f "./rp-l2tp.tar.gz" ]; then
        wget -c -O rp-l2tp.tar.gz http://mirror.vpseek.com/sources/rp-l2tp-0.4.tar.gz
    fi
    tar -zxvf rp-l2tp.tar.gz
    cd ./rp-l2tp*/
    ./configure
    make
    cp handlers/l2tp-control /usr/local/sbin/
    mkdir -p /var/run/xl2tpd/
    ln -s /usr/local/sbin/l2tp-control /var/run/xl2tpd/l2tp-control
    #安装xl2tpd
    cd ~
    if [ ! -f "./xl2tpd.tar.gz" ]; then
        wget -c -O xl2tpd.tar.gz http://mirror.vpseek.com/sources/xl2tpd-1.2.4.tar.gz
    fi
    tar -zxvf xl2tpd.tar.gz
    cd ./xl2tpd*/
    make install
    
    
    
    mkdir -p /etc/xl2tpd
    xl2tpd="/etc/xl2tpd/xl2tpd.conf"
    if [ -f $xl2tpd ]; then
        cp $xl2tpd $xl2tpd.bak 
    fi
    echo "
    [global]
    ipsec saref = yes
    
    [lns default]
    ip range = 10.1.2.2-10.1.2.255
    local ip = 10.1.2.1
    refuse chap = yes
    refuse pap = yes
    require authentication = yes
    ppp debug = yes
    pppoptfile = /etc/ppp/options.xl2tpd
    length bit = yes
    " > $xl2tpd
    
    
    #设置 ppp
    options_xl2tpd="/etc/ppp/options.xl2tpd"
    if [ -f $options_xl2tpd ]; then
        cp $options_xl2tpd $options_xl2tpd.bak 
    fi
    echo "
    require-mschap-v2
    ms-dns 8.8.8.8
    ms-dns 8.8.4.4
    asyncmap 0
    auth
    crtscts
    lock
    hide-password
    modem
    debug
    name l2tpd
    proxyarp
    lcp-echo-interval 30
    lcp-echo-failure 4
    " > $options_xl2tpd
    
    
    
    #添加 VPN 账号
    chap_secrets="/etc/ppp/chap-secrets"
    if [ -f $chap_secrets ]; then
        cp $chap_secrets $chap_secrets.bak 
    fi
    echo "
    $vpn_name * $vpn_password *
    " >> $chap_secrets
    
    
    #设置 iptables 的数据包转发
    iptables --table nat --append POSTROUTING --jump MASQUERADE
    echo 1 > /proc/sys/net/ipv4/ip_forward
    
    screen -dmS xl2tpd xl2tpd -D
    
    ipsec verify
    
    echo "###########################################"
    echo "##    L2TP VPN SETUP COMPLETE!"
    echo "##    VPN IP          :   $ip"
    echo "##    VPN USER        :   $vpn_name"
    echo "##    VPN PASSWORD    :   $vpn_password"
    echo "##    VPN PSK         :   $psk_password"
    echo "###########################################"
    
    

# 一些问题

## 1、xl2tpd[5205]: Maximum retries exceeded for tunnel 36394. Closing.

第一次连没问题，过一会断了再连就连不成功，后台 log 出现上面的信息。

解决方法：在 /etc/ipsec.conf 里的 L2TP-PSK-noNAT 中加上 DPD（Dead Peer Dectection） 死连接检测

    
    
    dpddelay=40
    dpdtimeout=130
    dpdaction=clear
    

## 2、Ubutun L2TP IPsec 客户端管理器的安装

Ubuntu 的网络连接设置默认只有 PPTP 类型的连接，要想连 L2TP 的 VPN 需要另外安装 L2TP + IPsec 客户端管理器

    
    
    apt-add-repository ppa:werner-jaeger/ppa-werner-vpn
    apt-get update
    apt-get install l2tp-ipsec-vpn

具体配置查看这里 -- 《[Configuring and Running L2TP/IPSec VPN on Ubuntu
Linux](https://www.versavpn.com/ubuntu-linux-configuring-running-l2tp/)》

## 3、Checking that pluto is running [FAILED]

ipsec 如果没有启动，执行下面指令启动：

    
    
    ipsec setup start
    

