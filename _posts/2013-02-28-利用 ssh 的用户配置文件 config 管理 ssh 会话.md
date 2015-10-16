---
layout: page
title: 利用 ssh 的用户配置文件 config 管理 ssh 会话
---
{% include JB/setup %}

通常利用 [ssh](http://en.wikipedia.org/wiki/Secure_Shell) 连接远程服务器，一般都要输入以下类似命令：

    
    
    ssh user@hostname -p port
    

如果拥有多个 ssh 账号，特别是像我这种喜欢在终端里直接 ssh 登陆，不用 [PuTTY](http://www.putty.org/)、[Secure
CRT](http://www.vandyke.com/products/securecrt/) 之类的 ssh 客户端的，要记住每个ssh
账号的参数，那是多么蛋疼的事情。

还好，ssh 提供一种优雅且灵活的方式来解决这个问题，就是利用 ssh 的用户配置文件 config 管理 ssh 会话。ssh
的用户配置文件是放在当前用户根目录下的 .ssh 文件夹里（~/.ssh/config，不存在则新创建一个），其配置写法如下：

    
    
    Host    别名
        HostName        主机名
        Port            端口
        User            用户名
        IdentityFile    密钥文件的路径
    

有了这些配置，就可以这样用 ssh 登陆服务器了：

    
    
    ssh 别名

这里以免费的 ssh shell -- [CJB](http://www.cjb.net/shell.html) 为例，介绍指定密钥文件配置连接 ssh
的用法，如果没有可以在[这里](http://www.cjb.net/cgi-bin/shell.cgi?action=signup)注册一个账号。

这里假设你有一个名为 user 的 CJB ssh 账号，那么用户名就是 "user"，端口使用 ssh 默认的端口 "22"，CJB 默认使用的主机名是
"shell.cjb.net"，不过在镇里这个域名已经被 DNS 污染用不了，只能用 IP 地址 216.194.70.6 代替。

PS：上面的 CJB 主机名 IP 可以通过 dig 获得

    
    
    dig shell.cjb.net

使用密钥的好处就是省去每次 ssh 登陆服务器时都要输入登陆密码的操作，这里使用 ssh-keygen 生成 ssh 密钥（以下操作是在 ~/.ssh/
目录里执行）：

    
    
    ssh-keygen -t rsa

这里使用 rsa 的加密方式（另外一种加密方式是 dsa），中间会询问密钥生成的位置，这里只输入 cjb，在当前位置生成名为 cjb 的密钥，接着会询问是否
要设置一个密码(passphrase)，这里留空，直接按回车就行（本来就不想登陆输入密码了...），最后，会在当前目录路径下生成一个名为 cjb
的私钥，一个名为cjb.pub 的公钥。

把公钥 cjb.pub 上传到远程 cjb 服务器的 ~/.ssh/ 目录下：

    
    
    scp ./cjb.pub user@216.194.70.6:~/.ssh/

上传完后，把公钥 cjb.pub 的内容复制到 authorized_keys 文件里（不存在则新创建一个）：

    
    
    cat cjb.pub >> authorized_keys
    

以 ssh publickey 的形式访问，对当前用户根目录下的 .ssh 文件夹里的目录文件是要有一定的权限要求，之前遇到过 ssh publickey
配置好了，不过用 publickey 登陆验证时则无效。所以，最好设下 .ssh 目录权限为 700，authorized_keys 权限为 600：

    
    
    chmod 700 ~/.ssh/
    chmod 600 authorized_keys
    

当然，用密钥的方式连接服务器是需要服务器上的 ssh 支持的，需要 ssh 的配置文件（默认是在 etc/ssh/sshd_config）里的
PubkeyAuthentication 设置成 yes。如果要改登陆的端口，直接把 Port 改成你想要的端口值就行。修改完后重启下 ssh
，配置就生效：

    
    
    /etc/init.d/ssh restart

还好，CJB 提供的免费 ssh 支持密钥访问。

这样，连接 cjb 的密钥配置完成，本地 ssh 用户配置文件 config 里的 IdentityFile 值写为私钥 cjb 的文件路径
"~/.ssh/cjb" 就行，最后，本地 ssh 用户配置文件 config 的内容如下：

    
    
    Host        cjb
        HostName        216.194.70.6
        Port            22
        User            user
        IdentityFile    ~/.ssh/cjb
    

配置完成后，就可以在终端直接输入 "ssh cjb" 畅通无阻地连上远程的 cjb 服务器上了。

如果有多个 ssh 账号需要配置，在 config 文件里隔行分开写就行，例如：

    
    
    Host        cjb
        HostName        216.194.70.6
        Port            22
        User            user
        IdentityFile    ~/.ssh/cjb
    
    Host        alias
        HostName    hostname
        Port        port
        User        user
    

嘿嘿，如果你也用 ssh 作 [socks5](http://en.wikipedia.org/wiki/SOCKS) 代理翻墙，以后不用这样写了：

    
    
    ssh -qTfnNC -D 12345 user@216.194.70.6 -p 22
    

可以省去 user 后面的，直接写成这样：

    
    
    ssh cjb -qTfnNC -D 12345

上面的 scp 传送也可以简写成这样：

    
    
    scp ./cjb.pub cjb:~/.ssh/
    

执行远程 ssh 命令：

    
    
    ssh cjb "ls ~"

打包一个文件（假设当前目录有个名为 test 的文件夹），接着上传到远程服务器，最后解压文件

    
    
    tar -zcvf - ./test/ | ssh user@216.194.70.6 'cd /user/; tar xvfz -'

