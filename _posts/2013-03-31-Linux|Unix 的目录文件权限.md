---
layout: page
title: Linux/Unix 的目录文件权限
---
{% include JB/setup %}

在 Linux/Unix 下，如果想知道目录文件的权限分配，可以用 ls 命令查看：

    
    
    ls -l
    

一般情况都会输出类似以下的结果（假设当前目录有以下文件或文件夹）：

    
    
    -rw-r--r--    1 dengjoe  staff    608  1  9 20:02 Emakefile
    -rw-r--r--    1 dengjoe  staff   1473  1  9 20:02 GNUmakefile
    -rw-r--r--    1 dengjoe  staff     51  1  9 20:02 Makefile
    -rw-r--r--    1 dengjoe  staff   1393  1  9 20:02 README.md
    drwxr-xr-x    3 dengjoe  staff    102  1  9 20:02 bin
    drwxr-xr-x   13 dengjoe  staff    442  1  9 20:02 deps
    drwxr-xr-x  561 dengjoe  staff  19074  3 31 10:06 ebin
    drwxr-xr-x   10 dengjoe  staff    340  1  9 20:02 include
    drwxr-xr-x   47 dengjoe  staff   1598  1  9 20:02 modules
    drwxr-xr-x   16 dengjoe  staff    544  1 15 15:35 priv
    drwxr-xr-x   19 dengjoe  staff    646  1  9 20:02 src
    -rwxr-xr-x    1 dengjoe  staff    287  1  9 20:02 start.sh
    

拿 start.sh 来说，它的文件信息是：

    
    
    -rwxr-xr-x    1 dengjoe  staff    287  1  9 20:02 start.sh
    

对应的每一列指的是：

    
    
    -rwxr-xr-x  :   权限 
    1           :   链接数（文件就是1，文件夹的话就是 ls -a 的列出的条数）
    dengjoe     :   该文件的拥有者
    staff       :   该拥有者所在的用户群组
    287         :   文件大小
    1  9 20:02  :   最后修改时间
    start.sh    :   文件名
    

"-rwxr-xr-x" 就是该文件的权限信息，对于这 10 个字符，可以把它再分成 4 列：

    
    
    -       rwx                 r-x                     r-x
    类型    文件拥有者的权限    文件所在群组的权限      其他人的权限
    

第 1 列指明该对象的类型，类型分别有：

  * "-" 表示这是文件
  * "d" 表示这是文件夹（directory）
  * "l" 表示这是链接（link）

第 2 列指的是该文件拥有者的权限，上面的 r 指的是可读（read），w 指的是可写（write），x 指的是可执行（execute），这里的 "rwx"
表示该文件的拥有者具有读取、写入、访问执行该文件的权限。

第 3 列指的是跟该文件拥有者属于同一用户组的用户拥有的权限，上面的 "r-x" 表示拥有读取、访问执行的权限，这里的 "-" 表示无的意思。

第 4 列指的是其他人（拥有者和拥有者所在群组之外的用户）的权限，这里的意思跟第三列一样。

对于读取（r）、写入（w）、执行（x）这 3 类权限可出现的 8 种权限组合情况，在 linux/unix 里用 0 -7 来表示这 8
种权限组合，具体如下：

  * 0：无权限（---）
  * 1：仅执行权限（--x）
  * 2：仅写权限（-w-）
  * 3：执行和写权限（-wx）
  * 4：只读权限（r--）
  * 5：读和执行权限（r-x）
  * 6：读写权限（rw-）
  * 7：所有权限（rwx）

