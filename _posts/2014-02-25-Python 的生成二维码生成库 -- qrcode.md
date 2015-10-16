---
layout: page
title: Python 的生成二维码生成库 -- qrcode
---
{% include JB/setup %}

二维码简称 [QR Code](http://zh.wikipedia.org/wiki/QR%E7%A2%BC)（Quick Response
Code），学名为快速响应矩阵码，是二维条码的一种，由日本的 [Denso Wave](http://www.denso-wave.com/) 公司于
1994 年发明。现随着智能手机的普及，已广泛应用于平常生活中，例如商品信息查询、社交好友互动、网络地址访问等等。

## 安装 Python 的二维码库 -- qrcode

由于生成 qrcode 图片需要依赖 Python 的图像库，所以需要先安装 Python 图像库
[PIL](http://www.pythonware.com/products/pil/)(Python Imaging Library)，不然会遇到
"ImportError: No module named Image" 的错误：

    
    
    sudo easy_install pil
    

如果安装 pil 时出现以下错误：

    
    
    _imagingft.c:73:10: fatal error: 'freetype/fterrors.h' file not found
    #include <freetype/fterrors.h>
             ^
    1 error generated.
    error: Setup script exited with error: command 'cc' failed with exit status 1
    

在 [StackOverflow](http://stackoverflow.com/questions/20325473/error-
installing-python-image-library-using-pip-on-mac-os-x-10-9) 上发现是 Mac 下所依赖的
[FreeType](http://www.freetype.org/) 链接变更问题，解决如下：

    
    
    ln -s /usr/local/include/freetype2 /usr/local/include/freetype
    sudo easy_install -U pil
    

安装 [qrcode](https://github.com/lincolnloop/python-qrcode) 库：

    
    
    sudo easy_install qrcode
    

成功安装后，即可以在终端里使用 qr 命令生成二维码了：

    
    
    qr "Just a test" > test.png
    
    
    
    qr --help
    

## 示例代码

    
    
    import qrcode
    
    
    qr = qrcode.QRCode(
        version=2,
        error_correction=qrcode.constants.ERROR_CORRECT_L,
        box_size=10,
        border=1
    )
    qr.add_data("http://dhq.me/")
    qr.make(fit=True)
    img = qr.make_image()
    img.save("dhqme_qrcode.png")
    

参数 version 表示生成二维码的尺寸大小，取值范围是 1 至 40，最小尺寸 1 会生成 21 * 21 的二维码，version 每增加
1，生成的二维码就会添加 4 尺寸，例如 version 是 2，则生成 25 * 25 的二维码。

参数 error_correction 指定二维码的容错系数，分别有以下4个系数：

  * ERROR_CORRECT_L: 7%的字码可被容错
  * ERROR_CORRECT_M: 15%的字码可被容错
  * ERROR_CORRECT_Q: 25%的字码可被容错
  * ERROR_CORRECT_H: 30%的字码可被容错

参数 box_size 表示二维码里每个格子的像素大小。

参数 border 表示边框的格子厚度是多少（默认是4）。

运行上面代码会生成敝站的一个 QR Code：

![dhq.me QR Code](https://lh3.googleusercontent.com/-0VTsOB275kY/Uwx9XiofhdI/A
AAAAAAAAiI/QIHo6vU9GW8/s540-no/dhqme_qrcode.png)

## 生成带有图标的二维码

二维码的容错系数（上面所指的 error_correction）越高，生成的二维码则可允许的残缺率越大，且二维码的数据主要保存在图片的四个角上，所以在二维码
中间放一个小图标，对二维码的识别也是不受多大影响的。

对于插入在二维码上的图标大小，这里指定限制图标的大小尺寸最大是二维码长宽的 1/4，以免残缺太大，影响识别。

最后结合 Python 图像库（PIL）的操作，把图片黏贴（paste）在二维码图片的中间，便可以生成一个带有图标的二维码，具体操作代码如下：

    
    
    import Image
    import qrcode
    
    
    qr = qrcode.QRCode(
        version=2,
        error_correction=qrcode.constants.ERROR_CORRECT_H,
        box_size=10,
        border=1
    )
    qr.add_data("http://dhq.me/")
    qr.make(fit=True)
    
    img = qr.make_image()
    img = img.convert("RGBA")
    
    icon = Image.open("favicon.png")
    
    img_w, img_h = img.size
    factor = 4
    size_w = int(img_w / factor)
    size_h = int(img_h / factor)
    
    icon_w, icon_h = icon.size
    if icon_w > size_w:
        icon_w = size_w
    if icon_h > size_h:
        icon_h = size_h
    icon = icon.resize((icon_w, icon_h), Image.ANTIALIAS)
    
    w = int((img_w - icon_w) / 2)
    h = int((img_h - icon_h) / 2)
    img.paste(icon, (w, h), icon)
    
    img.save("dhqme_qrcode.png")
    

![dhq.me QR Code Logo](https://lh5.googleusercontent.com/-uCHcilNHQxY/Uw2w4Tcp
uuI/AAAAAAAAAi0/qcr34cYq1c0/s540-no/dhqme_qrcode_logo.png)

