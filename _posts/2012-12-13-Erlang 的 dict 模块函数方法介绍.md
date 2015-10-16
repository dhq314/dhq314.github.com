---
layout: page
title: Erlang 的 dict 模块函数方法介绍
---
{% include JB/setup %}

Erlang 的 [dict](http://www.erlang.org/doc/man/dict.html) 模块比较完整的实现了一个键值（Key - 
Value）字典。通过这个模块，你可以插入，附加（append），删除，更新字典里的键值对，它也提供了获取字典大小和检查键是否存在等调用接口，而且还包含一些
对字典键值进行操作的函数方法，例如：递归（fold）、过滤（filter）、遍历映射（map）字典等特殊操作函数。

**new() -> dictionary()**

初始构造一个新的字典（其实是返回一个内部定义的dict记录record）

    
    
    dict:new().
    %% 下面是返回的dict记录的初始值，具体请看\lib\stdlib-1.18.2\src\dict.erl
    -record(dict, {
    	size=0		      	:: non_neg_integer(),   	% Number of elements
    	n=?seg_size	      	:: non_neg_integer(),   	% Number of active slots
    	maxn=?seg_size	   	:: non_neg_integer(),		% Maximum slots
    	bso=?seg_size div 2	:: non_neg_integer(),   	% Buddy slot offset
    	exp_size=?exp_size 	:: non_neg_integer(),   	% Size to expand at
    	con_size=?con_size 	:: non_neg_integer(),   	% Size to contract at
    	empty		      	:: tuple(),					% Empty segment
    	segs		      	:: tuple()	      			% Segments
    }).
    

**store(Key, Value, Dict1) -> Dict2**

以键值（Key - Value）对的形式存储在字典里。如果字典里已经存在 Key 的键，则把跟 Key 相关的值替换为 Value

    
    
    D = dict:new(),
    D1 = dict:store(k1, v1, D).
    %% D2 = dict:store(k2, v2, D1).
    %% D3 = dict:store(k2, v3, D2).
    

**from_list(List) -> Dict**

把一个 Key-Value 形式的列表转换为一个字典

    
    
    dict:from_list([{k1, v1}, {k2, v2}, {k3, v3}]).
    %% 相当于
    D = dict:new(),
    D1 = dict:store(k1, v1, D),
    D2 = dict:store(k2, v2, D1),
    D3 = dict:store(k3, v3, D2).
    

**size(Dict) -> int()**

返回字典里键值对的个数

    
    
    dict:size(dict:from_list([{k1, v1}, {k2, v2}, {k3, v3}])).
    

**to_list(Dict) -> List**

把字典转换成一个列表形式

    
    
    D = dict:from_list([{k1, v1}, {k2, v2}, {k3, v3}]).
    dict:to_list(D).
    

**append(Key, Value, Dict1) -> Dict2**

给当前列表跟 Key 相关联的值附加上一个新值 Value（如果跟 Key 相关联的值不是一个列表值，将会抛出一个异常错误）

    
    
    D = dict:new().
    D1 = dict:append(a, b, D).
    dict:to_list(D1).
    D2 = dict:append(a, c, D1).
    dict:to_list(D2).
    

**append_list(Key, ValList, Dict1) -> Dict2**

跟 dict:append/3 一样，都是给当前列表跟 Key 相关联的值附加上一个新值，只不过新加的值是一个列表值 ValList（如果跟 Key
相关联的值不是一个列表值，将会抛出一个异常错误，例如下面这种情况）

    
    
    D = dict:new(),
    D1 = dict:store(k, [v1], D),
    D2 = dict:append_list(k, [v2, v3], D1),
    dict:to_list(D2).
    

**erase(Key, Dict1) -> Dict2**

删除字典里跟键 Key 相关联的所有项

    
    
    D = dict:from_list([{k1, v1}, {k2, v2}, {k3, v3}]).
    dict:to_list(D).
    dict:to_list(dict:erase(k1, D)).
    

**is_key(Key, Dict) -> bool()**

判读键 Key 是否在字典 Dict 里存在

    
    
    D = dict:from_list([{k1, v1}, {k2, v2}, {k3, v3}]),
    dict:is_key(k1, D).
    

**fetch(Key, Dict) -> Value**

获取在字典 Dict 里跟键 Key 相关联的值（如果键 Key 不存在字典 Dict 里，则会抛出一个异常错误

    
    
    D = dict:from_list([{k1, v1}, {k2, v2}, {k3, v3}]).
    dict:fetch(k1, D).
    

**fetch_keys(Dict) -> Keys**

以列表的形式返回字典里所有的键

    
    
    D = dict:from_list([{k1, v1}, {k2, v2}, {k3, v3}]).
    dict:fetch_keys(D).
    

**filter(Pred, Dict1) -> Dict2**

字典 Dict1 里的每一个键值以参数的方式被断言函数 Predicate 调用，如果该键值在断言函数 Predicate 执行中返回的是
true，那么则留下，否则被丢弃，最终返回一个符合断言条件的字段 Dict2

    
    
    D = dict:from_list([{k1, 1}, {k2, 2}, {k3, 3}, {k4, 4}]),
    Predicate = fun(_K, V) -> V rem 2 == 0 end,
    D1 = dict:filter(Predicate, D),
    dict:to_list(D1).
    

**find(Key, Dict) -> {ok, Value} | error**

跟 dict:fetch/2 一样，都是查找返回一个在字典 Dict 里跟键 Key
相关联的值，不过返回的格式不一样，而且字典里没有相关联的键值存在不会抛异常错误，只返回一个原子 error

    
    
    D = dict:from_list([{k1, v1}, {k2, v2}, {k3, v3}]),
    dict:find(k1, D).
    

**fold(Fun, Acc0, Dict) -> Acc1**

字典里的每一对键值跟一个临时累积参数（Acc0）一齐被函数（Fun）调用，并返回一个新的累积器（accumulator）以传给下一次函数调用，直到字典里所有
的键值对都被函数（Fun）遍历调用完，最后返回一个累积结果值

    
    
    %% 这里是求字典里所有值的平方和
    D = dict:from_list([{k1, 1}, {k2, 2}, {k3, 3}]),
    dict:fold(fun(_Key, Val, Acc) -> Val * Val + Acc end, 0, D).
    

**map(Fun, Dict1) -> Dict2**

对字典里的每一对键值遍历调用函数(Fun)，最终返回一个新的字典

    
    
    D = dict:from_list([{k1, 1}, {k2, 2}, {k3, 3}]),
    dict:map(fun(_Key, Val) -> Val * Val end, D).
    

**merge(Fun, Dict1, Dict2) -> Dict3**

把 2 个字典合并成为一个新的字典，原来字典的键值都会保留下来，如果存在相同的键，则调用合并函数（Fun）处理并返回一个新值

    
    
    L1 = [{k1, 1}, {k2, 2}, {k3, 3}],
    L2 = [{k1, 1}, {k2, 2}, {k3, 3}, {k4, 4}],
    %% 这里如果有键相同时，则把值相加
    MergeFun = fun(_Key, V1, V2) -> V1 + V2 end,
    D1 = dict:from_list(L1),
    D2 = dict:from_list(L2),
    dict:to_list(dict:merge(MergeFun, D1, D2)).
    

**update(Key, Fun, Dict1) -> Dict2**

通过调用更新函数（Fun）来更新指定键 Key 在字典里的值，如果指定的键在字典里不存在的话，则报错

    
    
    D = dict:from_list([{k1, 1}, {k2, 2}]),
    dict:to_list(D),
    D1 = dict:update(k1, fun(V)-> V * 2 end, D),
    dict:to_list(D1).
    

**update(Key, Fun, Initial, Dict1) -> Dict2**

跟 dict:update/3 一样，通过调用更新函数（Fun）来更新指定键 Key
在字典里的值，不同的是，如果指定的键在字典里不存在的话，不会报异常错误，而是用给出的指定初始值（Initial）替换

    
    
    D = dict:from_list([{k1, 1}, {k2, 2}]),
    dict:to_list(D),
    D1 = dict:update(k3, fun(V)-> V * 2 end, 3, D),
    dict:to_list(D1).
    

**update_counter(Key, Increment, Dict1) -> Dict2**

如果指定键 Key 在字典里存在，则指定键在字典里的值跟参数 Increment 进行相加操作，否则，则在字典里插入（store）一个 {Key,
Increment} 的键值对

    
    
    D = dict:from_list([{k1, 1}]),
    D1 = dict:update_counter(k1, 1, D),
    dict:to_list(D1).
    %% 字典不存在键值则新加
    D1 = dict:update_counter(k2, 1, D),
    dict:to_list(D1).
    

