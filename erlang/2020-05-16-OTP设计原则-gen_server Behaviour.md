---
tags: Erlang OTP gen_server
---

# gen_server

## 1、Client-Server原则

- 客户端 - 服务器模型的特点是具有中央服务器和任意数量的客户端。
- 客户端 - 服务器模型用于资源管理操作，其中几个不同的客户端想要共享公共资源。服务器负责管理这个资源。

## 2、示例

- 前面总览已经有一个简单的服务器的例子。使用`gen_server`重写，结果如下：

  ```
  -module(ch3).
  -behaviour(gen_server).
  
  -export([start_link/0]).
  -export([alloc/0, free/1]).
  -export([init/1, handle_call/3, handle_cast/2]).
  
  start_link() ->
      gen_server:start_link({local, ch3}, ch3, [], []).
  
  alloc() ->
      gen_server:call(ch3, alloc).
  
  free(Ch) ->
      gen_server:cast(ch3, {free, Ch}).
  
  init(_Args) ->
      {ok, channels()}.
  
  handle_call(alloc, _From, Chs) ->
      {Ch, Chs2} = alloc(Chs),
      {reply, Ch, Chs2}.
  
  handle_cast({free, Ch}, Chs) ->
      Chs2 = free(Ch, Chs),
      {noreply, Chs2}.
  ```



## 3、启动gen_server

- 上一节的示例中，`gen_server`通过调用`ch3:start_link()`启动：

  ```
  start_link() ->
      gen_server:start_link({local, ch3}, ch3, [], []).
  ```

- `start_link/0`调用了函数`gen_server:start_link/0`，这个函数**产生并连接一个新进程**（一个`gen_server`）。

  参数：

  - `{local, ch3}`：指定进程名，`gen_server`在本地注册为`ch3`。

    如果名字被省略，`gen_server `不会被注册，此时一定要用它的 **pid**。

    名字还可以用 `{global, Name}`，这样的话 `gen_server `会调用 `global:register_name/2` 来注册。

  - `ch3`：回调模块的名字，即回调函数所在的模块名。

    接口函数 (`start_link`, `alloc `和 `free`) 和回调函数 (`init`, `handle_call `和 `handle_cast`) 放在同一个模块中。这是一个好的编程惯例，把与一个进程相关的代码放在同一个模块中。

  - []：传递给回调函数 init 的参数。此例中 init 不需要输入，所以忽视了这个参数。

  - []：一个选项list。

- 如果名字注册成功，这个新的 `gen_server `进程会调用回调函数 `ch3:init([]) `。`init `函数应该返回 `{ok, State}`，其中 `State `是 `gen_server `的内部状态，在此例中，`State`指的是 channel 集合。

  ```
  init(_Args) ->
      {ok, channels()}.
  ```

- `gen_server:start_link `是**同步调用**，在 `gen_server `初始化成功可接收请求之前它不会返回。

- 如果 `gen_server `是一个监控树的一部分，`supervisor `启动 `gen_server  `时一定要使用 `gen_server:start_link`。
  还有一个函数是 `gen_server:start `，这个函数会启动一个**独立**的  `gen_server`，也就是说它不会成为监控树的一部分。



## 4、同步消息请求-Call

- 同步的请求`alloc/0`是用`gen_server:call/2`实现的：

  ```
  alloc() ->
      gen_server:call(ch3, alloc).
  ```

- ch3 是 gen_server 的名字，要与进程名字相符合才能使用。alloc 是实际的请求。

- **这个请求会被转化成一个消息**，发送给 gen_server。收到消息后，gen_server 调用 handle_call(Request, From, State) 来处理消息，正常会返回 {reply, Reply, State1}。Reply 是会发回给客户端的回复内容，State1 是 gen_server 新的内部状态。

  ```
  handle_call(alloc, _From, Chs) ->
      {Ch, Chs2} = alloc(Chs),
      {reply, Ch, Chs2}.
  ```

- 此例中，回复内容就是分配给它的 channel `Ch`，而新的内部状态是剩余的 channel 集合 `Chs2`。

- 就这样，`ch3:alloc/0` 返回了分配给它的 channel `Ch`，`gen_server `则保存剩余的 channel 集合，继续等待新的请求。



## 5、异步消息请求

- 异步的请求 `free(Ch)` 是用 `gen_server:cast/2` 来实现的：

  ```
  free(Ch) ->
      gen_server:cast(ch3, {free, Ch}).
  ```

- `ch3`是 `gen_server`的名字，`{free, Ch}` 是实际的请求。

- **这个请求会被转化成一个消息**，发送给 `gen_server`。发送后**直接返回** `ok`。

- 收到消息后，`gen_server `调用 `handle_cast(Request, State)` 来处理消息，正常会返回 `{noreply,State1}`。`State1 `是 `gen_server `新的内部状态。

  ```
  handle_cast({free, Ch}, Chs) ->
      Chs2 = free(Ch, Chs),
      {noreply, Chs2}.
  ```

- 此例中，新的内部状态是新的剩余的 channel集合 `Chs2`。然后 `gen_server `继续等待新的请求。



## 6、终止

- **在监控树中**

  - 如果 `gen_server `是监控树的一部分，则**不需要终止函数**。`gen_server `会自动被它的监控者终止，具体怎么终止通过 [终止策略](http://erlang.org/doc/design_principles/sup_princ.html#shutdown) 来决定。

  - 如果要在终止前进行一些操作，终止策略必须有一个 `time-out` 值，且 `gen_server `必须在 `init` 函数中被设置为**捕捉 `exit `信号**。当被要求终止时，`gen_server `会调用回调函数 `terminate(shutdown, State)`：

    ```
    init(Args) ->
        ...,
        process_flag(trap_exit, true),
        ...,
        {ok, State}.
    
    ...
    
    terminate(shutdown, State) ->
        ..code for cleaning up here..
        ok.
    ```

- **独立的 gen_server**

  - 如果 `gen_server `不是监控树的一部分，可以写一个 `stop `函数，例如：

    ```
    ...
    export([stop/0]).
    ...
    
    stop() ->
        gen_server:cast(ch3, stop).
    ...
    
    handle_cast(stop, State) ->
        {stop, normal, State};
    handle_cast({free, Ch}, State) ->
        ....
    
    ...
    
    terminate(normal, State) ->
        ok.
    ```

    处理 `stop `消息的回调函数返回 `{stop, normal, State1}`，`normal `意味着这是一次**自然死亡**，而 `State1 `是一个新的 `gen_server `内部状态。这会导致 `gen_server `**调用 `terminate(normal, State1)`** 然后优雅地……挂掉。



## 7、处理其他消息

- 如果 `gen_server `会在除了请求之外接收其他消息，需要实现回调函数` handle_info(Info, State)` 来进行处理。其他消息可能是 `exit `消息，如果 `gen_server `与其他进程连接起来（不是 `supervisor`），并且被设置为捕捉 `exit `信号。

  ```
  handle_info({'EXIT', Pid, Reason}, State) ->
      ..code to handle exits here..
      {noreply, State1}.
  ```

- 一定要实现 `code_change `函数。（在代码热更新时会用到）

  ```
  code_change(OldVsn, State, Extra) ->
      ..code to convert state (and more) during code change
      {ok, NewState}.
  ```

  

