---
tags: Erlang OTP gen_event
---

# gen_event

## 1、事件处理原则

- **事件管理器（event manager）**是一个可以接收事件的对象。
- **事件（event）**可以是一个错误、一个警告或者是一些日志信息等。

- 在事件管理器中，可以安装不止一个**事件处理器（event handler）**。当事件管理器收到一个事件，所有的事件处理器都会处理这个事件。
- 事件管理器是一个**进程**，而事件处理器是一个**回调模块**。
- 事件管理器本质上是维护一个`{Module, State}`列表，其中`Module`是一个事件处理器（回调模块），`State`是处理器的内部状态。



## 2、示例

- 把错误信息输出到终端的事件处理器（回调模块）：

  ```erlang
  -module(terminal_logger).
  -behaviour(gen_event).
  
  -export([init/1, handle_event/2, terminate/2]).
  
  init(_Args) ->
      {ok, []}.
  
  handle_event(ErrorMsg, State) ->
      io:format("***Error*** ~p~n", [ErrorMsg]),
      {ok, State}.
  
  terminate(_Args, _State) ->
      ok.
  ```

- 把错误信息写入文件的事件处理器（回调模块）：

  ```erlang
  -module(file_logger).
  -behaviour(gen_event).
  
  -export([init/1, handle_event/2, terminate/2]).
  
  init(File) ->
      {ok, Fd} = file:open(File, read),
      {ok, Fd}.
  
  handle_event(ErrorMsg, Fd) ->
      io:format(Fd, "***Error*** ~p~n", [ErrorMsg]),
      {ok, Fd}.
  
  terminate(_Args, Fd) ->
      file:close(Fd).
  ```



## 3、开启一个事件管理器

- 调用函数来开启上文说的事件管理器

  ```erlang
  gen_event:start_link({local, error_man})
  ```

  该函数创建并连接一个新进程（事件管理器event manager）

  - `{local, error_man}`：时间管理在本地注册名为`error_man`

- 如果`gen_event`是一个监控树的一部分，如同之前的行为模式，`supervisor`启动`gen_event`时一定要用`gen_event:start_link`。

- `gen_event:start`会启动一个独立的`gen_event`，即它不会成为监控树的一部分。



## 4、添加一个事件处理器

- 调用函数`gen_event:add_handler/3`来添加事件处理器

  ```erlang
  %% 事件处理器terminal_logger
  gen_event:add_handler(error_man, terminal_logger, []).
  
  %% 事件处理器file_logger
  gen_event:add_handler(error_man, file_logger, [FileName]).
  ```

  `gen_event:add_handler/3`会发送一个消息给事件处理器`error_man`，事件处理器会调用`callback_mod:init([Args])`。正常的话返回`{ok, State}`。

- ```erlang
  %% terminal_logger
  init(_Args) ->
      {ok, []}.
      
  %% file_logger
  init(File) ->
      {ok, Fd} = file:open(File, read),
      {ok, Fd}.
  ```

  此例中，`terminal_logger`不需要用内部状态，而`file_logger`用内部状态保存文件描述符`Fd`。



## 5、事件通知

- 调用函数`gen_event:notify/2`来进行事件通知

  > 3> gen_event:notify(error_man, no_reply).
  > ***Error*** no_reply
  > ok

  - `error_man`：事件管理器
  - `no_reply`：事件

- 该事件以消息形式发给事件管理器。随后事件管理器根据事件处理器的**安装顺序**，依次调用`handle_event(Event, State)`。该函数返回`{ok, State1}`，`State1`是事件处理器的新状态。

  ```erlang
  %% terminal_logger
  handle_event(ErrorMsg, State) ->
      io:format("***Error*** ~p~n", [ErrorMsg]),
      {ok, State}.
      
  %% file_logger
  handle_event(ErrorMsg, Fd) ->
      io:format(Fd, "***Error*** ~p~n", [ErrorMsg]),
      {ok, Fd}.
  ```

  

## 6、删除事件处理器

- 调用函数`gen_event:delete/3`删除事件处理器

  > 4> gen_event:delete_handler(error_man, terminal_logger, []).
  > ok

  该函数发送一条消息给事件管理器`error_man`，请求删除事件处理器`terminal_logger`。此时，管理器会调用`terminal_logger:terminate([], State)`。

- ```erlang
  %% terminal_logger
  terminate(_Args, _State) ->
  	ok.
  	
  %% file_logger
  terminate(_Args, Fd) ->
  	file:close(Fd).
  ```

  `terminal_logger`不需要做清理，`file_logger`则要在关闭`init`开启的文件描述符`Fd`。



## 7、终止

- 和删除处理器一样，当事件管理器被终止，它会调用每个处理器的`terminate/2`。

- **监控树中**

  如果管理器是监控树的一部分，则不需要终止函数。管理器自动的被它的监控者终止，具体怎么终止通过 监控者的终止策略来决定。

- **独立的事件管理器**

  可调用以下函数终止：

  > gen_event:stop(error_man).
  > ok



## 8、处理其他消息

- 如果要处理事件以外的消息，需要实现回调函数`handle_info(Info, StateName, StateData)`。如`exit`消息，当事件管理器与其他进程连接（非监控者），并设置为捕捉`exit`信号，则收到消息时执行：

  ```erlang
  handle_info({'EXIT', Pid, Reason}, State) ->
      ..code to handle exits here..
      {ok, NewState}.
  ```

- 事件和消息

  - 事件本质上也是消息，但是外部函数会调用`gen_event:notify/2`来间接通知各事件处理器，调用回调函数`handle_event/2`。

  - 非事件的消息则可能不会显式调用`gen_event:notify/2`来传递消息，事件管理器收到消息后调用回调函数`handle_info/2`。

