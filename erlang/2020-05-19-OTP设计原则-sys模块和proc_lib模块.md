---
tags: Erlang OTP sys proc_lib
---

# sys模块和proc_lib模块

## 0、简介

- `sys`模块可以用来简单地debug用behavior实现的进程
- `sys`模块和`proc_lib`模块一起使用可以实现特殊的进程（不采用标准behavior，但符合OTP设计原则）
- 利用`sys`模块还可以实现自定义的behavior
- `sys`模块和`proc_lib`模块都属于`STDLIB application`



## 1、简易debug

- 用`sys`模块的函数debug用`OTP behaviour`实现的进程。

- 示例采用实现`gen_statem`的`code_lock`[例子](https://derekuang.github.io/2020/05/17/OTP%E8%AE%BE%E8%AE%A1%E5%8E%9F%E5%88%99-gen_statem-Behaviour.html#6%E7%A4%BA%E4%BE%8B)：

  ```bash
  Erlang/OTP 20 [DEVELOPMENT] [erts-9.0] [source-5ace45e] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]
  
  Eshell V9.0  (abort with ^G)
  1>  code_lock:start_link([1,2,3,4]).
  Lock
  {ok,<0.63.0>}
  2> sys:statistics(code_lock, true).
  ok
  3>  sys:trace(code_lock, true).
  ok
  4>  code_lock:button(1).
  *DBG* code_lock receive cast {button,1} in state locked
  ok
  *DBG* code_lock consume cast {button,1} in state locked
  5>  code_lock:button(2).
  *DBG* code_lock receive cast {button,2} in state locked
  ok
  *DBG* code_lock consume cast {button,2} in state locked
  6>  code_lock:button(3).
  *DBG* code_lock receive cast {button,3} in state locked
  ok
  *DBG* code_lock consume cast {button,3} in state locked
  7>  code_lock:button(4).
  *DBG* code_lock receive cast {button,4} in state locked
  ok
  Unlock
  *DBG* code_lock consume cast {button,4} in state locked
  *DBG* code_lock receive state_timeout lock in state open
  Lock
  *DBG* code_lock consume state_timeout lock in state open
  8> sys:statistics(code_lock, get).
  {ok,[{start_time,{ {2017,4,21},{16,8,7} }},
       {current_time,{ {2017,4,21},{16,9,42} }},
       {reductions,2973},
       {messages_in,5},
       {messages_out,0}]}
  9> sys:statistics(code_lock, false).
  ok
  10> sys:trace(code_lock, false).
  ok
  11> sys:get_status(code_lock).
  {status,<0.63.0>,
          {module,gen_statem},
          [[{'$initial_call',{code_lock,init,1}},
            {'$ancestors',[<0.61.0>]}],
           running,<0.61.0>,[],
           [{header,"Status for state machine code_lock"},
            {data,[{"Status",running},
                   {"Parent",<0.61.0>},
                   {"Logged Events",[]},
                   {"Postponed",[]}]},
            {data,[{"State",
                    {locked,#{code => [1,2,3,4],remaining => [1,2,3,4]}}}]}]]}
  ```

  - `sys:statistics/2`：启用或禁用对进程的信息采集；如果`Flag`是`get`，则返回统计采集。
  - `sys:trace/2`：在标准输出流打印所有系统事件。



## 2、特殊的进程

- 不采用标准`behavior`，但符合OTP设计原则，需要满足：

  - 提供启动方式使得可以纳入监控树
  - 支持`sys`模块的debug工具
  - 关心系统消息 （如追踪输出的请求、挂起或回复进程的请求（监控者向子进程的请求）。使用标准`behaviour`可以自动处理这些消息）

- 示例（从实现了[`gen_server`的`ch3`](https://derekuang.github.io/2020/05/16/OTP%E8%AE%BE%E8%AE%A1%E5%8E%9F%E5%88%99-gen_server-Behaviour.html#2%E7%A4%BA%E4%BE%8B)修改，使用`sys`和`proc_lib`实现可纳入监控树中）：

  ```erlang
  -module(ch4).
  -export([start_link/0]).
  -export([alloc/0, free/1]).
  -export([init/1]).
  -export([system_continue/3, system_terminate/4,
           write_debug/3,
           system_get_state/1, system_replace_state/2]).
  
  start_link() ->
      proc_lib:start_link(ch4, init, [self()]).
  
  alloc() ->
      ch4 ! {self(), alloc},
      receive
          {ch4, Res} ->
              Res
      end.
  
  free(Ch) ->
      ch4 ! {free, Ch},
      ok.
  
  init(Parent) ->
      register(ch4, self()),
      Chs = channels(),
      Deb = sys:debug_options([]),
      proc_lib:init_ack(Parent, {ok, self()}),
      loop(Chs, Parent, Deb).
  
  loop(Chs, Parent, Deb) ->
      receive
          {From, alloc} ->
              Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,
                                      ch4, {in, alloc, From}),
              {Ch, Chs2} = alloc(Chs),
              From ! {ch4, Ch},
              Deb3 = sys:handle_debug(Deb2, fun ch4:write_debug/3,
                                      ch4, {out, {ch4, Ch}, From}),
              loop(Chs2, Parent, Deb3);
          {free, Ch} ->
              Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,
                                      ch4, {in, {free, Ch}}),
              Chs2 = free(Ch, Chs),
              loop(Chs2, Parent, Deb2);
  
          {system, From, Request} ->
              sys:handle_system_msg(Request, From, Parent,
                                    ch4, Deb, Chs)
      end.
  
  system_continue(Parent, Deb, Chs) ->
      loop(Chs, Parent, Deb).
  
  system_terminate(Reason, _Parent, _Deb, _Chs) ->
      exit(Reason).
  
  system_get_state(Chs) ->
      {ok, Chs}.
  
  system_replace_state(StateFun, Chs) ->
      NChs = StateFun(Chs),
      {ok, NChs, NChs}.
  
  write_debug(Dev, Event, Name) ->
      io:format(Dev, "~p event = ~p~n", [Name, Event]).
  ```

  对`ch4`用`sys`模块进行简易debug：

  ```bash
  % erl
  Erlang (BEAM) emulator version 5.2.3.6 [hipe] [threads:0]
  
  Eshell V5.2.3.6  (abort with ^G)
  1> ch4:start_link().
  {ok,<0.30.0>}
  2> sys:statistics(ch4, true).
  ok
  3> sys:trace(ch4, true).
  ok
  4> ch4:alloc().
  ch4 event = {in,alloc,<0.25.0>}
  ch4 event = {out,{ch4,ch1},<0.25.0>}
  ch1
  5> ch4:free(ch1).
  ch4 event = {in,{free,ch1}}
  ok
  6> sys:statistics(ch4, get).
  {ok,[{start_time,{ {2003,6,13},{9,47,5} }},
       {current_time,{ {2003,6,13},{9,47,56} }},
       {reductions,109},
       {messages_in,2},
       {messages_out,1}]}
  7> sys:statistics(ch4, false).
  ok
  8> sys:trace(ch4, false).
  ok
  9> sys:get_status(ch4).
  {status,<0.30.0>,
          {module,ch4},
          [[{'$ancestors',[<0.25.0>]},{'$initial_call',{ch4,init,[<0.25.0>]}}],
           running,<0.25.0>,[],
           [ch1,ch2,ch3]]}
  ```

  

## 3、**启动进程（启动方式可纳入监控树）**

- `proc_lib`模块提供的一个功能就是启动进程。

  - 异步启动：`spawn_link/3,4`
  - 同步启动：`start_link/3,4,5`
  - 用这些函数启动进程可以被监控树监控，因为它们会存储一些监控树需要的信息（如高层级进程`ancestor`和初始化回调`initial call`）
  - 如果进程以`normal`或`shutdown`以外的理由终止，会生成一个`crash`报告

- 此例中，进程通过`start_link/3`同步启动：

  ```erlang
  start_link() ->
      proc_lib:start_link(ch4, init, [self()]).
  ```

  新进程执行`ch4:init(Pid)`启动，`Pid`即为父进程（在监控树中就是监控者的`Pid`）。

- 新进程在初始化时要通知父进程它的启动：

  ```erlang
  init(Parent) ->
      ...
      proc_lib:init_ack(Parent, {ok, self()}),
      loop(...).
  ```



## 4、**Debugging（支持sys模块的debug工具）**

- 要支持`sys`的debug工具，需要有**debug结构**——Deb，通过`sys:debug_options/1`初始生成

  ```erlang
  init(Parent) ->
      ...
      Deb = sys:debug_options([]),	%% 初始时没有debug被启用
      ...
      loop(Chs, Parent, Deb).
  ```

- debug结构的作用是记录或追踪**系统事件**（即每发生一个系统事件，都会出来记录、追踪）

  ```erlang
  sys:handle_debug(Deb, Func, Info, Event) => Deb1
  ```

  - `Deb`：debug结构
  - `Func`：输出格式化追踪的函数，对于每个系统事件，都会调用`Func(Dev, Event, Info)`；其中`Dev`是要输出到的I/O设备。
  - `Info`：任意类型，传递更多信息给`Func`
  - `Event`：系统事件，可由用户定义。一般至少输入和输出消息会认为是系统事件，分别用`{in, Msg, [,From]}`和`{out, Msg, To}`表示
  - `Deb1`：返回的一个更新的debug结构

- 此例中，`handle_debug`会在每次输入（`receive Msg`）和输出信息（`From ! Msg`）时被调用。格式化函数`Func`是`ch4:write_debug/3`，内部调用`io:format/3`打印消息：

  ```erlang
  loop(Chs, Parent, Deb) ->
      receive
          {From, alloc} ->
              Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,
                                      ch4, {in, alloc, From}),
              {Ch, Chs2} = alloc(Chs),
              From ! {ch4, Ch},
              Deb3 = sys:handle_debug(Deb2, fun ch4:write_debug/3,
                                      ch4, {out, {ch4, Ch}, From}),
              loop(Chs2, Parent, Deb3);
          {free, Ch} ->
              Deb2 = sys:handle_debug(Deb, fun ch4:write_debug/3,
                                      ch4, {in, {free, Ch}}),
              Chs2 = free(Ch, Chs),
              loop(Chs2, Parent, Deb2);
          ...
      end.
  
  write_debug(Dev, Event, Name) ->
      io:format(Dev, "~p event = ~p~n", [Name, Event]).
  ```



## 5、**处理系统消息（实现behavior的进程可自动处理）**

- 系统消息的形式如下：

  `{system, From, Request}`

- 进程收到系统消息后，直接调用`sys`的函数处理：

- `sys:handle_system_msg(Request, From, Parent, Module, Deb, State)`

  - `Request`和`From`：从系统消息处传递
  - `Parent`：父进程`pid`
  - `Module`：模块名
  - `Deb`：debug结构
  - `State`：描述内部状态的项，会传递给后续的函数

  处理完系统消息后：

  - 调用`Module:system_continue(Parent, Deb, State)`，如果进程继续执行
  - 调用`Module:system_terminate(Reason, Parent, Deb, State)`，如果进程终止；监控树的进程应该以父进程相同的理由退出

  如果进程要返回它的状态，`handle_system_msg`会调用：

  > ```erlang
  > Module:system_get_state(State)
  > ```

  如果进程要调用函数`StateFun`替换状态，`handle_system_msg`会调用：

  > ```erlang
  > Module:system_replace_state(StateFun, State)
  > ```

- 对应代码

  ```erlang
  loop(Chs, Parent, Deb) ->
      receive
          ...
  
          {system, From, Request} ->
              sys:handle_system_msg(Request, From, Parent,
                                    ch4, Deb, Chs)
      end.
  
  system_continue(Parent, Deb, Chs) ->
      loop(Chs, Parent, Deb).
  
  system_terminate(Reason, Parent, Deb, Chs) ->
      exit(Reason).
  
  system_get_state(Chs) ->
      {ok, Chs, Chs}.
  
  system_replace_state(StateFun, Chs) ->
      NChs = StateFun(Chs),
      {ok, NChs, NChs}.
  ```

- 如果该进程可以捕捉`exit`信号，且父进程终止，则它的预期行为是以同理由终止：

  ```erlang
  init(...) ->
      ...,
      process_flag(trap_exit, true),
      ...,
      loop(...).
  
  loop(...) ->
      receive
          ...
  
          {'EXIT', Parent, Reason} ->
              ..maybe some cleaning up here..
              exit(Reason);
          ...
      end.
  ```



## 6、自定义behavior

- 实现自定义的`behavior`，代码和特殊进程差不多，不过要**调用回调模块的函数**处理特殊任务。

- 如果需要保证有实现必要的回调函数，可在`behaviour`模块增加`-callback`属性描述预期的回调（`-optional_callbacks`说明该回调可选）：

  > -callback Name(Arg1, Arg2, ..., ArgN) -> Res.
  >
  > -optional_callbacks([Name1/Arity1, ..., NameK/ArityK]).

- 示例：

  ```erlang
  %% User-defined behaviour module
  -module(simple_server).
  -export([start_link/2, init/3, ...]).
  
  -callback init(State :: term()) -> 'ok'.
  -callback handle_req(Req :: term(), State :: term()) -> {'ok', Reply :: term()}.
  -callback terminate() -> 'ok'.
  -callback format_state(State :: term()) -> term().
  
  -optional_callbacks([format_state/1]).
  
  %% Alternatively you may define:
  %%
  %% -export([behaviour_info/1]).
  %% behaviour_info(callbacks) ->
  %%     [{init,1},
  %%      {handle_req,2},
  %%      {terminate,0}].
  
  start_link(Name, Module) ->
      proc_lib:start_link(?MODULE, init, [self(), Name, Module]).
  
  init(Parent, Name, Module) ->
      register(Name, self()),
      ...,
      Dbg = sys:debug_options([]),
      proc_lib:init_ack(Parent, {ok, self()}),
      loop(Parent, Module, Deb, ...).
  
  ...
  ```

- 回调模块：

  ```erlang
  -module(db).
  -behaviour(simple_server).
  
  -export([init/1, handle_req/2, terminate/0]).
  
  ...
  ```

- `behavior`模块中`-callback`属性指定的协议，在回调模块中可添加`-spec`来优化；
  `-callback`指定的协议一般都比较宽泛，所以` -spec`会非常有用。有协议的回调模块：

  ```erlang
  -module(db).
  -behaviour(simple_server).
  
  -export([init/1, handle_req/2, terminate/0]).
  
  -record(state, {field1 :: [atom()], field2 :: integer()}).
  
  -type state()   :: #state{}.
  -type request() :: {'store', term(), term()};
                     {'lookup', term()}.
  
  ...
  
  -spec handle_req(request(), state()) -> {'ok', term()}.
  
  ...
  ```

  每个 `-spec `协议都是对应的 `-callback` 协议的子类型。

  

  

  

