---
tags: Erlang OTP gen_statem
---

# gen_statem

## 1、事件驱动的状态机

- 现在的自动机理论没有具体描述**状态变迁**是如何触发的，而是假定输出是一个以输入和当前状态为参数的函数，它们是某种类型的值。

- 对一个事件驱动的状态机来说，输入就是一个触发**状态变迁**的事件，输出是**状态迁移**过程中执行的动作。用类似有限状态自动机的数学模型来描述，它是一系列如下形式的关系：

  ```
  State(S) x Event(E) -> Actions(A), State(S')
  ```

  解释：当前状态为`S`，事件`E`发生了；随后我们要执行动作`A`并且状态转移为`S'`（`S`和`S'`可能相同）。

- 由于 `A` 和 `S' `只取决于 `S `和 `E`，这种状态机被称为 Mealy 机。

- 跟大多数 `gen_ `开头的 behavior 一样， `gen_statem `保存了 server 的数据和状态。而且状态数是没有限制的（假设虚拟机内存足够），输入事件类型数也是没有限制的。



## 2、回调模式

- `state_functions`方式：一个状态对应一个事件处理函数，写法如下：

  ```
  StateName(EventType, EventContent, Data) ->
      ... code for actions here ...
      {next_state, NewStateName, NewData}.
  ```

- `handle_event_function`方式：所有的事件都用一个函数来处理，写法如下：

  ```
  handle_event(EventType, EventContent, State, Data) ->
      ... code for actions here ...
      {next_state, NewState, NewData}
  ```

- 回调模式是回调模块的一个属性，在服务器启动的时候设置。回调模式可能会因为代码或者回调模块的改变而改变。

- 确定回调模式需要强制实现一个回调函数`Module:callback_mode()`，函数返回其中一个回调模式。

- `Module:callback_mode()`也有可能返回一个包含回调模式和原子`state_enter`的列表，以激活回调模式的`state enter call`。

- 选择回调模式

  - 这两种回调方式有不同的功能和限制，但是目标都一样：要处理所有可能的事件和状态的组合。

  - 你可以同时只关心一种状态，确保每个状态都处理了所有事件；
    或者只关心一个事件，确保它在所有状态下都被处理。你也可以结合两种策略。

  - `state_functions` 方式中，状态只能用 `atom` 表示，`gen_statem` 引擎（`gen_statem.erl`相关状态机进程）通过状态名来分发处理。它提倡回调模块把一个状态下的所有事件和动作放在代码的同一个地方，以此同时只关注一个状态。

    当你的状态图确定时，这种模式非常好。就像本小节举的例子，状态对应的事件和动作都放在一起，每个状态有自己独一无二的名字。

  - 通过 `handle_event_function` 方式，可以结合两种策略，因为所有的事件和状态都在同一个回调函数中。

    无论是想以状态还是事件为中心，这种方式都能满足。不过没有分发到辅助函数的话，`Module:handle_event/4` 会迅速增长到无法管理。



## 3、状态enter回调

- 无论哪种回调模式，gen_statem都会在进入状态时自动回调函数，可以在状态的转移规则附近写状态入口回调。如下：

  ```
  StateName(enter, _OldState, Data) ->
      ... code for state entry actions here ...
      {keep_state, NewData};
  StateName(EventType, EventContent, Data) ->
      ... code for actions here ...
      {next_state, NewStateName, NewData}.
  ```

- 一旦激活`state enter call`，则要求在所有状态中都处理入口回调。



## 4、动作（Actions）

- 一般的动作会在 `gen_statem `处理事件的回调中执行（返回到 `gen_statem `引擎之前）。

- 还有一些特殊的状态迁移动作，在回调函数返回后指定 gen_statem 引擎去执行。回调函数可以在返回的元组中指定一个动作列表。这些动作影响 gen_statem 引擎本身，可以做下列事情：
  - [延缓](https://www.cnblogs.com/-wyp/p/6892632.html#gen_statem_postpone)（`postpone`）当前事件
  - [挂起](https://www.cnblogs.com/-wyp/p/6892632.html#gen_statem_hibernate)（`hibernate`）状态机
  - [状态超时](https://www.cnblogs.com/-wyp/p/6892632.html#gen_statem_timeout)（`state time-out`）
  - [一般超时](https://www.cnblogs.com/-wyp/p/6892632.html#gen_statem_timeout2)（`generic time-out`）
  - [事件超时](https://www.cnblogs.com/-wyp/p/6892632.html#gen_statem_event_timeou)（`event time-out`）
  - [回复调用者](https://www.cnblogs.com/-wyp/p/6892632.html#gen_statem_event)
  - [生成下一个要处理的事件](https://www.cnblogs.com/-wyp/p/6892632.html#gen_statem_event_self)
- 可以回复很多调用者、生成多个后续事件、设置相对时间或绝对时间的超时等等。



## 5、事件类型

- 事件分成不同的类型。同状态下的不同类型的事件都在同一个回调函数中处理，回调函数以 `EventType `和 `EventContent `作为参数。事件类型如下：

  - `cast`：由`gen_statem:cast `生成
  - `{call, From}`：由 `gen_statem:call `生成，状态迁移动作返回 `{reply, From, Msg}` 或调用 `gen_statem:reply `时，会用到 `From `作为回复地址
  - `info`：发送给 `gen_statem` 进程的常规进程消息
  - `state_timeout`：状态迁移动作 `{state_timeout,Time,EventContent}` 生成
  - `{timeout,Name}`：状态迁移动作 `{ {timeout,Name},Time,EventContent }` 生成
  - `timeout`：状态迁移动作 `{timeout,Time,EventContent}`（或简写为 Time）生成
  - `internal`：状态迁移动作 `{next_event,internal,EventContent}` 生成

  上述所有事件类型都可以用 `{next_event,EventType,EventContent}` 来生成。



## 6、示例

- 密码锁的门可以用一个自动机来表述。初始状态，门是锁住的。当有人按一个按钮，即触发一个事件。结合此前按下的按钮，结果可能是正确、不完整或者错误。如果正确，门锁会开启10秒钟（10,000毫秒）。如果不完整，则等待下一个按钮被按下。如果错了，一切从头再来，等待新一轮按钮。如图
  

![IMAGE MISSING](http://erlang.org/doc/design_principles/code_lock.svg)

- 密码锁状态机可以用 gen_statem 实现，回调模块如下：

  ```erlang
  -module(code_lock).
  -behaviour(gen_statem).
  -define(NAME, code_lock).
  
  -export([start_link/1]).
  -export([button/1]).
  -export([init/1,callback_mode/0,terminate/3,code_change/4]).
  -export([locked/3,open/3]).
  
  start_link(Code) ->
      gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).
  
  button(Digit) ->
      gen_statem:cast(?NAME, {button,Digit}).
  
  init(Code) ->
      do_lock(),
      Data = #{code => Code, remaining => Code},
      {ok, locked, Data}.
  
  callback_mode() ->
      state_functions.
  
  locked(
    cast, {button,Digit},
    #{code := Code, remaining := Remaining} = Data) ->
      case Remaining of
          [Digit] ->
          do_unlock(),
              {next_state, open, Data#{remaining := Code},
               [{state_timeout,10000,lock}]};
          [Digit|Rest] -> % Incomplete
              {next_state, locked, Data#{remaining := Rest}};
          _Wrong ->
              {next_state, locked, Data#{remaining := Code}}
      end.
  
  open(state_timeout, lock,  Data) ->
      do_lock(),
      {next_state, locked, Data};
  open(cast, {button,_}, Data) ->
      {next_state, open, Data}.
  
  do_lock() ->
      io:format("Lock~n", []).
  do_unlock() ->
      io:format("Unlock~n", []).
  
  terminate(_Reason, State, _Data) ->
      State =/= locked andalso do_lock(),
      ok.
  code_change(_Vsn, State, Data, _Extra) ->
      {ok, State, Data}.
  ```

  接下来解释代码。



## 7、启动状态机

- 在密码锁的例子中，可以调用 `code_lock:start_link(Code)` 来启动 `gen_statem`：

  ```erlang
  start_link(Code) ->
      gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).
  ```

- `start_link `函数调用 [gen_statem:start_link/4](http://erlang.org/doc/man/gen_statem.html#start_link-4)，生成并连接了一个新进程（`gen_statem`）。

  - 第一个参数，`{local, ?NAME}` 指定了名字。与`gen_server`同理。
  - 第二个参数，`?MODULE`，这个参数就是回调模块的名字，此例的回调模块就是当前模块。接口函数（`start_link/1` 和 `button/1`）与回调函数（`init/1`, `locked/3`, 和 `open/3`）放在同一个模块中。

  - 第三个参数，`Code`，是一串数字，存储了正确的门锁密码，将被传递给` init/1` 函数。
  - 第四个参数，`[]`，是一个选项list。查看 [gen_statem:start_link/3](http://erlang.org/doc/man/gen_statem.html#start_link-3) 可获悉可用选项。

- 如果名字注册成功，这个新的 `gen_statem `进程会调用 `init `回调` code_lock:init(Code)`。`init `函数应该返回` {ok, State, Data}`。

  -  `State `是初始状态（此例中是锁住状态，假设门一开始是锁住的）。
  - `Data `是  `gen_statem `的内部数据。此例中 `Data `是一个`map`，其中 `code `对应的是正确的密码，`remaining  `对应的是按钮按对后剩余的密码（初始与 `code `一致）。

  ```erlang
  init(Code) ->
      do_lock(),
      Data = #{code => Code, remaining => Code},
      {ok,locked,Data}.
  ```

- `gen_statem:start_link `是同步调用，在 `gen_statem `初始化成功可接收请求之前它不会返回。

- 如果 `gen_statem `是一个监控树的一部分，监督者对其的启动方式痛`gen_server`同理。

- 函数 [Module:callback_mode/0](http://erlang.org/doc/man/gen_statem.html#Module:callback_mode-0) 规定了回调模块的[回调模式](https://www.cnblogs.com/-wyp/p/6892632.html#gen_statem_cb_md)，此例中是 [state_functions](http://erlang.org/doc/man/gen_statem.html#type-callback_mode) 模式，每个状态有自己的处理函数。

  ```erlang
  callback_mode() ->
      state_functions.
  ```

  

## 8、事件处理

- 通知`code_lock`按钮事件的函数是用`gen_statem:cast/2`实现的：

  ```erlang
  button(Digit) ->
      gen_statem:cast(?NAME, {button,Digit}).
  ```

  - `?Name`：`gen_statem`的名字，要与进程名字相同
  - `{button, Digit}`：事件内容

- 这个事件会被**转化成一个消息**，发送给 `gen_statem`。当收到事件时， `gen_statem `调用 `StateName(cast, Event, Data)`，一般会返回一个元组` {next_state, NewStateName, NewData}`。

  -  `StateName `：当前状态名
  - `NewStateName`：下一个状态
  - `NewData `： `gen_statem `的新的内部数据
  - `Actions `：是 `gen_statem `引擎要执行的动作列表

  ```erlang
  locked(
    cast, {button,Digit},
    #{code := Code, remaining := Remaining} = Data) ->
      case Remaining of
          [Digit] -> % Complete
          do_unlock(),
              {next_state, open, Data#{remaining := Code},
               [{state_timeout,10000,lock}]};
          [Digit|Rest] -> % Incomplete
              {next_state, locked, Data#{remaining := Rest}};
          [_|_] -> % Wrong
              {next_state, locked, Data#{remaining := Code}}
      end.
  
  open(state_timeout, lock, Data) ->
      do_lock(),
      {next_state, locked, Data};
  open(cast, {button,_}, Data) ->
      {next_state, open, Data}.
  ```

- 事件的处理流程就是上面的流程图所示。



## 9、状态超时

- 当给出正确的密码，门锁开启，`locked/2` 返回如下元组：

  ```erlang
  {next_state, open, Data#{remaining := Code},
   [{state_timeout,10000,lock}]};
  ```

- 10,000 是以毫秒为单位的超时时长。10秒后，会触发一个超时，然后 `StateName(state_timeout, lock, Data)` 被调用，此后门重新锁住：

  ```erlang
  open(state_timeout, lock,  Data) ->
      do_lock(),
      {next_state, locked, Data};
  ```

- 状态超时按字面理解就是维持某个状态一段时间后及视为超时，触发超时的`Actions`。

  - **改变状态**时，状态超时会自动取消。相当于重启一个时间为 `infinite `的超时来取消状态超时。



## 10、全状态事件

- 有一些事件在任何状态下都有可能发生。可以在一个**公共的函数**处理这些事件，所有的状态函数都调用它来处理通用的事件（代码复用）。

- 假定一个 `code_length/0` 函数返回正确密码的长度（不敏感的信息）。我们把所有与状态无关的事件分发到公共函数` handle_event/3`（除了处理全状态事件，还能处理一些当前状态下无意义的事件）：

  ```erlang
  ...
  -export([button/1,code_length/0]).
  ...
  
  code_length() ->
      gen_statem:call(?NAME, code_length).
  
  ...
  locked(...) -> ... ;
  locked(EventType, EventContent, Data) ->
      handle_event(EventType, EventContent, Data).
  
  ...
  open(...) -> ... ;
  open(EventType, EventContent, Data) ->
      handle_event(EventType, EventContent, Data).
  
  handle_event({call,From}, code_length, #{code := Code} = Data) ->
      {keep_state, Data, [{reply,From,length(Code)}]}.
  ```

- 此例使用 `gen_statem:call/2`，调用者会等待 `server `的回复。

  - `{reply,From,Reply}` ：向调用者`From`回复`Reply`
  - `{keep_state, ...}` ：保持状态不变。



## 11、单个事件处理器

- 如果使用 `handle_event_function `模式，所有的事件都会在 `Module:handle_event/4` 被处理，我们可以在第一层以事件为中心进行分组，然后再判断状态：

  ```erlang
  ...
  -export([handle_event/4]).
  
  ...
  callback_mode() ->
      handle_event_function.
  
  handle_event(cast, {button,Digit}, State, #{code := Code} = Data) ->
      case State of
      locked ->
          case maps:get(remaining, Data) of
          [Digit] -> % Complete
              do_unlock(),
              {next_state, open, Data#{remaining := Code},
                       [{state_timeout,10000,lock}]};
          [Digit|Rest] -> % Incomplete
              {keep_state, Data#{remaining := Rest}};
          [_|_] -> % Wrong
              {keep_state, Data#{remaining := Code}}
          end;
      open ->
              keep_state_and_data
      end;
  handle_event(state_timeout, lock, open, Data) ->
      do_lock(),
      {next_state, locked, Data}.
  
  ...
  ```



## 12、终止

- 在监控树中

  - 如果 `gen_statem `是监控树的一部分，则不需要终止函数。`gen_statem `自动的被它的监控者终止，具体怎么终止通过 [终止策略](http://erlang.org/doc/design_principles/sup_princ.html#shutdown) 来决定。

  - 如果需要在终止前进行一些操作，那么终止策略必须有一个 `time-out` 值，且 `gen_statem `必须在 `init `函数中被设置为捕捉 `exit `信号，调用` process_flag(trap_exit, true)`：

    ```erlang
    init(Args) ->
        process_flag(trap_exit, true),
        do_lock(),
        ...
    ```

  - 当被要求终止时，`gen_statem `会调用回调函数` terminate(shutdown, State, Data)`：

    ```erlang
    terminate(_Reason, State, _Data) ->
        State =/= locked andalso do_lock(),
        ok.
    ```

- 独立的gen_statem

  - 如果 `gen_statem `不是监控树的一部分，可以写一个 `stop `函数（使用` gen_statem:stop`）。

    ```erlang
    ...
    -export([start_link/1,stop/0]).
    
    ...
    stop() ->
        gen_statem:stop(?NAME).
    ```

  - 这会导致 `gen_statem `调用 `terminate/3`，等待进程终止。



## 13、事件超时

- 事件超时功能继承自 `gen_statem `的前辈 `gen_fsm `，事件超时的定时器在有事件达到的时候就会被取消。你可以接收到一个事件或者一个超时，但不会两个都收到。

- 事件超时由状态迁移动作 `{timeout,Time,EventContent} `指定，或者仅仅是 `Time`， 或者仅仅一个 `Timer `而不是动作列表（继承自 `gen_fsm`）。

- 不活跃情况下想做点什么时，可以用此类超时。如果30秒内没人按钮，重置密码列表：

  ```erlang
  ...
  
  locked(
    timeout, _, 
    #{code := Code, remaining := Remaining} = Data) ->
      {next_state, locked, Data#{remaining := Code}};
  locked(
    cast, {button,Digit},
    #{code := Code, remaining := Remaining} = Data) ->
  ...
          [Digit|Rest] -> % Incomplete
              {next_state, locked, Data#{remaining := Rest}, 30000};
  ...
  ```

- 接收到任意按钮事件时，启动一个30秒超时，如果接收到超时事件就重置密码列表。

- 接收到其他事件时，事件超时会被取消，所以要么接收到其他事件要么接受到超时事件。所以不能也不必要重启一个事件超时（不像状态超时）。因为你处理的任何事件都会取消事件超时。



## 14、一般超时

- 状态超时只在状态不变时有效；事件超时只在不被其他事件打断时有效。

- 如果想要在某个状态下开启一个定时器，而在另一个状态下做处理，想要不改变状态就取消一个定时器，或者希望同时存在多个定时器。这些都可以用过 `generic time-outs` 一般超时来实现。它们看起来有点像事件超时，但是它们**有名字**，不同名字的可以同时存在多个，并且不会被自动取消。

- 下面是用**一般超时**实现来替代**状态超时**的例子，定时器名字是 `open_tm `：

  ```erlang
  ...
  locked(
    cast, {button,Digit},
    #{code := Code, remaining := Remaining} = Data) ->
      case Remaining of
          [Digit] ->
          do_unlock(),
              {next_state, open, Data#{remaining := Code},
           [{ {timeout,open_tm},10000,lock }]};
  ...
  
  open({timeout,open_tm}, lock, Data) ->
      do_lock(),
      {next_state,locked,Data};
  open(cast, {button,_}, Data) ->
      {keep_state,Data};
  ...
  ```

- 和状态超时一样，可以通过给特定的名字设置新的定时器或设置为`infinite`来取消定时器。



## 15、Erlang定时器

- 最全面的处理超时的方式就是使用 Erlang 的定时器，详见 `erlang:start_timer3,4`。大部分的超时任务可以通过 `gen_statem `的超时功能来完成，但有时候你可能想获取 `erlang:cancel_timer(Tref)` 的返回值（剩余时间）。

- 下面是用 erlang 定时器替代前文状态超时的实现：

  ```
  ...
  locked(
    cast, {button,Digit},
    #{code := Code, remaining := Remaining} = Data) ->
      case Remaining of
          [Digit] ->
          do_unlock(),
          Tref = erlang:start_timer(10000, self(), lock),
              {next_state, open, Data#{remaining := Code, timer => Tref}};
  ...
  
  open(info, {timeout,Tref,lock}, #{timer := Tref} = Data) ->
      do_lock(),
      {next_state,locked,maps:remove(timer, Data)};
  open(cast, {button,_}, Data) ->
      {keep_state,Data};
  ...
  ```

- 当状态迁移到 `locked `时，我们可以不从 `Data `中清除 `timer `的值，因为每次进入 `open` 状态都是一个新的 `timer `值。不过最好不要在 `Data `中保留过期的值。

- 当其他事件触发，你想清除一个 `timer `时，可以使用 `erlang:cancel_timer(Tref)` 。如果没有延缓，超时消息被 `cancel `后就不会再被收到，所以要确认是否一不小心延缓了这类消息。要注意的是，超时消息可能在你 `cancel `它之前就到达，所以要根据 `erlang:cancel_timer(Tref) `的返回值，把这消息从进程邮箱里读出来。

- 另一种处理方式是，不要 `cancel `掉一个 `timer`，而是在它到达之后忽略它。



## 16、延缓事件

- 如果想在当前状态忽略某个事件，在后续的某个状态中再处理，可以**延缓**这个事件。延缓的事件会在**状态变化后**重新触发，即：`OldState `=/= `NewState `。

- 延缓是通过状态迁移动作 `postpone `来指定的。

- 此例中，我们可以延缓在 `open `状态下的按钮事件（而不是忽略它），这些事件会进入等待队列，等到 `locked `状态时再处理：

  ```erlang
  ...
  open(cast, {button,_}, Data) ->
      {keep_state,Data,[postpone]};
  ...
  ```

- 延缓的事件只会在状态改变时重新触发，因此要考虑怎么保存内部数据。内部数据可以在数据 `Data `或者状态 `State `中保存，比如用两个几乎一样的状态来表示布尔值，或者使用一个复合状态（回调模块的 `handle_event_function`）。如果某个值的变化会改变事件处理，那需要把这个值保存在状态 `State `里。因为 `Data `的变化不会触发延缓的事件。

- 如果你没有用延缓的话，这个不重要。但是如果你决定使用延缓功能，没有用不同的状态做区分，可能会产生很难发现的 bug。

- 选择性receive
  Erlang 的选择性 receive 语句经常被用来写简单的状态机（不用 gen_statem 的普通 erlang 代码）。下面是可能的实现方式之一：

  ```erlang
  -module(code_lock).
  -define(NAME, code_lock_1).
  -export([start_link/1,button/1]).
  
  start_link(Code) ->
      spawn(
        fun () ->
            true = register(?NAME, self()),
            do_lock(),
            locked(Code, Code)
        end).
  
  button(Digit) ->
      ?NAME ! {button,Digit}.
  
  locked(Code, [Digit|Remaining]) ->
      receive
      {button,Digit} when Remaining =:= [] ->
          do_unlock(),
          open(Code);
      {button,Digit} ->
          locked(Code, Remaining);
      {button,_} ->
          locked(Code, Code)
      end.
  
  open(Code) ->
      receive
      after 10000 ->
          do_lock(),
          locked(Code, Code)
      end.
  
  do_lock() ->
      io:format("Locked~n", []).
  do_unlock() ->
      io:format("Open~n", []).
  ```

  此例中选择性 receive 隐含了把 `open `状态接收到的所有事件延缓到 `locked `状态的逻辑。

  **选择性 receive 语句不能用在 gen_statem 或者任何 gen_* 中**，因为 receive 语句已经在 gen_* 引擎中包含了。为了兼容 sys ，**behavior 进程必须对系统消息作出反应**，并把非系统的消息传递给回调模块，因此把 receive 集成在引擎层的 loop 里。

  动作 `postpone`（延缓）是被设计来**模拟选择性 receive** 的。选择性 receive 隐式地延缓所有不被接受的事件，而 `postpone `动作则是显示地延缓一个收到的事件。

  两种机制逻辑复杂度和时间复杂度是一样的，而选择性 receive 语法的常因子更少。



## 17、entry动作

- 在 `callback_mode/0` 函数的返回列表中加入 `state_enter`，会在每次状态改变的时候传入参数 `(enter, OldState, ...)` 调用一次回调函数。你只需像事件一样处理这些请求即可：

  ```erlang
  ...
  init(Code) ->
      process_flag(trap_exit, true),
      Data = #{code => Code},
      {ok, locked, Data}.
  
  callback_mode() ->
      [state_functions,state_enter].
  
  locked(enter, _OldState, Data) ->
      do_lock(),
      {keep_state,Data#{remaining => Code}};
  locked(
    cast, {button,Digit},
    #{code := Code, remaining := Remaining} = Data) ->
      case Remaining of
          [Digit] ->
          {next_state, open, Data};
  ...
  
  open(enter, _OldState, _Data) ->
      do_unlock(),
      {keep_state_and_data, [{state_timeout,10000,lock}]};
  open(state_timeout, lock, Data) ->
      {next_state, locked, Data};
  ...
  ```

  可以返回 `{repeat_state, ...}` 、`{repeat_state_and_data,_}` 或 `repeat_state_and_data `来重复执行 `entry `代码，这些词其他含义跟 `keep_state `家族一样（保持状态、数据不变等等）。



## 18、自生成事件

- 有时候可能需要**在状态机中生成事件**，可以用状态迁移动作`` {next_event,EventType,EventContent}`` 来实现。

- 可以生成所有类型的事件。其中 `internal `类型只能通过 `next_event `来生成，不会由外部产生，你可以确定一个 `internal `事件是来自状态机自身。

- 可以用自生成事件来**预处理输入数据**，例如解码、用换行分隔数据。有强迫症的人可能会说，应该分出另一个状态机来发送预处理好的数据给主状态机。为了降低消耗，这个预处理状态机可以通过一般的状态事件处理来实现。

- 下面的例子为一个输入模型，通过 `put_chars(Chars) `输入，`enter() `来结束输入：

  ```erlang
  ...
  -export(put_chars/1, enter/0).
  ...
  put_chars(Chars) when is_binary(Chars) ->
      gen_statem:call(?NAME, {chars,Chars}).
  
  enter() ->
      gen_statem:call(?NAME, enter).
  
  ...
  
  locked(enter, _OldState, Data) ->
      do_lock(),
      {keep_state,Data#{remaining => Code, buf => []}};
  ...
  
  handle_event({call,From}, {chars,Chars}, #{buf := Buf} = Data) ->
      {keep_state, Data#{buf := [Chars|Buf],
       [{reply,From,ok}]};
  handle_event({call,From}, enter, #{buf := Buf} = Data) ->
      Chars = unicode:characters_to_binary(lists:reverse(Buf)),
      try binary_to_integer(Chars) of
          Digit ->
              {keep_state, Data#{buf := []},
               [{reply,From,ok},
                {next_event,internal,{button,Chars}}]}
      catch
          error:badarg ->
              {keep_state, Data#{buf := []},
               [{reply,From,{error,not_an_integer}}]}
      end;
  ...
  ```

  用` code_lock:start([17]) `启动程序，然后就能通过 `code_lock:put_chars(<<"001">>)`, `code_lock:put_chars(<<"7">>)`, `code_lock:enter()` 这一系列动作开锁了。



## 19、重写例子

- 鉴于上文提到的修改，用了状态`enter`回调，用一个新状态图表述：
  ![IMAGE MISSING](http://erlang.org/doc/design_principles/code_lock_2.svg)

- 回调模式：`state_functions`

  ```erlang
  -module(code_lock).
  -behaviour(gen_statem).
  -define(NAME, code_lock_2).
  
  -export([start_link/1,stop/0]).
  -export([button/1,code_length/0]).
  -export([init/1,callback_mode/0,terminate/3,code_change/4]).
  -export([locked/3,open/3]).
  
  start_link(Code) ->
      gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).
  stop() ->
      gen_statem:stop(?NAME).
  
  button(Digit) ->
      gen_statem:cast(?NAME, {button,Digit}).
  code_length() ->
      gen_statem:call(?NAME, code_length).
  
  init(Code) ->
      process_flag(trap_exit, true),
      Data = #{code => Code},
      {ok, locked, Data}.
  
  callback_mode() ->
      [state_functions,state_enter].
  
  locked(enter, _OldState, #{code := Code} = Data) ->
      do_lock(),
      {keep_state, Data#{remaining => Code}};
  locked(
    timeout, _, 
    #{code := Code, remaining := Remaining} = Data) ->
      {keep_state, Data#{remaining := Code}};
  locked(
    cast, {button,Digit},
    #{code := Code, remaining := Remaining} = Data) ->
      case Remaining of
          [Digit] -> % Complete
              {next_state, open, Data};
          [Digit|Rest] -> % Incomplete
              {keep_state, Data#{remaining := Rest}, 30000};
          [_|_] -> % Wrong
              {keep_state, Data#{remaining := Code}}
      end;
  locked(EventType, EventContent, Data) ->
      handle_event(EventType, EventContent, Data).
  
  open(enter, _OldState, _Data) ->
      do_unlock(),
      {keep_state_and_data, [{state_timeout,10000,lock}]};
  open(state_timeout, lock, Data) ->
      {next_state, locked, Data};
  open(cast, {button,_}, _) ->
      {keep_state_and_data, [postpone]};
  open(EventType, EventContent, Data) ->
      handle_event(EventType, EventContent, Data).
  
  handle_event({call,From}, code_length, #{code := Code}) ->
      {keep_state_and_data, [{reply,From,length(Code)}]}.
  
  do_lock() ->
      io:format("Locked~n", []).
  do_unlock() ->
      io:format("Open~n", []).
  
  terminate(_Reason, State, _Data) ->
      State =/= locked andalso do_lock(),
      ok.
  code_change(_Vsn, State, Data, _Extra) ->
      {ok,State,Data}.
  ```

- 回调模式：`handle_event_function`

  ```erlang
  ...
  -export([handle_event/4]).
  
  ...
  callback_mode() ->
      [handle_event_function,state_enter].
  
  %% State: locked
  handle_event(
    enter, _OldState, locked,
    #{code := Code} = Data) ->
      do_lock(),
      {keep_state, Data#{remaining => Code}};
  handle_event(
    timeout, _, locked,
    #{code := Code, remaining := Remaining} = Data) ->
      {keep_state, Data#{remaining := Code}};
  handle_event(
    cast, {button,Digit}, locked,
    #{code := Code, remaining := Remaining} = Data) ->
      case Remaining of
          [Digit] -> % Complete
              {next_state, open, Data};
          [Digit|Rest] -> % Incomplete
              {keep_state, Data#{remaining := Rest}, 30000};
          [_|_] -> % Wrong
              {keep_state, Data#{remaining := Code}}
      end;
  %%
  %% State: open
  handle_event(enter, _OldState, open, _Data) ->
      do_unlock(),
      {keep_state_and_data, [{state_timeout,10000,lock}]};
  handle_event(state_timeout, lock, open, Data) ->
      {next_state, locked, Data};
  handle_event(cast, {button,_}, open, _) ->
      {keep_state_and_data,[postpone]};
  %%
  %% Any state
  handle_event({call,From}, code_length, _State, #{code := Code}) ->
      {keep_state_and_data, [{reply,From,length(Code)}]}.
  
  ...
  ```

  真正的密码锁中把按钮事件从 locked 状态延迟到 open 状态感觉会很奇怪，它只是用来举例说明事件延缓。



## 20、过滤状态

- 前面实现的服务器中，在因为内部错误或收到一个`exit`信号而终止时，`gen_statem`引擎会调用`sys:get_status/1,2`来获取所有内部状态，包括门锁密码`Code`和剩下需要按的按钮`Rest`。

- 如果不想在错误日志中输出这些敏感内容以及另外一些没有用的数据，可以进行筛选。

- 实现`Mod:format_status/2`来格式化错误日志中得到的内部状态，如：

  ```erlang
  ...
  -export([init/1,terminate/3,code_change/4,format_status/2]).
  ...
  
  format_status(Opt, [_PDict,State,Data]) ->
      StateData =
      {State,
       maps:filter(
         fun (code, _) -> false;
             (remaining, _) -> false;
             (_, _) -> true
         end,
         Data)},
      case Opt of
      terminate ->
          StateData;
      normal ->
          [{data,[{"State",StateData}]}]
      end.
  ```

  `Mod:format_status/2`并非是强制的。

  - 如果不实现，所有的内部状态`State`、`Data`都被获取，包括敏感信息；
  - 反之，可以选择过滤掉敏感信息（`maps:filter/2`），返回其他数据。



## 21、复合状态

- 回调模式是`handle_event_function`时，支持状态使用非原子，比如一个复合状态可能是一个元组。

- 如果想在状态变化时取消状态超时，或和延缓事件配合控制事件处理，就要用到复合状态：
  引入可配置的锁门按钮来完善前面的例子：

  - 存在按钮可以使得`open`状态下立刻锁门；
  - 通过`set_lock_button/1`这个接口来设置锁门按钮。

- 使用`gen_statem:call`实现`button/1`，在`open`状态下延缓按钮事件。在`open`状态调用`button/1`，状态变为`locked`前不会返回，直至到`locked`状态事件才会被处理和回复。

- 有一个进程在`button/1`挂起（因为`receive`阻塞），这时调用`set_lock_button/1`改变锁门按钮时，被挂起的调用会立刻生效，门被锁住。因此，可以把锁门的按钮当作状态的一部分。

- 定义状态为` {StateName,LockButton}`，其中 `StateName `和之前一样，而 `LockButton `则表示当前的锁门按钮：

  ```erlang
  -module(code_lock).
  -behaviour(gen_statem).
  -define(NAME, code_lock_3).
  
  -export([start_link/2,stop/0]).
  -export([button/1,code_length/0,set_lock_button/1]).
  -export([init/1,callback_mode/0,terminate/3,code_change/4,format_status/2]).
  -export([handle_event/4]).
  
  start_link(Code, LockButton) ->
      gen_statem:start_link(
          {local,?NAME}, ?MODULE, {Code,LockButton}, []).
  stop() ->
      gen_statem:stop(?NAME).
  
  button(Digit) ->
      gen_statem:call(?NAME, {button,Digit}).
  code_length() ->
      gen_statem:call(?NAME, code_length).
  set_lock_button(LockButton) ->
      gen_statem:call(?NAME, {set_lock_button,LockButton}).
  
  init({Code,LockButton}) ->
      process_flag(trap_exit, true),
      Data = #{code => Code, remaining => undefined},
      {ok, {locked,LockButton}, Data}.
  
  callback_mode() ->
      [handle_event_function,state_enter].
  
  handle_event(
    {call,From}, {set_lock_button,NewLockButton},
    {StateName,OldLockButton}, Data) ->
      {next_state, {StateName,NewLockButton}, Data,
       [{reply,From,OldLockButton}]};
  handle_event(
    {call,From}, code_length,
    {_StateName,_LockButton}, #{code := Code}) ->
      {keep_state_and_data,
       [{reply,From,length(Code)}]};
  %%
  %% State: locked
  handle_event(
    EventType, EventContent,
    {locked,LockButton}, #{code := Code, remaining := Remaining} = Data) ->
      case {EventType, EventContent} of
      {enter, _OldState} ->
          do_lock(),
          {keep_state, Data#{remaining := Code}};
          {timeout, _} ->
              {keep_state, Data#{remaining := Code}};
      { {call,From}, {button,Digit} } ->
          case Remaining of
          [Digit] -> % Complete
              {next_state, {open,LockButton}, Data,
               [{reply,From,ok}]};
          [Digit|Rest] -> % Incomplete
              {keep_state, Data#{remaining := Rest, 30000},
               [{reply,From,ok}]};
          [_|_] -> % Wrong
              {keep_state, Data#{remaining := Code},
               [{reply,From,ok}]}
          end
      end;
  %%
  %% State: open
  handle_event(
    EventType, EventContent,
    {open,LockButton}, Data) ->
      case {EventType, EventContent} of
      {enter, _OldState} ->
          do_unlock(),
          {keep_state_and_data, [{state_timeout,10000,lock}]};
      {state_timeout, lock} ->
          {next_state, {locked,LockButton}, Data};
      { {call,From}, {button,Digit} } ->
          if
          Digit =:= LockButton ->
              {next_state, {locked,LockButton}, Data,
               [{reply,From,locked}]};
          true ->
              {keep_state_and_data,
               [postpone]}
          end
      end.
  
  do_lock() ->
      io:format("Locked~n", []).
  do_unlock() ->
      io:format("Open~n", []).
  
  terminate(_Reason, State, _Data) ->
      State =/= locked andalso do_lock(),
      ok.
  code_change(_Vsn, State, Data, _Extra) ->
      {ok,State,Data}.
  format_status(Opt, [_PDict,State,Data]) ->
      StateData =
      {State,
       maps:filter(
         fun (code, _) -> false;
             (remaining, _) -> false;
             (_, _) -> true
         end,
         Data)},
      case Opt of
      terminate ->
          StateData;
      normal ->
          [{data,[{"State",StateData}]}]
      end.
  ```



## 22、挂起（非receive阻塞）

- 如果一个节点中有很多个 server，并且他们在生命周期中某些时候会空闲，那么这些 server 的堆内存会造成浪费，通过 `proc_lib:hibernate/3` 来挂起 server 会把它的内存占用降到最低。

- 注意：挂起一个进程的代价很高，不要再每个事件后都挂起进程。

- 此例中我们可以在` {open,_}` 状态挂起，因为正常来说只有在一段时间后它才会收到状态超时，迁移至 `locked `状态：

  ```erlang
  ...
  %% State: open
  handle_event(
    EventType, EventContent,
    {open,LockButton}, Data) ->
      case {EventType, EventContent} of
          {enter, _OldState} ->
              do_unlock(),
              {keep_state_and_data,
               [{state_timeout,10000,lock},hibernate]};
  ...
  ```

  最后一行的动作列表中做了`hibernate`的修改。如果任何事件在`{open, _}`状态到达，就不必重新挂起，接收事件后server会被唤醒。

- 如果要重新挂起，需要在更多的地方插入 `hibernate `来改变。例如，跟状态无关的 `set_lock_button `和 `code_length `操作，在` {open,_}` 状态可以让他 `hibernate`，但是这样会让代码很乱。

- 另一个不常用的方法是使用事件超时，在一段时间的不活跃后触发挂起。

