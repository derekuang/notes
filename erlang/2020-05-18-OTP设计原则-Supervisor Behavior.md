---
tags: Erlang OTP supervisor
---

# supervisor

## 1、监控原则

- **监控者（supervisor）**负责开启、终止和监控它的子进程。监控者的思想就是通过必要时的**重启**，来保证子进程**一直活着**。
- 子进程规格说明制定了要启动和监控的子进程。
  - 子进程根据规格列表依次启动。
  - 终止顺序和启动顺序**相反**（启动时，后者可能依赖前者，若先终止前者会导致后者直接挂掉）。



## 2、示例

- 启动`gen_server`子进程的监控树：

  ```erlang
  -module(ch_sup).
  -behaviour(supervisor).
  
  -export([start_link/0]).
  -export([init/1]).
  
  start_link() ->
      supervisor:start_link(ch_sup, []).
  
  init(_Args) ->
      SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
      ChildSpecs = [#{id => ch3,
                      start => {ch3, start_link, []},
                      restart => permanent,
                      shutdown => brutal_kill,
                      type => worker,
                      modules => [ch3]}],
      {ok, {SupFlags, ChildSpecs}}.
  ```



## 3、supervisor flag

- 即示例中的`SupFlags`，定义如下：

  ```erlang
  sup_flags() = #{strategy => strategy(),         % optional
                  intensity => non_neg_integer(), % optional
                  period => pos_integer()}        % optional
      strategy() = one_for_all
                 | one_for_one
                 | rest_for_one
                 | simple_one_for_one
  ```

  - `strategy`：重启策略
  - `intensity`+`period`：最大重启频率



## 4、重启策略

- 重启策略由`init`返回的`map`中的`strategy`指定，默认为`one_for_one`。

- **one_for_one**

  如果子进程终止，只有终止的子进程会重启。

  ![img](https://images2017.cnblogs.com/blog/1046797/201712/1046797-20171225112154444-238345665.png)

- **one_for_all**

  如果子进程终止，其它子进程都会被终止，然后重启所有子进程。

  ![img](https://images2017.cnblogs.com/blog/1046797/201712/1046797-20171225112402131-1112750349.png)

- **rest_for_one**

  如果子进程终止，启动顺序在该子进程后面的子进程都会被终止，随后被重启。

- **simple_one_for_ine**

  简化版的`one_for_one`，所有子进程都是同一个进程动态添加到监控树的实例。



## 5、最大重启频率

- `init`函数返回的`supervisor flag`中的`intensity`和`period`共同限制了子进程给定时间间隔内的重启次数。

  ```erlang
  SupFlags = #{intensity => MaxR, period => MaxT, ...}
  ```

  - `intensity`：重启次数，默认值为1次。能忍受多少次突发重启。
  - `period`：周期时间间隔，默认值为5秒。爆发的容忍度。

- 当监控者的子进程重启频率超过阈值

  - 监控者终止所有子进程
  - 监控者退出，返回的理由是`shutdown`
  - 该监控者的上一级监控者也会重启或者退出

- 这个重启机制的目的是为了防止进程反复因为**同一个原因**终止和重启。

- 在多级监控的应用中，顶层`supervisor`和次级的`supervisor`应平衡好。



## 6、子进程规格说明

- **子进程规格（child specification）**的类型定义：

  ```erlang
  child_spec() = #{id => child_id(),       % mandatory
                   start => mfargs(),      % mandatory
                   restart => restart(),   % optional
                   shutdown => shutdown(), % optional
                   type => worker(),       % optional
                   modules => modules()}   % optional
      child_id() = term()
      mfargs() = {M :: module(), F :: atom(), A :: [term()]}
      modules() = [module()] | dynamic
      restart() = permanent | transient | temporary
      shutdown() = brutal_kill | timeout()
      worker() = worker | supervisor
  ```

  - `id`/`name`

    - 在`supervisor`内部识别不同的`child specification`
    - 必填项

  - `start`

    - 规定了启动子进程的函数，用于`apply(M,F,A)`
    - 必填项

  - `restart`

    - 规定了一个被终止的进程什么时候触发重启

    - `permanent`：总会重启

      `temporary`：不重启

      `transient`：异常退出时重启（**非**`normal`、`shutdown`、`{shutdown, Term}`）

    - 可选项；默认值为`permanent`

  - `shutdown`

    - 规定了进程被终止的方式

    - `brutal_kill`：使用`exit(Child, kill)`**无条件**终止子进程

      `Integer(整数值)`：使用`exit(Child, shutdown)`通知子进程退出，等待返回。如果指定时间内没收到退出信号（超时），改为使用`brutal_kill`的方式

      `infinity`：一般情况下子进程是`supervisor`时使用，让子监控树有足够时间退出。警告：如果子进程是`worker`，应当安全实现子进程，确保子进程会返回

    - 可选项；当子进程为`worker`，默认值为5000；当子进程为`supervisor`，默认值为`infinity`

  - `type`

    - 表明了子进程的性质

    - `worker`

      `supervisor`

    - 可选项；默认值为`worker`

  - `modules`

    - 子进程执行相关的内容

    - 单元素列表`[Module]`：Module是回调模块，子进程为`supervisor`、`gen_server`、`gen_statem`

      `dynamic`：当子进程是`gen_event`

    - 可选项；默认值为`[M]`，`M`来自子进程启动参数`{M, F, A}`



  

  ## 7、启动supervisor

  - 示例中，调用`ch_sup:start_link()`来启动

    ```erlang
    start_link() ->
        supervisor:start_link(ch_sup, []).
    ```

  - `ch_sup:start_link/0`调用`supervisor:start_link/2`，生成并连接一个新进程

    - `ch_sup`：回调模块名
    - `[]`：传递给`init`的参数

- `supervisor:start_link/2,3`是同步调用，所有子进程启动前它不会返回



## 8、动态增加子进程

- 除上述的静态监控树外，可动态添加子进程到监控树：

  ```erlang
  supervisor:start_child(Sup, ChildSpec)
  ```

  - `Sup`：`supervisor`的进程id或注册名
  - `ChildSpec`：子进程规格

- 如`supervisor`终止被重启，所有动态添加的进程都会丢失



## 9、终止子进程

- 调用函数`supervisor:terminate_child/2`根据规格终止子进程

  ```erlang
  supervisor:terminate_child(Sup, Id)
  ```

- 删除一个终止的子进程

  ```erlang
  supervisor:delete_child(Sup, Id)
  ```

  - `Sup`：`supervisor`的进程id或注册名
  - `Id`：子进程规格中的`Id`项

- 删除静态的子进程规格会导致它跟动态子进程一样，在 supervisor 重启时丢失。



## 10、simple_one_for_one

- 重启策略 `simple_one_for_one `是简化的 `one_for_one`，所有的子进程是相同过程的实例，被**动态**地添加到监控树中。

  ```erlang
  -module(simple_sup).
  -behaviour(supervisor).
  
  -export([start_link/0]).
  -export([init/1]).
  
  start_link() ->
      supervisor:start_link(simple_sup, []).
  
  init(_Args) ->
      SupFlags = #{strategy => simple_one_for_one,
                   intensity => 0,
                   period => 1},
      ChildSpecs = [#{id => call,
                      start => {call, start_link, []},
                      shutdown => brutal_kill}],
      {ok, {SupFlags, ChildSpecs}}.
  ```

  - 启动时，`supervisor`没有启动任何子进程，所有子进程都是动态添加：

    ```erlang
    supervisor:start_child(Sup, [id1])
    ```

  - 子进程调用`apply/3`启动：

    ```erlang
    call:start_link(id1)
    ```

  - `simple_one_for_one `监程的子进程通过下面的方式来终止：

    ```erlang
    supervisor:terminate_child(Sup, Pid)
    ```

- 由于`simple_one_for_one`的监控者可能有大量子进程，所以它是**异步**终止它们的。即子进程平行地做清理工作，终止顺序不可预测。



