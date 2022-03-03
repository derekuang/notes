---
tags: Erlang OTP application
---

# Included Applications

## 1、引言

- 应用可以内含其他应用。被包含的应用（**included application**）有自己的应用目录和应用资源文件（`.app`文件），且作为上级应用的监控树的一部分启动。

- 应用不能被多个应用包含

- 没有被任何应用包含的应用成为原初应用（`primary application`）

  ![img](https://images2017.cnblogs.com/blog/1046797/201712/1046797-20171226165436557-1460885786.png)

- 应用控制器会在加载原初应用时，**自动加载被包含的应用**，但是**不会启动**它们。被包含的应用顶层 supervisor 必须由包含它的应用的 supervisor 启动。



## 2、指定被包含的应用

- 可以在应用资源文件（`.app`文件）里面使用`included_applications`字段来指定包含的应用：

  ```erlang
  {included_applications, [incl_app]}
  ```



## 3、启动时同步

- 如果要在被包含应用和上级应用之间同步，可通过`start phase`字段来实现。
- `start_phases -> {Phase, PhaseArgs}`
  - `Phase`：原子类型；
  - `PhaseArgs`：任何类型；

- 包含其它应用时，`mod`字段必须为`{application_starter, [Module, StartArgs]}`

  - `Module`：应用回调模块
  - `StartArgs`：传递给`Module:start/2`的参数

  ```erlang
  {application, prim_app,
   [{description, "Tree application"},
    {vsn, "1"},
    {modules, [prim_app_cb, prim_app_sup, prim_app_server]},
    {registered, [prim_app_server]},
    {included_applications, [incl_app]},
    {start_phases, [{init,[]}, {go,[]}]},
    {applications, [kernel, stdlib, sasl]},
    {mod, {application_starter,[prim_app_cb,[]]}},
    {env, [{file, "/usr/local/log"}]}
   ]}.
  
  {application, incl_app,
   [{description, "Included application"},
    {vsn, "1"},
    {modules, [incl_app_cb, incl_app_sup, incl_app_server]},
    {registered, []},
    {start_phases, [{go,[]}]},
    {applications, [kernel, stdlib, sasl]},
    {mod, {incl_app_cb,[]}}
   ]}.
  ```

  示意图：

  ![启动时同步](https://derekuang.github.io/article_image/synchronizing_processes_during_startup.PNG)

  启动原处应用跟正常启动应用是**一样**的，如下：

  - 调用`application:start(Application)`

  - 应用控制器为应用创建`application master`
  - 进程组长（`master`）调用`Module:start(normal, StartArgs)`启动顶层`supervisor`

  这三步是与正常启动一样，接下来步骤如下：

  - 原初应用和被包含应用按照从上到下、从左到右的顺序，`master`依次为它们`start phase`

  - 对于每个应用，`master`按照原初应用中指定的`phase`顺序依次调用：

    `Module:start_phase(Phase, Type, PhaseArgs)`
    当前应用的 start_phases 中未指定的 phase 会被忽略

  被包含应用的`.app`文件需要有如下内容：

  - `{mod, {Module, StartArgs}}`：指定了应用回调模块；
    `StartArgs`会被忽略，因为只有原初应用会调用`Module:start/2`
  - `{mod, {application_starter, [Module,StartArgs]}}`：如果包含了其他应用
  - `{start_phases, [{Phase,PhaseArgs}]}`：是原初应用指定的`Phase `子集