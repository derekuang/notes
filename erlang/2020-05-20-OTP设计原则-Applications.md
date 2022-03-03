---
tags: Erlang OTP application
---

# Applications

## 1、应用的概念

- 目的：当实现了一些特定的功能，想封装成一个应用以**作为一个整体启动和终止**，并在其他系统重用。
- 方法：
  - 创建应用回调模块，描述如何启动和终止这个应用
  - 创建应用规格说明（application specification），放在应用资源文件中，该文件指定了组成应用的模块列表以及回调模块名。



## 2、应用回调模块

- 回调函数：

  - `start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State}`
- 在启动应用时调用，通过顶层的`supervisor`创建监控树
  
- `StartType`：通常是`normal`
  
- `StartArgs`：由应用资源文件中的mod指定
  
- `Pid`：顶层`supervisor`的`pid`
  
- `State`：状态值会传递给`stop`函数
  
- `stop(State)`
  
  - 在应用停止后调用，做清理工作（实际应用即监控树的终止是自动处理的）
  
- 打包前文的`supervisor behaviour`监控树作为应用，回调模块如下：

  ```erlang
  -module(ch_app).
  -behaviour(application).
  
  -export([start/2, stop/1]).
  
  start(_Type, _Args) ->
      ch_sup:start_link().
  
  stop(_State) ->
      ok.
  ```

  库应用（如`STDLIB`）不需要启动和终止（只是提供特定的功能）所以不需要回调模块。



## 3、应用资源文件

- 应用的规格说明用来配置一个应用，放在应用资源文件中，简称`.app`文件：

  ```erlang
  {application, Application, [Opt1, ..., OptN]}.
  ```

  - `Application`：应用的名称；原子类型；资源文件名必须是`Application.app`
  - `Opt`：应用的特定属性；`{Key-Value}`元组；每个`Key`都是可选的且有默认值

- 库应用最简单的`.app`文件：

  ```erlang
  {application, libapp, []}.
  ```

- 有监控树的应用最简单的`.app`文件：

  ```erlang
  {application, ch_app,
   [{mod, {ch_app,[]}}]}.
  ```

- 示例

  ```erlang
  {application, ch_app,
   [{description, "Channel allocator"},
    {vsn, "1"},
    {modules, [ch_app, ch_sup, ch3]},
    {registered, [ch3]},
    {applications, [kernel, stdlib, sasl]},
    {mod, {ch_app,[]}}
   ]}.
  ```
  
  参数解析：
  
  - `description`：简短的描述；字符串；默认值“”
  - `vsn`：版本号；字符串；默认值“”
  - `modules`：应用引入的所有模块，生成启动脚本和`tar`文件时`systools`会用此列表；默认值[]
  - `registerd`：应用中所有注册的进程名，`systools`会用来检测不同应用的名字冲突；默认值[]
  - `applications`：必须在本应用启动前启动的应用，`systools`会用来生成正确地启动脚本；默认值[]；所有应用都至少依赖于`kernel`和`STDLIB`
  - `mod`：有监控树的应用的回调模块以及启动参数



## 4、目录结构

- **开发环境目录结构准则**

  ```
      ─ ${application}（应用名）
        ├── doc（推荐；所有的源文档应放在此目录的子目录下）
        │   ├── internal（推荐；应用的实现细节（不对外）相关文档放在此处）
        │   ├── examples（推荐；存放示例源码。建议大家把对外文档的示例放在此处）
        │   └── src（推荐；存放所有的文档源文件（包括Markdown、AsciiDoc 和 XML 文件））
        ├── include（可选；存放能被其他应用访问到的 include 文件）
        ├── priv（可选；存放应用运行时需要的资源，如可执行文件、动态链接等）
        ├── src（必须；容纳 Erlang 源码、.app 文件和应用内部使用的 include 文件）
        │   └── ${application}.app.src
        └── test（推荐；存放测试相关的所有文件，包括测试规范和测试集等）
  ```

  - 开发环境可能有其他语言的源码，应该把他们放在其他目录。按照惯例，以语言名为前缀命名目录，如C语言用`c_src`。后缀 `_src `意味着这个文件夹里的文件是编译和应用步骤中的一部分。最终构建好的文件应放在 `priv/lib` 或 `priv/bin` 目录下。

- **发布环境的目录结构**

  ```
      ─ ${application}-${version}（应用-版本）
        ├── bin（可选；存放应用生成的可执行文件，比如 escript 或 shell-script）
        ├── doc（可选；存放发布文档）
        │   ├── html（可选；存放应用的 html 文档）
        │   ├── man[1-9]（1：推荐；存放应用可执行文件的帮助文档；
        					3：推荐；存放模块 API 的帮助文档；
        					6：推荐；存放应用概述帮助文档）
        │   ├── pdf（可选；存放应用的 pdf 文档）
        │   ├── internal
        │   └── examples
        ├── ebin（必须；包含 Erlang 目标代码 beam 文件，.app 文件也必须要放在这里）
        │   └── ${application}.app
        ├── include（可选；存放能被其他应用访问到的 include 文件）
        ├── priv（可选；存放应用相关的文件，可用 code:priv_dir/1 函数访问此目录）
        │   ├── lib（推荐；存放应用需要用到的共享对象（ shared-object ）文件）
        │   └── bin（推荐；存放应用需要用到的可执行文件，例如 port-program）
        └── src（可选；容纳Erlang源码、.app文件和应用内部使用的include文件；发布版本中不必要用到）
  ```



## 5、应用控制器

- 当erlang运行时系统（erts）启动，`kernel`应用会启动很多进程，其中一个是**应用控制器（application controller）**进程，注册名为`application_controller`。
- 应用的所有操作都通过该控制器协调，使用了`application`模块的一些函数；控制应用的加载、卸载、启动和终止。



## 6、加载和卸载应用

- 在应用启动之前，应该要先**加载**（并非加载代码）。控制器会读取并存储`.app`文件的信息：

  > ```shell
  > 1> application:load(ch_app).
  > ok
  > 2> application:loaded_applications().
  > [{kernel,"ERTS  CXC 138 10","2.8.1.3"},
  >  {stdlib,"ERTS  CXC 138 10","1.11.4.3"},
  >  {ch_app,"Channel allocator","1"}]
  > ```
  
- 可以**卸载**终止或未启动的应用。卸载时，控制器会把相关信息从内部数据库删除：

  > ```shell
  > 3> application:unload(ch_app).
  > ok
  > 4> application:loaded_applications().
  > [{kernel,"ERTS  CXC 138 10","2.8.1.3"},
  >  {stdlib,"ERTS  CXC 138 10","1.11.4.3"}]
  > ```

  

## 7、启动和终止应用

- 启动应用

  ```shell
  5> application:start(ch_app).
  ok
  6> application:which_applications().	%% 已经启动的应用
  [{kernel,"ERTS  CXC 138 10","2.8.1.3"},
   {stdlib,"ERTS  CXC 138 10","1.11.4.3"},
   {ch_app,"Channel allocator","1"}]
  ```

  - 控制器会校验`applications`的值，确保配置中所有应用在此应用运行前都已经启动。

  - 随后控制器为应用创建一个`application master`，是应用中所有进程的**组长（Group Leader）**。`master`通过调用应用回调函数`start/2`启动应用。

  

- 终止应用

  ```shell
  7> application:stop(ch_app).
  ok
  ```

  `master`通过关闭顶层`supervisor`来终止应用。最后`master`调用应用回调函数`stop/1`。



## 8、配置应用

- 使用**配置参数**来配置应用。配置参数就是应用资源文件（`.app`文件）中的`env`字段，对应一个`{Par,Val}`列表：

  ```erlang
  {env, [{file, "/user/local/log"}, {...}, ...]}
  ```

  - `Par`：原子类型
  - `Val`：任意类型
  - **系统配置文件（Name.config）**的配置可以**覆盖**应用资源文件的配置（原子名冲突），启动时，可通过命令行参数`-config Name`指定配置文件。
  - **命令行指定的值**可覆盖上述的两类值

- 配置级别：

  命令行 > 系统配置文件 > 应用资源文件



## 9、应用启动类型

- 应用启动时可指定**启动类型**：

  ```erlang
  application:start(Application, Type)
  ```

  `Type`：

  - `permanent`：应用终止时，其它所用应用和`erts`都会终止。
  - `transient`：如终止理由是`normal`，则只会有终止报告；如非`normal`，则与`permanent`处理方式一样。（没什么用）
  - `temporary`：应用终止总是只有终止报告。（默认）

- 通过`application:stop/1`显示终止一个应用时，不管启动类型是什么，其他应用都不会被影响。

