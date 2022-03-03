---
tags: Erlang OTP application
---

# Releases

## 1、简介

- 当存在多个应用，用这些应用加`Erlang/OTP`应用子集创建一个完整的系统，即为`release`。
- 创建`release`需要一个`release`源文件，文件中指定了`release`包含的应用。
- 该文件用于生成**启动脚本**和**release包**。可移动和安装到另一个地址的系统称为**目标系统**。



## 2、Release源文件

- **release源文件**简称`.rel`文件，文件名为`Rel.rel`。模板如下：

  ```erlang
  {release, {Name,Vsn}, {erts, EVsn},
   [{Application1, AppVsn1},
     ...
    {ApplicationN, AppVsnN}]}.
  ```

  - `{Name, Vsn}`：`release`的名字和版本号
  - `{erts, Evsn}`：基于的`ERTS`版本
  - `{ApplicationN, AppVsnN}`：应用名和版本号（`kernel`和`STDLIB`一定要有）
  - 以上变量除了`ApplicationN`是原子，其他都是字符串

- 示例

  `Applications`提过`ch_app`的`release`应该要有如下[应用资源文件](https://derekuang.github.io/2020/05/20/OTP%E8%AE%BE%E8%AE%A1%E5%8E%9F%E5%88%99-Applications.html#3%E5%BA%94%E7%94%A8%E8%B5%84%E6%BA%90%E6%96%87%E4%BB%B6)：

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

  `.rel`文件必须包含`kernel`、`STDLIB`和`sasl`，因为`ch_app`依赖于这些应用。`ch_rel-1.rel`：

  ```erlang
  {release,
   {"ch_rel", "A"},
   {erts, "5.3"},
   [{kernel, "2.9"},
    {stdlib, "1.12"},
    {sasl, "1.10"},
    {ch_app, "1"}]
  }.
  ```



## 3、生成启动脚本

- `sasl`应用的`systools`模块包含构建和检查`release`的工具。用`systools:make_script/1,2`来生成启动脚本：

  ```shell
  1> systools:make_script("ch_rel-1", [local]).
  ok
  ```

  随后创建**可读脚本**`ch_rel-1.script`和运行时系统用到的**二进制版本**`ch_rel-1.boot`

  - `ch_rel-1`：`.rel`文件的名称
  - `local`：附加选项，在启动脚本中使用应用所在的目录（绝对路径）

  通过命令行`erl -boot ch_rel-1`来加载和启动应用



## 4、创建release包

- `systools:make_tar/1,2`以`.rel`文件作为输入，输出一个zip压缩的tar文件，即**release包**：

  ```shell
  1> systools:make_script("ch_rel-1").
  ok
  2> systools:make_tar("ch_rel-1").
  ok
  ```

  一个release包默认包含：

  - `.app`文件
  - `.rel`文件
  - 所有应用的目标代码（`.beam`），代码根据应用目录结构组织
  - 二进制启动脚本，重命名为`start.boot`

  解压如下：

  ```erlang
  % tar tf ch_rel-1.tar
  lib/kernel-2.9/ebin/kernel.app
  lib/kernel-2.9/ebin/application.beam
  ...
  lib/stdlib-1.12/ebin/stdlib.app
  lib/stdlib-1.12/ebin/beam_lib.beam
  ...
  lib/sasl-1.10/ebin/sasl.app
  lib/sasl-1.10/ebin/sasl.beam
  ...
  lib/ch_app-1/ebin/ch_app.app
  lib/ch_app-1/ebin/ch_app.beam
  lib/ch_app-1/ebin/ch_sup.beam
  lib/ch_app-1/ebin/ch3.beam
  releases/A/start.boot
  releases/A/ch_rel-1.rel
  releases/ch_rel-1.rel
  ```

  `Release`包生成前，生成一个新的启动脚本（不使用`local`选项）。在`Release`包中，所有应用目录都放在`lib`目录下。由于不知道`release`包发布在哪，不能写死绝对路径（即使用相对路径）。



## 5、目录结构

- `release_handler`从`release`包安装的代码目录结构如下：

  ```shell
  $ROOT/lib/App1-AVsn1/ebin
                      /priv
           /App2-AVsn2/ebin
                      /priv
           ...
           /AppN-AVsnN/ebin
                      /priv
       /erts-EVsn/bin
       /releases/Vsn
       /bin
  ```

  - `lib `：应用目录
  - `erts-EVsn/bin`：Erlang 运行时系统可执行文件
  - `releases/Vsn`：`.rel `文件和启动文件 `start.boot`。`relup `和 `sys.config` 也在此目录下
  - `bin `：最上层的 Erlang 运行时系统可执行文件

- 无磁盘或只读客户端

  - 如系统由无磁盘或只读客户端节点组成，$ROOT 目录中还会有一个 clients 目录。只读的节点就是节点在一个只读文件系统中。

  - 每个客户端节点在 clients 中有一个子目录。每个子目录的名字是对应的节点名。一个客户端目录至少包含 bin和 releases 两个子目录。这些目录用来存放 release 的信息，以及把当前 release 指派给客户端。$ROOT 目录如下所示：

    ```shell
    $ROOT/...
        /clients/ClientName1/bin
                            /releases/Vsn
                /ClientName2/bin
                            /releases/Vsn
                ...
                /ClientNameN/bin
                            /releases/Vsn
    ```

  - 如果有不同类型的 Erlang 虚拟机，或者在不同的操作系统中，可以把 clients 分成每个类型一个子目录（TypeN）。或者每个类型设置一个 $ROOT。此时 $ROOT 目录相关的一些子目录都需要包含进来：

    ```shell
    $ROOT/...
        /clients/Type1/lib
                      /erts-EVsn
                      /bin
                      /ClientName1/bin
                                  /releases/Vsn
                      /ClientName2/bin
                                  /releases/Vsn
                      ...
                      /ClientNameN/bin
                                  /releases/Vsn
                ...
                /TypeN/lib
                      /erts-EVsn
                      /bin
                      ...
    ```

    这个结构中，Type1 的客户端的根目录为 $ROOT/clients/Type1 。

