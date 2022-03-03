---
tags: Erlang OTP application
---

# Distributed Applications

## 1、简介

- 在有多个节点的分布式系统中，应以分布式的方式来管理应用。如某应用所在节点崩溃，则另一个节点可以重启该应用。
-  这样的应用成为**分布式应用**，此处分布式指的是**“管理”**；如从使用应用服务的角度来看，所有的应用都是可视为分布式的。



## 2、配置分布式应用

- 分布式应用受两个进程控制：应用控制器（`application_controller`）和分布式应用控制进程（`dist_ac`）；这两个都是`kernel`应用的一部分。

- 配置分布式应用
  `distributed = [{Application, [Timeout], NodeDesc}]`

  - `Application`：应用名。
  - `NodeDesc = [Node | {Node, ..., Node}]`：应用能在上面运行的节点列表，按优先级排列，元组里面的节点是平行的。
  - `Timeout = integer()`：在其它节点上重启应用的等待时间；默认值为0。

- 启动配置参数（可运行应用的节点相互连接，协商应用在哪启动）

  - `sync_nodes_mandatory = [Node]`：指定了必须启动的其它节点
  - `sync_nodes_optional = [Node]`：指定了可以启动的其它节点
  - `sync_nodes_timeout = integer() | inginity`：指定了等待其它节点启动的超时时长

- 节点启动时会等待所有`sync_nodes_mandatory`和`sync_nodes_optionl`的节点启动；如必须的节点启动了，`sync_nodes_timeout`时长后所有应用都会被启动；若有必须的节点没启动，则当前节点会终止

- **示例**：

  - 应用`myapp`在`cp1@cave`中运行
  - 如果`cp1@cave`终止，`myapp`将在`cp2@cave`或`cp3@cave`上重启

  `cp1@cave`的系统配置`cp1.config`如下：

  ```erlang
  [{kernel,
    [{distributed, [{myapp, 5000, [cp1@cave, {cp2@cave, cp3@cave}]}]},
     {sync_nodes_mandatory, [cp2@cave, cp3@cave]},
     {sync_nodes_timeout, 5000}
    ]
   }
  ].
  ```

  `cp2@cave`和`cp3@cave`的配置几乎一样，除了`sync_nodes_mandatory`必须启动节点是另外二者

- 所有节点的`distributed`和`sync_nodes_timeout`值必须一致，否则该行为不会被定义



## 3、启动和终止分布式应用

- 当所有必须的节点被启动，通过在所有节点调用`application:start(Application)`来启动这个分布式应用。
- 可使用**引导脚本（Releases）**自动启动应用

- 应用会在配置参数`distributed`中的第一个节点启动。一如往常，控制器创建组长（application master），调用回调（创建顶层监控树）：

  ```erlang
  Module:start(normal, StartArgs)
  ```

- **示例**：

  继续上一小节例子，启动三个节点（`cpN`），指定系统配置文件（`cpN.config`）

  ```SHELL
  > erl -sname cp1 -config cp1
  > erl -sname cp2 -config cp2
  > erl -sname cp3 -config cp3
  ```

  所有节点可用时，经过**所有**节点都调用`application:start(myapp)`，`myapp`会在启动。此时在`cp1`启动，如下图：
  ![IMAGE MISSING](file:///C:/Users/Administrator/Desktop/otp_doc_19.3/doc/design_principles/dist1.gif)

  同理，在**所有**节点调用`application:stop(Application)`终止应用。



## 4、故障切换

- 若应用所在节点终止，并且在超时时间内没有重启，则会在`distributed`配置中指定下一个可用节点重启，即**故障切换**。

- 示例：

  - 如`cp1`终止

    1. 系统等待`cp1`重启5s
    2. 超时后在 cp2 和 cp3 中选择一个运行的应用最少的
    3. cp2 运行的应用比 cp3 少，myapp 将会 cp2 节点重启

    ![IMAGE MISSING](file:///C:/Users/Administrator/Desktop/otp_doc_19.3/doc/design_principles/dist2.gif)



## 5、接管

- 如一个在`distributed`配置中优先级较高的节点启动了，应用在旧节点终止，在新节点重启。即**接管**。

- 应用启动方式

  ```erlang
  Module:start({takeover, Node}, StartArgs)
  ```

  - `Node`：旧节点

- 示例：

  - 如果 `myapp `在 `cp3 `节点运行，此时 `cp2 `启动，应用**不会被重启**，因为 `cp2 `和 `cp3 `是没有先后顺序的。
    ![IMAGE MISSING](file:///C:/Users/Administrator/Desktop/otp_doc_19.3/doc/design_principles/dist4.gif)
  - 但如果 `cp1 `也重启了，函数 `application:takeover/2` 会将 `myapp `移动到 `cp1`，因为对 `myapp `来说 `cp1 `比 `cp3 `优先级高。此时节点 `cp1 `会调用 `Module:start({takeover, cp3@cave}, StartArgs)` 来启动应用。
    ![IMAGE MISSING](file:///C:/Users/Administrator/Desktop/otp_doc_19.3/doc/design_principles/dist5.gif)