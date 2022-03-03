---
tags: 教程 hexo
---

# hexo个人博客部署

## 1. 安装软件

- node.js
- git

## 2. 安装hexo

以我个人为例，在E盘下的Blog文件夹安装hexo。

- 打开cmd，进入到E:/Blog目录下

```
E:  
mkdir Blog  
cd Blog 
```

- 用淘宝的npm源安装hexo

```
npm install -g cnpm --registry=https://registry.npm.taobao.org  
cnpm install hexo -cli -g  
cnpm intall hexo --save
```

- 在Blog目录下进入 `git bash`终端，初始化hexo  

```
 hexo init  
```

- 安装依赖及相关插件  

```
 cnpm install  
```

- 启动hexo（默认启用4000端口）

```
 hexo s
```

- 在本机浏览器访问hexo

```
 localhost:4000
```

## 3. 把hexo部署到github

- 创建仓库`derekuang.github.io`，derekuang为github的用户名

- 配置github与本机的ssh免密登录

- 进入 `git bash`

- 设置用户名和邮箱

```
 git config --global user.name "derekuang"
 git config --global user.email "k2497808365@gmail.com"
```

- 生成SSH密钥（在用户主目录下生成 `.ssh`文件夹）

```
 ssh-keygen -t rsa -C "k2497808365@gmail.com"
```

- 把 `.ssh`文件夹下的公钥 `id_rsa_pub`复制到guihub的`Setting`下的`SSH and GPG keys`下

- 验证SSH免密是否成功(如果成功会打印`Hi derekuang! You've successfully...`)

```
 ssh -T git@github.com
```

- 打开Blog目录下的`_config.yml`文件，添加远程仓库地址

```
 deploy:  
 \#\#注意以下冒号后面都要跟空格 
 	type: git  
 	repository: git@github.com:derekuang/derekuang.github.io.git  
 	branch: master
```

- 最终三部曲部署到github，适用于添加或删除了`.md`文件更新

```
 hexo clean  
 hexo g
 hexo d
```

- 以下是我的博客地址

[derekuang.github.io](derekuang.github.io)