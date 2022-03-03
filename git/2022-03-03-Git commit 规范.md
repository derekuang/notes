---
tags: git commit 规范
---



# Git commit 规范[^1]

## commit message 格式

>\<type\>\<scope\>: \<subject\>

## type(必须)

- feat: 新功能(feature)
- fix/to: 修复bug
  - fix: 产生diff并自动修复问题，适合一次提交直接修复问题。
  - to: 只产生diff不自动修复问题，适合多次提交。最终提交使用fix。
- docs: 文档(documentation)
- style: 格式(不影响代码运行的变动)
- refactor: 重构(既不是新功能，也不是修改bug的代码变动)
- perf: 优化(performance)
- test: 增加测试
- chore: 构建过程或辅助工具的变动
- revert: 版本回滚
- merge: 代码合并
- sync: 同步主线或分支的bug

## scope(可选)

> scope 用于说明 commit 影响的范围，比如数据层、控制层、视图层等等，视项目不同而不同。

## subject(必须)

> subject 是 commit 目的的简短描述，不超过 50 个字符。

- 建议使用中文（感觉中国人用中文描述问题能更清楚一些）
- 结尾不加句号或其他标点符号

## 参考来源

[^1]:如何规范你的Git commit https://www.infoq.cn/article/fVRAZhpYeLJg9jsyDdas