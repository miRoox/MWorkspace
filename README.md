# MWorkspace

仿MATLAB的工作区(Workspace)。

![demo](/demo.png)

## 功能

- [x] 符号监视
- [ ] 符号元信息（类型、属性、尺寸）
- [x] 上下文切换
- [x] 符号过滤
- [x] 新建符号
- [ ] 编辑选择部分
- [x] 导入数据
- [ ] 导出选择部分
- [x] 清除/移除选择部分
- [ ] 插入推荐
- [ ] 面板选项

## 安装

在[发布页面](https://github.com/miRoox/MWorkspace/releases)下载发布的`.palcet`文件，
然后使用`PacletInstall`函数将其安装到Mathematica中。例如

```mathematica
Needs["PacletManager`"]
PacletInstall["~/Downloads/MWorkspace-0.2.0.paclet"]
```
