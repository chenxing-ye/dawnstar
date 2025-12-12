# Dawnstar 游戏服务器框架

Dawnstar 是一个基于 Erlang/OTP 构建的高性能游戏服务器框架，提供了灵活的配置管理、强大的测试支持和全面的性能测试策略。

## 项目概述

Dawnstar 游戏服务器框架旨在为游戏开发提供一个稳定、高效且易于扩展的基础架构。框架采用模块化设计，支持配置管理、网络通信、日志系统等核心功能，便于开发者快速构建自己的游戏服务器。

## 快速开始

### 构建项目

在项目根目录下执行以下命令来构建项目：

```bash
$ rebar3 compile
```

### 运行测试

执行以下命令来运行所有测试：

```bash
$env:DIAGNOSTIC=1; rebar3 eunit
```

### 项目结构

```
apps/
├── dawnstar/            # 主应用
└── framework/           # 框架核心模块
    ├── src/             # 源代码目录
    │   └── conf/        # 配置模块
    │       ├── fw_conf.erl
    │       └── fw_conf_loader.erl
    └── test/            # 测试文件目录
        ├── fw_conf_gen_tests.erl
        ├── fw_conf_loader_tests.erl
        └── fw_conf_tests.erl
```

## 核心功能

- **灵活的配置管理系统**：支持从文件加载配置、配置合并、动态更新等功能
- **模块化设计**：便于扩展和维护
- **强大的测试支持**：集成 EUnit 和 Common Test 框架
- **全面的性能测试策略**：提供多种性能测试方法和工具

## 文档

- [开发文档](DEVELOPMENT.md)：详细的开发记录、进展和开发流程规范
- [测试指南](TESTING_GUIDE.md)：项目的测试策略、EUnit 单元测试使用说明和性能测试方法
- [Common Test 指南](COMMON_TEST_GUIDELINE.md)：Common Test 框架的使用指南
- [VSCode Erlang 报错解决指南](VSCODE_ERLANG_TROUBLESHOOTING.md)：VSCode Erlang 开发环境的报错解决方法
- [许可证](LICENSE.md)：Apache License 2.0

## 许可证

Dawnstar 游戏服务器框架采用 Apache License 2.0 许可证。详情请参阅 [LICENSE.md](LICENSE.md) 文件。
