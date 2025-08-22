# Remusys-Lang -- Simple SysY Frontend

CSCC 2025 参赛项目, 一个简易的 SysY 前端, 使用 [LALRPOP](https://github.com/lalrpop/lalrpop) 编写.

该前端并不适配任何后端——你需要自己实现后端对接逻辑. 作为 CSCC 2025 参赛项目, 对接到 [remusys-ir](https://github.com/medihbt/remusys-ir) 的代码参见编译器主程序 [Remusys-compiler](https://github.com/medihbt/remusys-compiler).

该前端项目没有继续开发的计划, 之后倘若有精力可能会考虑为 Remusys 实现一点别的东西.

## ⚠️ 警告

**该项目是实验性项目, 仅可用于学习研究, 请勿用于实际生产环境!**

**项目处于初期开发 / 测试阶段, 没有任何 API 稳定性, 代码架构、接口、实现等随时发生破坏性变化, 倘若造成后果, 则由使用者自负.**

## 技术栈说明

**解析器实现**: 本项目使用 [LALRPOP](https://github.com/lalrpop/lalrpop) 作为语法解析器生成器。LALRPOP 是 Rust 生态中成熟的 LR(1) 解析器生成工具，被广泛应用于各种编程语言前端开发中。

**个人贡献包括**:

- 完整的 SysY 语法规范设计与实现 ([grammar.lalrpop](src/grammar.lalrpop))
- AST 节点结构设计与语义分析
- 类型系统实现
- 符号表管理与作用域处理
- 与后端 IR 的对接接口设计

选择使用成熟的解析器生成器而非从零实现，是现代编译器开发的最佳实践，能够将精力集中在语言设计和语义分析等核心问题上。
