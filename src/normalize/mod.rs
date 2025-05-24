//! 把初步解析得到的抽象语法树 AST 规范化为语义语法树 SST.
//!
//! 初步解析得到的 AST 中包含大量中间代码生成器无法识别、意义不明的结点, 例如
//! `UnresolvedVariable` 的具体类型未知、`RawInitList` 初始化列表层级不一、
//! 杂乱无章。规范化操作会将 AST 中的这些结点转换为语义语法树 SST 中的结点,
//! 使得 SST 中的每个结点都有明确的语义, 便于后续的中间代码生成.
//!
//! 规范化操作在表达式上的的主要工作是:
//!
//! * 根据变量定义的类型, 把 `RawInitList` 转换为 `ArrayInitList`.
//! * 如果表达式可以在编译期求值, 就求值.
//! * 计算表达式的类型, 通过添加 `ImplicitCast` 把不符合 SST 类型要求的
//!   表达式转换为符合要求的表达式. 例如， SST 的加法表达式要求两边操作数
//!   类型一致, 但 AST 没有这个要求. 在规范化时, 需要添加 `ImplicitCast`
//!   把两边操作数转换为相同的类型; 又比如, 加法表达式要求两个操作数都是右值,
//!   但 AST 没有这个要求. 在规范化时, 需要添加 `LValueToRValue` 这种
//!   隐式转换.
//!
//! 规范化操作在变量定义上的主要工作是:
//!
//! * 计算变量定义的类型. AST 的变量定义结点和 SST 的变量定义结点类型不一致,
//!   前者是类型不完整的 `UnresolvedVariable`, 后者是类型明确的 `Variable`.
//!   在规范化时, 需要根据数组下标计算出变量的实际类型, 把 `UnresolvedVariable`
//!   转换为 `Variable`.
//! * 常量的求值: 被标记为 `const` 的变量可能会用作数组维度等常量用途, 在编译期就要被求值.

use core::panic;
use std::{cell::RefCell, rc::Rc};

use expr::ExprEvaluator;
use scope::Scope;

use crate::{
    ast::{
        AstModule,
        expr::{Expr, literal::Literal},
        stmt::{
            Stmt,
            decl::{UnresolvedVarDecl, VarDecl, VarKind, Variable},
            whilestmt::WhileStmt,
        },
    },
    typing::{AstType, FixedArrayType},
};

pub mod expr;
pub mod scope;

pub struct AstNormalizer<'a> {
    pub module: &'a mut AstModule,
    pub scope: Vec<Scope>,
    pub loop_stack: Vec<Rc<WhileStmt>>,
}

impl<'a> AstNormalizer<'a> {
    pub fn new(module: &'a mut AstModule) -> Self {
        Self {
            module,
            scope: vec![Scope::new(0)],
            loop_stack: vec![],
        }
    }

    pub fn run_on_module(&mut self) {
        self.run_on_global_defs(self.module.global_defs.as_mut_slice());
    }

    fn run_on_global_defs(&mut self, defs: &mut [Stmt]) {
        for def in defs.iter_mut() {
            match def {
                Stmt::UnresolvedVarDecl(var) => {
                    let var_decl = self.run_on_unresolved_var_decl(var);
                    Stmt::VarDecl(Box::new(var_decl))
                }
                Stmt::FuncDecl(func) => todo!(),
                Stmt::VarDecl(var_decl) => todo!(),
                _ => panic!("AST root only support global definitions"),
            };
        }
    }

    fn run_on_resolved_var_decl(&mut self, var_decl: &mut VarDecl) {
        let top_scope = self.peek_scope_mut();
        for var in var_decl.defs.iter_mut() {
            let name = var.name.clone();
            let var_type = var.var_type.clone();
            let initval = var.initval.borrow();
            match var.kind {
                VarKind::GlobalConst | VarKind::LocalConst => {
                    top_scope.add_const(name, initval.clone(), var_type, Rc::clone(var));
                }
                VarKind::GlobalVar | VarKind::LocalVar | VarKind::FuncArg => {
                    top_scope.add_var(name, Rc::clone(var))
                }
            }
        }
    }
    fn run_on_unresolved_var_decl(&mut self, var_decl: &UnresolvedVarDecl) -> VarDecl {
        let is_const = var_decl.is_const;
        let base_type = var_decl.base_type.clone();
        let mut resolved = Vec::with_capacity(var_decl.defs.len());
        for def in var_decl.defs.iter() {
            let def = def.borrow();
            let name = def.name.as_str();
            let kind = def.kind;

            // init array subscripts
            let final_type = match def.array_subscript.as_ref() {
                None => base_type.clone(),
                Some(subscript) => self.array_subscript_to_type(base_type.clone(), subscript, true),
            };

            // init value
            let init_val = self.run_on_expr(&def.initval, Some(&final_type), is_const);

            resolved.push(Rc::new(Variable {
                name: name.to_string(),
                kind,
                var_type: final_type,
                initval: RefCell::new(init_val),
            }));
        }

        VarDecl {
            is_const,
            base_type,
            defs: resolved.into_boxed_slice(),
        }
    }

    fn array_subscript_to_type(
        &mut self,
        base_type: AstType,
        subscript: &[Expr],
        should_eval: bool,
    ) -> AstType {
        let mut evaluated_subscript = subscript
            .iter()
            .map(|s| self.run_on_expr(s, None, should_eval))
            .collect::<Vec<_>>();
        evaluated_subscript.reverse();
        let mut final_type = base_type;
        for e in subscript.iter() {
            match e {
                Expr::None => final_type = AstType::DynArray(Rc::new(final_type)),
                Expr::Literal(Literal::Int(i)) => {
                    if *i == 0 {
                        panic!("Array subscript should be a non-zero integer")
                    } else {
                        final_type = AstType::FixedArray(Rc::new(FixedArrayType {
                            elemty: final_type,
                            nelems: *i as usize,
                        }))
                    }
                }
                _ => panic!("Array subscript should be a integral constant"),
            }
        }
        final_type
    }

    fn run_on_expr(
        &mut self,
        expr: &Expr,
        type_request: Option<&AstType>,
        should_eval: bool,
    ) -> Expr {
        let mut expr_normalizer = ExprEvaluator::from_normalizer(self, should_eval);
        expr_normalizer.normalize_build_expr(expr, type_request).0
    }

    fn peek_scope(&self) -> &Scope {
        self.scope.last().unwrap()
    }
    fn peek_scope_mut(&mut self) -> &mut Scope {
        self.scope.last_mut().unwrap()
    }
    fn push_scope(&mut self) {
        self.scope.push(Scope::new(self.peek_scope().layer + 1));
    }
    fn pop_scope(&mut self) {
        self.scope.pop();
    }
}
