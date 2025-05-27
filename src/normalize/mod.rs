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

use expr::ExprNormalizer;
use scope::Scope;

use crate::{
    ast::{
        AstModule,
        expr::{Expr, literal::Literal},
        stmt::{
            ExprStmt, Stmt,
            block::Block,
            decl::{Function, UnresolvedVarDecl, UnresolvedVariable, VarDecl, Variable},
            ifstmt::IfStmt,
            whilestmt::WhileStmt,
        },
    },
    typing::{AstType, FixedArrayType},
};

pub mod expr;
// pub mod expr_norecurse;
pub mod scope;

pub struct AstNormalizer<'a> {
    pub ast_module: &'a AstModule,
    pub scope: Vec<Scope>,
    pub loop_stack: Vec<Rc<WhileStmt>>,
}

impl<'a> AstNormalizer<'a> {
    pub fn new(ast_module: &'a AstModule) -> Self {
        Self {
            ast_module,
            scope: vec![Scope::new(0)],
            loop_stack: vec![],
        }
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

impl<'a> AstNormalizer<'a> {
    pub fn normalize(&mut self) -> AstModule {
        let mut sst_module = AstModule::new_empty_sst(self.ast_module.file.clone());
        // 把 SST 中默认导入的符号加载到当前作用域.
        for def in sst_module.global_defs.iter() {
            match def {
                Stmt::FuncDecl(func) => {
                    self.peek_scope_mut()
                        .add_func(func.name.clone(), func.clone());
                }
                Stmt::VarDecl(var_decl) => {
                    for var in var_decl.defs.iter() {
                        self.peek_scope_mut().add_var(var.name.clone(), var.clone());
                    }
                }
                _ => {}
            }
        }
        // 继续处理 AST 中的全局定义.
        self.normalize_global_defs(&mut sst_module, self.ast_module.global_defs.as_slice());
        sst_module
    }

    fn normalze_expr(&self, expr: &Expr, type_req: Option<&AstType>, should_eval: bool) -> Expr {
        let mut expr_normalizer = ExprNormalizer::from_normalizer(self, should_eval);
        let (new_expr, new_type) = expr_normalizer.normalize_build_expr(expr, type_req);
        if let Some(AstType::Bool) = type_req {
            ExprNormalizer::operand_cast_to_bool(new_expr, &new_type)
        } else {
            new_expr
        }
    }

    fn normalize_global_defs(&mut self, sst_module: &mut AstModule, global_defs: &[Stmt]) {
        for def in global_defs {
            match def {
                Stmt::UnresolvedVarDecl(var) => {
                    let new_vardecl = self.normalize_unresolved_var_decl(var);
                    sst_module
                        .global_defs
                        .push(Stmt::VarDecl(Box::new(new_vardecl)));
                }
                Stmt::VarDecl(var) => {
                    let new_vardecl = self.normalize_resolved_var_decl(var);
                    sst_module
                        .global_defs
                        .push(Stmt::VarDecl(Box::new(new_vardecl)));
                }
                Stmt::FuncDecl(func) => {
                    self.normalize_func(func, sst_module);
                }
                _ => panic!("normalize_global_defs: unexpected statement type"),
            }
        }
    }

    fn normalize_unresolved_var_decl(&mut self, var_decl: &UnresolvedVarDecl) -> VarDecl {
        let is_const = var_decl.is_const;
        let base_ty = &var_decl.base_type;
        let new_defs = self.resolve_vardefs(var_decl.defs.as_ref(), is_const, true);
        let new_var_decl = VarDecl {
            is_const,
            base_type: base_ty.clone(),
            defs: new_defs,
        };
        new_var_decl
    }
    fn normalize_resolved_var_decl(&mut self, var_decl: &VarDecl) -> VarDecl {
        let mut new_defs = vec![];
        for var in var_decl.defs.iter() {
            let initval = self.normalze_expr(&var.initval, Some(&var.var_type), var.is_const());
            new_defs.push(Rc::new(Variable {
                name: var.name.clone(),
                var_type: var.var_type.clone(),
                initval: initval,
                kind: var.kind.clone(),
            }));
        }
        self.vardecl_fill_scope_symtab(&new_defs);
        let new_var_decl = VarDecl {
            is_const: var_decl.is_const,
            base_type: var_decl.base_type.clone(),
            defs: new_defs.into_boxed_slice(),
        };
        new_var_decl
    }
    fn resolve_vardefs(
        &mut self,
        unresolved_defs: &[UnresolvedVariable],
        is_const: bool,
        add_to_scope: bool,
    ) -> Box<[Rc<Variable>]> {
        let mut new_defs = Vec::with_capacity(unresolved_defs.len());
        for uvar in unresolved_defs.iter() {
            let name = uvar.name.as_str();
            let kind = uvar.kind.get();
            let varty = if uvar.array_dims.is_empty() {
                uvar.base_type.clone()
            } else {
                self.array_dimensions_to_type(&uvar.array_dims, &uvar.base_type)
            };
            let initval = self.normalze_expr(&uvar.initval, Some(&varty), is_const);
            let var = Rc::new(Variable {
                name: name.into(),
                var_type: varty,
                initval,
                kind,
            });
            new_defs.push(var.clone());
            if add_to_scope {
                self.add_variable_to_scope(&var);
            }
        }
        new_defs.into_boxed_slice()
    }

    fn vardecl_fill_scope_symtab(&mut self, vardefs: &[Rc<Variable>]) {
        for var in vardefs.iter() {
            self.add_variable_to_scope(var);
        }
    }
    fn add_variable_to_scope(&mut self, var: &Rc<Variable>) {
        if self.peek_scope().symbols.contains_key(&var.name) {
            panic!(
                "Variable {} already exists in the current scope {}  (inner layer {})",
                var.name,
                self.scope.len() - 1,
                self.peek_scope().layer
            );
        }
        if var.is_const() {
            self.peek_scope_mut().add_const(
                var.name.clone(),
                var.initval.clone(),
                var.var_type.clone(),
                var.clone(),
            );
        } else {
            self.peek_scope_mut().add_var(var.name.clone(), var.clone());
        }
    }

    fn array_dimensions_to_type(&self, arrsub: &[Expr], basety: &AstType) -> AstType {
        let mut arrsub = arrsub
            .iter()
            .map(|x| self.normalze_expr(x, Some(&AstType::Int), true))
            .collect::<Vec<_>>();
        // SysY 的数组类型顺序是从外到内, 但是我们需要从内到外.
        // 例如, 在 SysY 源码中的数组: `int a[2][3][4];`
        // Remusys 类型系统要求的数组类型: `[[i32; 4]; 3]; 2]`
        arrsub.reverse();

        let mut arrty = basety.clone();
        for (idx, dim) in arrsub.iter().enumerate() {
            arrty = match dim {
                Expr::None => {
                    if idx == arrsub.len() - 1 {
                        AstType::DynArray(Rc::new(arrty))
                    } else {
                        panic!(
                            "Invalid array dimension: Dynamic array dimension must be the outermost"
                        )
                    }
                }
                Expr::Literal(Literal::Int(i)) => {
                    if *i == 0 {
                        panic!("Invalid array dimension: Array dimension cannot be zero")
                    }
                    AstType::FixedArray(Rc::new(FixedArrayType {
                        elemty: arrty,
                        nelems: *i as usize,
                    }))
                }
                _ => panic!("Invalid array dimension: Array dimension must be a constant integer"),
            }
        }
        arrty
    }

    fn normalize_func(&mut self, old_func: &Function, sst_module: &mut AstModule) {
        // 解析参数
        let args = self.resolve_vardefs(old_func.unresolved_args.as_slice(), false, false);
        let new_func = Rc::new(Function {
            name: old_func.name.clone(),
            ret_type: old_func.ret_type.clone(),
            resolved_args: args,
            unresolved_args: old_func.unresolved_args.clone(),
            is_vararg: old_func.is_vararg,
            body: RefCell::new(None),
            attr: old_func.attr.clone(),
        });
        sst_module
            .global_defs
            .push(Stmt::FuncDecl(new_func.clone()));
        if old_func.is_extern() {
            // extern 函数不需要处理函数体
            return;
        }

        // 把函数和参数放进符号表
        self.peek_scope_mut()
            .add_func(new_func.name.clone(), new_func.clone());
        self.push_scope();
        self.vardecl_fill_scope_symtab(new_func.resolved_args.as_ref());

        let newblock = {
            let block = old_func.body.borrow();
            self.normalize_block(block.as_ref().unwrap())
        };
        new_func.body.replace(Some(newblock));
        self.pop_scope();
    }

    fn normalize_block(&mut self, block: &Block) -> Block {
        let mut newblock = Block {
            stmts: Vec::with_capacity(block.stmts.len()),
        };
        self.push_scope();
        for stmt in block.stmts.iter() {
            let new_stmt = self.normalize_stmt(stmt);
            match &new_stmt {
                Stmt::None => {}
                _ => newblock.stmts.push(new_stmt),
            }
        }
        self.pop_scope();
        newblock
    }

    fn normalize_stmt(&mut self, stmt: &Stmt) -> Stmt {
        match stmt {
            Stmt::None => Stmt::None,
            Stmt::Block(block) => {
                let new_block = self.normalize_block(block);
                Stmt::Block(Box::new(new_block))
            }
            Stmt::UnresolvedVarDecl(var) => {
                let new_vardecl = self.normalize_unresolved_var_decl(var);
                Stmt::VarDecl(Box::new(new_vardecl))
            }
            Stmt::VarDecl(var) => {
                let new_vardecl = self.normalize_resolved_var_decl(var);
                Stmt::VarDecl(Box::new(new_vardecl))
            }
            Stmt::FuncDecl(_) => panic!("Cannot handle function declaration in block"),
            Stmt::If(if_stmt) => {
                let new_if_stmt = self.normalize_if_stmt(if_stmt);
                Stmt::If(Rc::new(new_if_stmt))
            }
            Stmt::While(while_stmt) => {
                let new_while_stmt = self.normalize_while_stmt(while_stmt);
                Stmt::While(new_while_stmt)
            }
            Stmt::ExprStmt(expr_stmt) => {
                let expr = self.normalze_expr(&expr_stmt.expr.borrow(), None, false);
                Stmt::ExprStmt(Rc::new(ExprStmt {
                    expr: RefCell::new(expr),
                }))
            }
            Stmt::Return(expr) => {
                let expr = self.normalze_expr(expr, None, false);
                Stmt::Return(Rc::new(expr))
            }
            Stmt::Break => {
                if self.loop_stack.is_empty() {
                    panic!("break statement not in loop");
                }
                let weak = Rc::downgrade(self.loop_stack.last().unwrap());
                Stmt::BreakTo(weak)
            }
            Stmt::Continue => {
                if self.loop_stack.is_empty() {
                    panic!("continue statement not in loop");
                }
                let weak = Rc::downgrade(self.loop_stack.last().unwrap());
                Stmt::ContinueTo(weak)
            }
            Stmt::BreakTo(_) | Stmt::ContinueTo(_) => {
                panic!("cannot handle resolved break/continue")
            }
        }
    }

    fn normalize_if_stmt(&mut self, if_stmt: &IfStmt) -> IfStmt {
        let cond = self.normalze_expr(&if_stmt.cond, Some(&AstType::Bool), false);
        let then_stmt = self.normalize_stmt(&if_stmt.then_stmt);
        let else_stmt = if_stmt.else_stmt.as_ref().map(|s| self.normalize_stmt(s));
        IfStmt {
            cond,
            then_stmt: Box::new(then_stmt),
            else_stmt: else_stmt.map(|s| Box::new(s)),
        }
    }

    fn normalize_while_stmt(&mut self, while_stmt: &Rc<WhileStmt>) -> Rc<WhileStmt> {
        let cond = self.normalze_expr(&while_stmt.cond, Some(&AstType::Bool), false);
        let new_while_stmt = Rc::new(WhileStmt {
            cond,
            body: RefCell::new(Box::new(Stmt::None)),
        });
        self.loop_stack.push(Rc::clone(&new_while_stmt));
        let body = self.normalize_stmt(&while_stmt.body.borrow());
        new_while_stmt.body.replace(Box::new(body));
        self.loop_stack.pop();
        new_while_stmt
    }
}
