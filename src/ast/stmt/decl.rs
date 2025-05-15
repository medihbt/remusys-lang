use std::{cell::RefCell, rc::Rc};

use crate::{ast::expr::Expr, typing::AstType};

pub enum VarKind {
    GlobalConst,
    GlobalVar,
    LocalConst,
    LocalVar,
    FuncArg,
}

pub struct Variable {
    pub name: String,
    pub var_type: AstType,
    pub initval: RefCell<Expr>,
    pub kind: VarKind,
}

pub struct UnresolvedVariable {
    pub name: String,
    pub base_type: AstType,
    pub kind: VarKind,
    pub array_subscript: Option<Box<[Expr]>>,
    pub initval: Option<Expr>,
}

pub enum VarDefChain {
    None,
    Unresolved(Vec<RefCell<UnresolvedVariable>>),
    Resolved(Box<[Rc<Variable>]>),
}

pub struct VarDecl {
    pub is_const: bool,
    pub base_type: AstType,
    pub defs: VarDefChain,
}

pub struct Function {
    pub name: String,
    pub ret_type: AstType,
    pub args: VarDefChain,
}
