use std::{cell::RefCell, rc::Rc};

use crate::{ast::expr::Expr, typing::AstType};

use super::block::Block;

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

impl UnresolvedVariable {
    pub fn new(name: String, base_type: AstType, kind: VarKind) -> Self {
        Self {
            name,
            base_type,
            kind,
            array_subscript: None,
            initval: None,
        }
    }
}

pub struct UnresolvedVarDecl {
    pub is_const: bool,
    pub base_type: AstType,
    pub line: usize,
    pub defs: Vec<RefCell<UnresolvedVariable>>,
}

pub struct VarDecl {
    pub is_const: bool,
    pub base_type: AstType,
    pub defs: Box<[Rc<Variable>]>,
}

pub struct Function {
    pub name: String,
    pub ret_type: AstType,
    pub resolved_args: Box<[Rc<Variable>]>,
    pub unresolved_args: Vec<UnresolvedVariable>,
    pub body: Option<Block>,
}

impl Function {
    pub fn new(name: String, ret_type: AstType) -> Self {
        Self {
            name,
            ret_type,
            resolved_args: Box::new([]),
            unresolved_args: vec![],
            body: None,
        }
    }
    pub fn is_extern(&self) -> bool {
        self.body.is_none()
    }
}
