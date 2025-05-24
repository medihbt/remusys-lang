use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{attr::Attr, expr::Expr},
    typing::AstType,
};

use super::block::Block;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    pub initval: Expr,
}

impl UnresolvedVariable {
    pub fn new(name: String, base_type: AstType, kind: VarKind) -> Self {
        Self {
            name,
            base_type,
            kind,
            array_subscript: None,
            initval: Expr::None,
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
    pub is_vararg: bool,
    pub body: Option<Block>,
    pub attr: Option<Attr>,
}

impl Function {
    pub fn new(name: String, ret_type: AstType, is_vararg: bool) -> Self {
        Self {
            name,
            ret_type,
            resolved_args: Box::new([]),
            unresolved_args: vec![],
            is_vararg,
            body: None,
            attr: None,
        }
    }
    pub fn is_extern(&self) -> bool {
        self.body.is_none()
    }
    pub fn header_is_resolved(&self) -> bool {
        !self.resolved_args.is_empty()
    }
    pub fn resolve_attr(&mut self) {
        let new_attr = if let Some(Attr::Unresolved(attr)) = self.attr.as_ref() {
            Some(attr.resolve())
        } else {
            return;
        };
        self.attr = new_attr;
    }
    pub fn is_intrinsic(&self) -> bool {
        self.intrinsic_get_id().is_some()
    }
    pub fn intrinsic_get_id(&self) -> Option<&String> {
        self.attr.as_ref().and_then(|attr| match attr {
            Attr::Intrinsic { id } => Some(id),
            _ => None,
        })
    }
    pub fn n_fixed_args(&self) -> usize {
        self.resolved_args.len()
    }
    pub fn get_arg_type(&self, index: usize) -> Option<&AstType> {
        if !self.header_is_resolved() {
            panic!("Function header is not resolved");
        }
        if index < self.resolved_args.len() {
            Some(&self.resolved_args[index].var_type)
        } else {
            None
        }
    }
}
