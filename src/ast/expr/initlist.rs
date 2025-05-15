use crate::typing::AstType;

use super::Expr;

use std::hash::Hash;

#[derive(Debug, Clone, Hash)]
pub struct ArrayInitList {
    pub exprs: Box<[Expr]>,
    pub vtype: AstType,
}

#[derive(Debug, Clone, Hash)]
pub struct RawInitList {
    pub exprs: Vec<Expr>,
}

impl RawInitList {
    pub fn new(exprs: Vec<Expr>) -> Self {
        Self { exprs }
    }
}
