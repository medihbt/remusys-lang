use crate::typing::AstType;

use super::Expr;

use std::hash::Hash;

#[derive(Debug, Clone, Hash)]
pub struct ArrayInitList {
    pub final_elems: Box<[Expr]>,
    pub type_levels: Box<[AstType]>,
    pub dimensions: Box<[(usize /* dimension */, usize /* element size */)]>,
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
