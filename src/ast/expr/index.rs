use std::{
    cell::RefCell,
    hash::{Hash, Hasher},
};

use crate::typing::AstType;

use super::Expr;

#[derive(Debug, Clone)]
pub struct ArrayIndex {
    pub indexee: Expr,
    pub index: Expr,
    pub vtype: RefCell<AstType>,
}

impl Hash for ArrayIndex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.indexee.hash(state);
        self.index.hash(state);
    }
}
