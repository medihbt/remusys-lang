use crate::{ast::operator::Operator, typing::AstType};

use super::{ident::Ident, Expr, ExprItem, ExprTrait};

#[derive(Debug, Clone, Hash)]
pub struct BinExp {
    pub op: Operator,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Debug, Clone, Hash)]
pub struct Assign {
    pub lhs: Ident,
    pub rhs: Expr,
}
