use crate::ast::operator::Operator;

use super::Expr;

#[derive(Debug, Clone, Hash)]
pub struct BinExp {
    pub op: Operator,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Debug, Clone, Hash)]
pub struct Assign {
    pub lhs: Expr,
    pub rhs: Expr,
}
