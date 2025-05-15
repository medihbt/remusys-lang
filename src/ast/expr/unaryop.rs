use crate::{ast::operator::Operator, typing::AstType};

use super::Expr;

#[derive(Debug, Clone, Hash)]
pub struct UnaryExp {
    pub op: Operator,
    pub expr: Expr,
}

#[derive(Debug, Clone, Hash)]
pub struct ImplicitCast {
    pub kind: Operator,
    pub expr: Expr,
    pub target: AstType,
}
