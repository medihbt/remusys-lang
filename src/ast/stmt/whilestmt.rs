use crate::ast::expr::Expr;

pub struct WhileStmt {
    pub cond: Expr,
    pub body: Box<Expr>,
}
