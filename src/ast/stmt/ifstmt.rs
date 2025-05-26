use crate::ast::expr::Expr;

use super::Stmt;

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub cond: Expr,
    pub then_stmt: Box<Stmt>,
    pub else_stmt: Option<Box<Stmt>>,
}
