use std::cell::RefCell;

use crate::ast::expr::Expr;

use super::Stmt;

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub cond: Expr,
    pub body: RefCell<Box<Stmt>>,
}
