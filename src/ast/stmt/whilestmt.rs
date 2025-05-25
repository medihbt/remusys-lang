use std::cell::RefCell;

use crate::ast::expr::Expr;

use super::Stmt;

pub struct WhileStmt {
    pub cond: Expr,
    pub body: RefCell<Box<Stmt>>,
}
