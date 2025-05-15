use std::{cell::RefCell, rc::Rc};

use block::Block;
use decl::VarDecl;
use ifstmt::IfStmt;
use whilestmt::WhileStmt;

use super::expr::Expr;

pub mod block;
pub mod decl;
pub mod ifstmt;
pub mod whilestmt;

pub enum Stmt {
    None,
    Block(Box<Block>),
    VarDecl(Box<VarDecl>),
    If(Rc<IfStmt>),
    While(Rc<WhileStmt>),
    ExprStmt(Rc<ExprStmt>),
    Return(Rc<Expr>),
    Break,
    Continue,
}

pub struct ExprStmt {
    pub expr: RefCell<Expr>,
}
