use std::{cell::RefCell, rc::{Rc, Weak}};

use block::Block;
use decl::{Function, UnresolvedVarDecl, VarDecl};
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
    UnresolvedVarDecl(Box<UnresolvedVarDecl>),
    VarDecl(Box<VarDecl>),
    FuncDecl(Rc<RefCell<Function>>),
    If(Rc<IfStmt>),
    While(Rc<WhileStmt>),
    ExprStmt(Rc<ExprStmt>),
    Return(Rc<Expr>),

    /// Unresolved `break` statement.
    /// In normalize pass, it will be resolved to `BreakTo`.
    Break,

    /// Unresolved `continue` statement.
    /// In normalize pass, it will be resolved to `ContinueTo`.
    Continue,

    BreakTo(Weak<WhileStmt>),
    ContinueTo(Weak<WhileStmt>),
}

pub struct ExprStmt {
    pub expr: RefCell<Expr>,
}
