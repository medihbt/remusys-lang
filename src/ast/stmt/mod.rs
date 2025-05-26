use std::{
    cell::RefCell, fmt::Debug, rc::{Rc, Weak}
};

use block::Block;
use decl::{Function, UnresolvedVarDecl, VarDecl};
use ifstmt::IfStmt;
use whilestmt::WhileStmt;

use super::expr::Expr;

pub mod block;
pub mod decl;
pub mod ifstmt;
pub mod whilestmt;

#[derive(Debug, Clone)]
pub enum Stmt {
    None,
    /// Syntax:
    ///
    /// ```BNF
    /// block ::= '{' stmt* '}'
    /// ```
    Block(Box<Block>),
    UnresolvedVarDecl(Box<UnresolvedVarDecl>),

    /// SST-only, no syntax
    VarDecl(Box<VarDecl>),
    FuncDecl(Rc<Function>),
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

    /// SST-only, no syntax
    BreakTo(Weak<WhileStmt>),
    /// SST-only, no syntax
    ContinueTo(Weak<WhileStmt>),
}

#[derive(Clone)]
pub struct ExprStmt {
    pub expr: RefCell<Expr>,
}

impl Debug for ExprStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner = self.expr.borrow();
        inner.fmt(f)
    }
}

impl Stmt {
    /// Check if the statement only appears in SST.
    pub fn is_sst_only(&self) -> bool {
        matches!(
            self,
            Stmt::VarDecl(_) | Stmt::BreakTo(_) | Stmt::ContinueTo(_)
        )
    }
}
