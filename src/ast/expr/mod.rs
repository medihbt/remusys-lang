use super::operator::Operator;
use crate::typing::AstType;
use binop::{Assign, BinExp};
use call::Call;
use ident::Ident;
use index::ArrayIndex;
use initlist::{ArrayInitList, RawInitList};
use literal::Literal;
use unaryop::{ImplicitCast, UnaryExp};
use std::{cell::RefCell, hash::Hash};

pub mod binop;
pub mod call;
pub mod ident;
pub mod index;
pub mod initlist;
pub mod literal;
pub mod unaryop;

#[derive(Debug, Clone, Hash)]
pub enum Expr {
    None,
    Literal(Literal),
    String(Box<str>),

    RawInitList(RawInitList),
    ArrayInitList(ArrayInitList),

    Ident(Ident),
    ArrayIndex(Box<ArrayIndex>),

    BinOP(Box<BinExp>),
    ShortCircuit(Box<BinExp>),

    UnaryOP(Box<UnaryExp>),
    ImplicitCast(Box<ImplicitCast>),
    Call(Box<Call>),

    Assign(Box<Assign>),
}

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        matches!(self, Expr::Ident(..) | Expr::ArrayIndex(..))
    }
    pub fn is_rvalue(&self) -> bool {
        !self.is_lvalue()
    }
}
