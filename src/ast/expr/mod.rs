use binop::{Assign, BinExp};
use call::Call;
use ident::Ident;
use index::ArrayIndex;
use initlist::{ArrayInitList, RawInitList};
use literal::Literal;
use unaryop::{ImplicitCast, UnaryExp};

use crate::typing::AstType;

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

    IntrinsicTimeStart(usize /* line number */),
    IntrinsicTimeEnd(usize /* line number */),
}

pub struct ExprTrait {
    pub valtype: AstType,
    pub is_normalized: bool,
    pub is_constexpr: bool,
    pub is_lvalue: bool,
}

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        matches!(self, Expr::Ident(..) | Expr::ArrayIndex(..))
    }
    pub fn is_rvalue(&self) -> bool {
        !self.is_lvalue()
    }
}
