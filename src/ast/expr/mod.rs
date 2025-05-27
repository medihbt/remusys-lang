use binop::{Assign, BinExp};
use call::Call;
use ident::Ident;
use index::ArrayIndex;
use initlist::{ArrayInitList, RawInitList};
use literal::Literal;
use unaryop::{ImplicitCast, UnaryExp};


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
    String(String),

    RawInitList(Box<RawInitList>),
    ArrayInitList(Box<ArrayInitList>),

    Ident(Ident),
    ArrayIndex(Box<ArrayIndex>),

    BinOP(Box<BinExp>),
    CmpOP(Box<BinExp>), // Comparison operators are a subset of binary operators
    ShortCircuit(Box<BinExp>), // Logical operators that short-circuit evaluation

    UnaryOP(Box<UnaryExp>),
    ImplicitCast(Box<ImplicitCast>),
    Call(Box<Call>),

    Assign(Box<Assign>),

    IntrinsicTimeStart(usize /* line number */),
    IntrinsicTimeEnd(usize /* line number */),
}

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        matches!(self, Expr::Ident(..) | Expr::ArrayIndex(..))
    }
    pub fn is_rvalue(&self) -> bool {
        !self.is_lvalue()
    }
}
