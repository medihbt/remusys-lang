use crate::typing::AstType;

use super::operator::Operator;

#[derive(Debug, Clone)]
pub enum Expr {
    None,
    Literal(Literal),
    InitList(Box<[Expr]>),

    Ident(Ident),
    ArrayIndex(Box<ArrayIndex>),

    BinOP(Box<BinExp>),
    UnaryOP(Box<UnaryExp>),
    ImplicitCast(Box<ImplicitCast>),
    LValueToRValue(Box<LValueToRValue>),
    Call(Box<Call>),
}

#[derive(Debug, Clone, Copy)]
pub enum Literal {
    Int(i32),
    Float(f32),
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: Box<str>,
}
#[derive(Debug, Clone)]
pub struct ArrayIndex {
    pub indexee: Expr,
    pub index: Expr,
}

#[derive(Debug, Clone)]
pub struct BinExp {
    pub op: Operator,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Debug, Clone)]
pub struct UnaryExp {
    pub op: Operator,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct ImplicitCast {
    pub expr: Expr,
    pub cast_type: AstType,
}

#[derive(Debug, Clone)]
pub struct LValueToRValue {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub name: Ident,
    pub args: Vec<Expr>,
}
