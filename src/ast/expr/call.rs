use super::{Expr, ident::Ident};

#[derive(Debug, Clone, Hash)]
pub struct Call {
    pub name: Ident,
    pub args: Vec<Expr>,
}
