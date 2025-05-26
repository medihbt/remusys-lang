use std::{
    cell::Cell, fmt::Debug, hash::{Hash, Hasher}, rc::Weak
};

use crate::{
    ast::stmt::decl::{Function, Variable},
    typing::AstType,
};

#[derive(Clone)]
pub enum Ident {
    Unresolved(String, Cell<usize> /* line number */),
    Variable(Weak<Variable>),
    Func(Weak<Function>),
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unresolved(s, _) => write!(f, "Unresolved({})", s),
            Self::Variable(v) => write!(f, "Variable({})", v.upgrade().unwrap().name),
            Self::Func(func) => write!(f, "Func({})", func.upgrade().unwrap().name),
        }
    }
}

impl Ident {
    pub fn get_name(&self) -> String {
        self.read_name(|s| s.to_string())
    }
    pub fn read_name<R>(&self, reader: impl FnOnce(&str) -> R) -> R {
        match self {
            Self::Unresolved(s, _) => reader(s.as_str()),
            Self::Variable(v) => reader(v.upgrade().unwrap().name.as_str()),
            Self::Func(f) => reader(f.upgrade().unwrap().name.as_str()),
        }
    }

    pub fn get_type(&self) -> Option<AstType> {
        match self {
            Self::Unresolved(..) => None,
            Self::Variable(v) => Some(v.upgrade().unwrap().var_type.clone()),
            Self::Func(_) => None,
        }
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        self.read_name(|s| s.hash(state))
    }
}
