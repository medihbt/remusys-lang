use std::{
    cell::Cell, hash::{Hash, Hasher}, rc::Weak
};

use crate::{
    ast::stmt::decl::{Function, Variable},
    typing::AstType,
};

#[derive(Debug, Clone)]
pub enum Ident {
    Unresolved(String, Cell<usize> /* line number */),
    Variable(Weak<Variable>),
    Func(Weak<Function>),
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
