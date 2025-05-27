use std::{fmt::Debug, hash::Hash};

use crate::typing::AstType;

#[derive(Clone, Copy, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f32),
}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "Int({})", i),
            Literal::Float(fl) => write!(f, "Float({})", fl),
        }
    }
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Int(i) => format!("Literal::Int({})", i),
            Literal::Float(fl) => format!("Literal::Float({})", fl),
        }
    }
}

impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Literal::Int(i) => i.hash(state),
            Literal::Float(f) => f.to_bits().hash(state),
        }
    }
}

impl Literal {
    pub fn value_equals(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::Int(i1), Literal::Int(i2)) => i1 == i2,
            (Literal::Float(f1), Literal::Float(f2)) => f1 == f2,
            (Literal::Int(i), Literal::Float(f)) => (*i as f32) == *f,
            (Literal::Float(f), Literal::Int(i)) => *f == (*i as f32),
        }
    }
    pub fn value_cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Literal::Int(i1), Literal::Int(i2)) => i1.cmp(i2),
            (Literal::Float(f1), Literal::Float(f2)) => f1.partial_cmp(f2).unwrap(),
            (Literal::Int(i), Literal::Float(f)) => (*i as f32).partial_cmp(f).unwrap(),
            (Literal::Float(f), Literal::Int(i)) => f.partial_cmp(&(*i as f32)).unwrap(),
        }
    }

    pub fn add(&self, other: &Self) -> (Self, AstType) {
        match (self, other) {
            (Literal::Int(i1), Literal::Int(i2)) => {
                (Literal::Int(i1 + i2), AstType::Int)
            }
            (Literal::Float(f1), Literal::Float(f2)) => {
                (Literal::Float(f1 + f2), AstType::Float)
            }
            (Literal::Int(i), Literal::Float(f)) => {
                (Literal::Float((*i as f32) + f), AstType::Float)
            }
            (Literal::Float(f), Literal::Int(i)) => {
                (Literal::Float(f + (*i as f32)), AstType::Float)
            }
        }
    }
    pub fn sub(&self, other: &Self) -> (Self, AstType) {
        match (self, other) {
            (Literal::Int(i1), Literal::Int(i2)) => {
                (Literal::Int(i1 - i2), AstType::Int)
            }
            (Literal::Float(f1), Literal::Float(f2)) => {
                (Literal::Float(f1 - f2), AstType::Float)
            }
            (Literal::Int(i), Literal::Float(f)) => {
                (Literal::Float((*i as f32) - f), AstType::Float)
            }
            (Literal::Float(f), Literal::Int(i)) => {
                (Literal::Float(f - (*i as f32)), AstType::Float)
            }
        }
    }
    pub fn mul(&self, other: &Self) -> (Self, AstType) {
        match (self, other) {
            (Literal::Int(i1), Literal::Int(i2)) => {
                (Literal::Int(i1 * i2), AstType::Int)
            }
            (Literal::Float(f1), Literal::Float(f2)) => {
                (Literal::Float(f1 * f2), AstType::Float)
            }
            (Literal::Int(i), Literal::Float(f)) => {
                (Literal::Float((*i as f32) * f), AstType::Float)
            }
            (Literal::Float(f), Literal::Int(i)) => {
                (Literal::Float(f * (*i as f32)), AstType::Float)
            }
        }
    }
    pub fn div(&self, other: &Self) -> (Self, AstType) {
        if let Literal::Int(0) = other {
            panic!("Division by zero");
        }
        match (self, other) {
            (Literal::Int(i1), Literal::Int(i2)) => {
                (Literal::Int(i1 / i2), AstType::Int)
            }
            (Literal::Float(f1), Literal::Float(f2)) => {
                (Literal::Float(f1 / f2), AstType::Float)
            }
            (Literal::Int(i), Literal::Float(f)) => {
                (Literal::Float((*i as f32) / f), AstType::Float)
            }
            (Literal::Float(f), Literal::Int(i)) => {
                (Literal::Float(f / (*i as f32)), AstType::Float)
            }
        }
    }
    pub fn mod_op(&self, other: &Self) -> (Self, AstType) {
        if let Literal::Int(0) = other {
            panic!("Division by zero");
        }
        match (self, other) {
            (Literal::Int(i1), Literal::Int(i2)) => {
                (Literal::Int(i1 % i2), AstType::Int)
            }
            (Literal::Float(f1), Literal::Float(f2)) => {
                (Literal::Float(f1 % f2), AstType::Float)
            }
            (Literal::Int(i), Literal::Float(f)) => {
                (Literal::Float((*i as f32) % f), AstType::Float)
            }
            (Literal::Float(f), Literal::Int(i)) => {
                (Literal::Float(f % (*i as f32)), AstType::Float)
            }
        }
    }
    pub fn neg(&self) -> (Self, AstType) {
        match self {
            Literal::Int(i) => (Literal::Int(-i), AstType::Int),
            Literal::Float(f) => (Literal::Float(-f), AstType::Float),
        }
    }
}
