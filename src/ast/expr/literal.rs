use std::hash::Hash;

#[derive(Debug, Clone, Copy)]
pub enum Literal {
    Int(i32),
    Float(f32),
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
