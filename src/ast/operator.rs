#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    Shl,
    Shr,

    Neg,

    LogicalAnd,
    LogicalOr,
    LogicalNot,

    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,

    Assign,
}

impl Operator {
    pub fn to_str(&self) -> &'static str {
        match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Mod => "%",
            Operator::BitAnd => "&",
            Operator::BitOr => "|",
            Operator::BitXor => "^",
            Operator::BitNot => "~",
            Operator::Shl => "<<",
            Operator::Shr => ">>",

            Operator::Neg => "-",

            Operator::LogicalAnd => "&&",
            Operator::LogicalOr => "||",
            Operator::LogicalNot => "!",

            Operator::Eq => "==",
            Operator::Ne => "!=",
            Operator::Gt => ">",
            Operator::Ge => ">=",
            Operator::Lt => "<",
            Operator::Le => "<=",

            Operator::Assign => "=",
        }
    }

    pub const fn is_binop(self) -> bool {
        matches!(
            self,
            Operator::Add
                | Operator::Sub
                | Operator::Mul
                | Operator::Div
                | Operator::Mod
                | Operator::BitAnd
                | Operator::BitOr
                | Operator::BitXor
                | Operator::Shl
                | Operator::Shr
                | Operator::Eq
                | Operator::Ne
                | Operator::Gt
                | Operator::Ge
                | Operator::Lt
                | Operator::Le
        )
    }
    pub const fn is_unop(self) -> bool {
        matches!(
            self,
            Operator::BitNot
                | Operator::LogicalNot
                | Operator::LogicalAnd
                | Operator::LogicalOr
        )
    }
    pub const fn has_control_flow(self) -> bool {
        matches!(
            self,
            Operator::LogicalAnd | Operator::LogicalOr | Operator::LogicalNot
        )
    }
}
