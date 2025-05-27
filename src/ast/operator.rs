#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

    Positive,
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

    ItoF,
    FtoI,
    BoolToInt,
    BoolToFloat,
    LValueToRValue,
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

            Operator::Positive => "+",
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

            Operator::ItoF => "itof",
            Operator::FtoI => "ftoi",
            Operator::BoolToInt => "bool_to_int",
            Operator::BoolToFloat => "bool_to_float",
            Operator::LValueToRValue => "LValueToRValue",
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
    pub const fn is_cmp(&self) -> bool {
        matches!(
            self,
            Operator::Eq | Operator::Ne | Operator::Gt | Operator::Ge | Operator::Lt | Operator::Le
        )
    }
    pub const fn is_short_circuit(self) -> bool {
        matches!(self, Operator::LogicalAnd | Operator::LogicalOr)
    }
    pub const fn is_unop(self) -> bool {
        matches!(
            self,
            Operator::BitNot | Operator::LogicalNot | Operator::Positive | Operator::Neg
        )
    }
    pub const fn has_control_flow(self) -> bool {
        matches!(
            self,
            Operator::LogicalAnd | Operator::LogicalOr | Operator::LogicalNot
        )
    }
    pub const fn is_cast_op(self) -> bool {
        matches!(
            self,
            Operator::ItoF
                | Operator::FtoI
                | Operator::BoolToInt
                | Operator::BoolToFloat
                | Operator::LValueToRValue
        )
    }
}
