//! Normalize an expression.
//! 
//! If the expression is a compile-time constant expression, it will be evaluated.
//! If the expression is an unresolved identifier, it will be resolved.
//! If the expression is a initializer list, it will be resolved to an array initializer.
//! 
//! If the operand is a lvalue while the position requires an rvalue, it will add `LValueToRValue` cast.

use super::Expr;

pub struct ExprNormalizer {
    pub to_normalize: Expr,
}