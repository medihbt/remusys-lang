use core::panic;
use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        expr::{
            Expr,
            binop::{Assign, BinExp},
            call::Call,
            ident::Ident,
            index::ArrayIndex,
            initlist::ArrayInitList,
            literal::Literal,
            unaryop::{ImplicitCast, UnaryExp},
        },
        operator::Operator,
    },
    typing::AstType,
    util::MultiLevelIndex,
};

use super::{
    AstNormalizer,
    scope::{Scope, ScopeSymbol},
};

pub struct ExprNormalizerNorecurse<'a> {
    pub scope_stack: &'a Vec<Scope>,
    pub should_eval: bool,
}

pub enum ExprStat<'a> {
    Enter(ExprStatEnter<'a>),
    Exit(ExprStatExit<'a>),

    RawInitList(RawInitListStat<'a>),
    ArrayIndex(ArrayIndexStat<'a>),
    BinOP(BinOpStat<'a>),
    UnaryOP(&'a UnaryExp),
    Call(CallStat<'a>),
    Assign(AssignStat<'a>),
}
pub struct RawInitListStat<'a> {
    expr: &'a Expr,
    array_init_list: ArrayInitList,
    final_index: usize,
}
pub struct ArrayIndexStat<'a> {
    expr: &'a Expr,
    array_index: ArrayIndex,
    index: usize,
    prev_should_eval: bool,
}
pub enum BinOpStat<'a> {
    ProcessLHS(&'a BinExp),
    ProcessRHS(&'a BinExp, Expr, AstType),
}
pub enum CallStat<'a> {
    ProcessCall(&'a Call),
    ProcessArg {
        call: &'a Call,
        form_args: Vec<(Expr, AstType)>,
    },
}
pub enum AssignStat<'a> {
    ProcessLHS(&'a Assign),
    ProcessRHS(&'a Assign, Expr, AstType),
}
pub struct ExprStatEnter<'a> {
    pub expr: &'a Expr,
}
pub struct ExprStatExit<'a> {
    pub expr: &'a Expr,
    pub normalized: Expr,
    pub ast_type: AstType,
}

impl<'a> ExprNormalizerNorecurse<'a> {
    pub fn new(scope_stack: &'a Vec<Scope>, should_eval: bool) -> Self {
        Self {
            scope_stack,
            should_eval,
        }
    }

    pub fn normalize_build_expr(&self, expr: &Expr) -> (Expr, AstType) {
        let mut expr_stack = vec![ExprStat::Enter(ExprStatEnter { expr })];

        while let Some(stat) = expr_stack.pop() {
            match stat {
                ExprStat::Enter(expr_stat_enter) => todo!(),
                ExprStat::Exit(expr_stat_exit) => todo!(),
                ExprStat::RawInitList(raw_init_list_stat) => todo!(),
                ExprStat::ArrayIndex(array_index_stat) => todo!(),
                ExprStat::BinOP(bin_op_stat) => todo!(),
                ExprStat::UnaryOP(unary_exp) => todo!(),
                ExprStat::Call(call_stat) => todo!(),
                ExprStat::Assign(assign_stat) => todo!(),
            }
        }
        ()
    }

    fn normalize_build_expr_enter<'b>(
        &self,
        expr_stk: &mut Vec<ExprStat<'b>>,
        curr_enter: ExprStatEnter<'b>,
    ) {
        match curr_enter.expr {
            Expr::None => expr_stk.push(ExprStat::Exit(ExprStatExit {
                expr: curr_enter.expr,
                normalized: Expr::None,
                ast_type: AstType::Void,
            })),
            Expr::Literal(literal) => expr_stk.push(ExprStat::Exit(ExprStatExit {
                expr: curr_enter.expr,
                normalized: Expr::Literal(literal.clone()),
                ast_type: match literal {
                    Literal::Int(_) => AstType::Int,
                    Literal::Float(_) => AstType::Float,
                },
            })),
            Expr::String(s) => expr_stk.push(ExprStat::Exit(ExprStatExit {
                expr: curr_enter.expr,
                normalized: Expr::String(s.clone()),
                ast_type: AstType::Str,
            })),
            Expr::RawInitList(initlist) => {}
            Expr::ArrayInitList(array_init_list) => todo!(),
            Expr::Ident(ident) => todo!(),
            Expr::ArrayIndex(array_index) => todo!(),
            Expr::BinOP(bin_exp) => todo!(),
            Expr::CmpOP(bin_exp) => todo!(),
            Expr::ShortCircuit(bin_exp) => todo!(),
            Expr::UnaryOP(unary_exp) => todo!(),
            Expr::ImplicitCast(implicit_cast) => todo!(),
            Expr::Call(call) => todo!(),
            Expr::Assign(assign) => todo!(),
            Expr::IntrinsicTimeStart(_) => todo!(),
            Expr::IntrinsicTimeEnd(_) => todo!(),
        }
    }

    fn normalize_build_expr_exit<'b>(
        &self,
        expr_stk: &mut Vec<ExprStat<'b>>,
        curr_exit: ExprStatExit<'b>,
    ) {
    }
}
