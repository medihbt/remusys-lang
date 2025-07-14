use core::panic;
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

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
};

use super::{
    AstNormalizer,
    scope::{Scope, ScopeSymbol},
};

pub struct ExprNormalizer<'a> {
    pub scope_stack: &'a Vec<Scope>,
    pub should_eval: bool,
    pub stack_usage: Cell<usize>,
    pub max_stack_usage: Cell<usize>,
}

impl<'a> ExprNormalizer<'a> {
    pub fn from_normalizer(normalizer: &'a AstNormalizer, should_eval: bool) -> Self {
        Self {
            scope_stack: &normalizer.scope,
            should_eval,
            stack_usage: Cell::new(0),
            max_stack_usage: Cell::new(0),
        }
    }

    pub fn normalize_build_expr(
        &mut self,
        expr: &Expr,
        type_request: Option<&AstType>,
    ) -> (Expr, AstType) {
        self.stack_usage.set(self.stack_usage.get() + 1);
        let ret = match expr {
            Expr::None => (Expr::None, type_request.cloned().unwrap_or(AstType::Void)),
            Expr::Literal(literal) => {
                let ty = match literal {
                    Literal::Int(_) => AstType::Int,
                    Literal::Float(_) => AstType::Float,
                };
                (expr.clone(), ty)
            }
            Expr::String(_) => (expr.clone(), AstType::Str),
            Expr::RawInitList(initlist) => {
                let ty = type_request.expect("type_request");
                let mut array_list = initlist.into_array(ty);
                let elemty = array_list.type_levels.last().unwrap().clone();
                for elem_ref in &mut array_list.final_elems {
                    let (elem, ty) = self.normalize_build_expr(elem_ref, Some(&elemty));
                    // 左值变右值
                    let elem = Self::operand_lvalue_to_rvalue(elem, &ty);
                    // 类型转换
                    let elem = Self::assign_type_cast(&elemty, &ty, elem);
                    *elem_ref = elem;
                }
                (Expr::ArrayInitList(array_list), ty.clone())
            }
            Expr::Ident(ident) => self.normalize_ident(ident),
            Expr::ArrayIndex(idx) => self.normalize_array_index(idx),
            Expr::BinOP(b) => self.normalize_binop(b),
            Expr::CmpOP(c) => self.normalize_binop(c),
            Expr::ShortCircuit(s) => self.normalize_binop(s),
            Expr::UnaryOP(u) => self.normalize_unary_op(u),
            Expr::Call(call) => self.normalize_call_expr(call),
            Expr::Assign(assign) => self.normalize_assign_expr(assign),

            Expr::ImplicitCast(_)
            | Expr::IntrinsicTimeStart(_)
            | Expr::IntrinsicTimeEnd(_)
            | Expr::ArrayInitList(_) => {
                panic!("It's strange that we have normalized expression in an un-normalized AST")
            }
        };
        self.max_stack_usage
            .set(self.max_stack_usage.get().max(self.stack_usage.get()));
        self.stack_usage.set(self.stack_usage.get() - 1);
        if self.stack_usage.get() == 0 {
            let max_usage = self.max_stack_usage.get();
            if max_usage > 1000 {
                println!("max stack usage: {}", self.max_stack_usage.get());
            }
        }
        ret
    }

    pub fn normalize_ident(&mut self, ident: &Ident) -> (Expr, AstType) {
        let (name, line) = if let Ident::Unresolved(name, line) = ident {
            (name, line.get())
        } else {
            panic!("expect unresolved ident")
        };

        let (mut expr, mut ty) = (Expr::None, AstType::Void);
        for layer in (0..self.scope_stack.len()).rev() {
            let scope = &self.scope_stack[layer];
            let symbol = match scope.get_symbol(name) {
                Some(symbol) => symbol,
                None => {
                    if layer == 0 {
                        panic!("cannot find symbol `{}` (line {}) in scope", name, line);
                    }
                    continue;
                }
            };
            (expr, ty) = match symbol {
                ScopeSymbol::Const(value, ty, var) => {
                    if self.should_eval {
                        (value.clone(), ty.clone())
                    } else {
                        (Expr::Ident(Ident::Variable(Rc::downgrade(var))), ty.clone())
                    }
                }
                ScopeSymbol::Var(var) => {
                    if self.should_eval {
                        panic!("cannot eval variable `{}` (line {}) in scope", name, line);
                    }
                    (
                        Expr::Ident(Ident::Variable(Rc::downgrade(var))),
                        var.var_type.clone(),
                    )
                }
                ScopeSymbol::Func(func) => {
                    if self.should_eval {
                        panic!("cannot eval function `{}` (line {}) in scope", name, line);
                    }
                    (
                        Expr::Ident(Ident::Func(Rc::downgrade(func))),
                        func.ret_type.clone(),
                    )
                }
            };
            break;
        }
        (expr, ty)
    }

    fn normalize_call_name_ident(&mut self, call_name: &Ident) -> (Ident, usize) {
        let (name, line) = if let Ident::Unresolved(name, line) = call_name {
            (name, line.get())
        } else {
            panic!("expect unresolved ident")
        };
        let global_scope = self.scope_stack.first().expect("global scope");
        let symbol = match global_scope.get_symbol(name) {
            Some(ScopeSymbol::Func(f)) => f,
            _ => panic!("cannot find function `{}` (line {}) in scope", name, line),
        };
        (Ident::Func(Rc::downgrade(symbol)), line)
    }

    pub fn normalize_array_index(&mut self, arr_index: &ArrayIndex) -> (Expr, AstType) {
        let (indexee, indexee_ty) = self.normalize_ident(&arr_index.indexee);
        let mut indices = Vec::with_capacity(arr_index.indices.len());
        for i in arr_index.indices.iter() {
            let (index, index_ty) = self.normalize_build_expr(i, Some(&AstType::Int));
            if index_ty != AstType::Int {
                panic!("array index must be int");
            }
            let index = Self::operand_lvalue_to_rvalue(index, &index_ty);
            if self.should_eval && !matches!(index, Expr::Literal(Literal::Int(_))) {
                panic!(
                    "cannot eval array index: requires int literal but got {:?}",
                    index
                );
            }
            indices.push(index);
        }

        let array_elem_chain = indexee_ty.dump_element_chain();
        if !self.should_eval {
            if array_elem_chain.len() <= indices.len() {
                panic!(
                    "array index length too long: expected > {} but got {}",
                    array_elem_chain.len() - 1,
                    indices.len()
                );
            }
            let var = match indexee {
                Expr::Ident(Ident::Variable(var)) => var,
                _ => panic!("expect variable"),
            };
            let index_top_level = indices.len();
            return (
                Expr::ArrayIndex(Box::new(ArrayIndex {
                    indexee: Ident::Variable(var),
                    indices,
                    vtype: RefCell::new(indexee_ty),
                })),
                array_elem_chain[index_top_level].clone(),
            );
        }
        if array_elem_chain.len() != indices.len() + 1 {
            panic!(
                "array index length mismatch: expected {} but got {}",
                array_elem_chain.len() - 1,
                indices.len()
            );
        }

        let arr_indexee = match indexee {
            Expr::ArrayInitList(arr) => arr,
            _ => panic!("expect array init list"),
        };
        (
            Self::get_array_elem_from_index(&indices, &arr_indexee),
            array_elem_chain.last().unwrap().clone(),
        )
    }

    fn get_array_elem_from_index(indices: &[Expr], array_list: &ArrayInitList) -> Expr {
        let dimensions = array_list.dimensions.as_ref();
        let mut final_index = 0;
        for (level, index) in indices.iter().enumerate() {
            let int_index = match index {
                Expr::Literal(Literal::Int(i)) => *i as usize,
                _ => panic!("expect int literal for array index"),
            };
            if int_index >= dimensions[level] {
                panic!(
                    "array index out of bounds: {} >= {}",
                    int_index, dimensions[level]
                );
            }
            final_index += int_index * array_list.n_final_elems[level + 1];
        }
        if final_index >= array_list.final_elems.len() {
            panic!("array index out of bounds: {}", final_index);
        }
        array_list.final_elems[final_index].clone()
    }

    fn normalize_binop(&mut self, binop: &BinExp) -> (Expr, AstType) {
        let (lhs, lhs_ty) = self.normalize_build_expr(&binop.lhs, None);
        let (rhs, rhs_ty) = self.normalize_build_expr(&binop.rhs, None);

        if self.should_eval {
            if !matches!(lhs, Expr::Literal(_)) || !matches!(rhs, Expr::Literal(_)) {
                panic!("expect literal");
            }
            let (lit, ty) = self.eval_binop(binop.op, &lhs, &rhs, &lhs_ty, &rhs_ty);
            return (Expr::Literal(lit), ty);
        }

        // 左右值归一化: 如果操作数是左值, 就添加一个 `LValueToRValue` 的转换
        let lhs = Self::operand_lvalue_to_rvalue(lhs, &lhs_ty);
        let rhs = Self::operand_lvalue_to_rvalue(rhs, &rhs_ty);

        // 操作数类型归一化
        match binop.op {
            Operator::Add | Operator::Sub | Operator::Mul | Operator::Div | Operator::Mod => {
                let (l, r, t) =
                    Self::binary_alu_operand_add_cast(binop.op, lhs, rhs, &lhs_ty, &rhs_ty);
                (
                    Expr::BinOP(Box::new(BinExp {
                        op: binop.op,
                        lhs: l,
                        rhs: r,
                    })),
                    t,
                )
            }
            Operator::Eq
            | Operator::Ne
            | Operator::Gt
            | Operator::Ge
            | Operator::Lt
            | Operator::Le => {
                let (l, r, t) =
                    Self::binary_cmp_operand_add_cast(binop.op, lhs, rhs, &lhs_ty, &rhs_ty);
                (
                    Expr::CmpOP(Box::new(BinExp {
                        op: binop.op,
                        lhs: l,
                        rhs: r,
                    })),
                    t,
                )
            }

            Operator::LogicalAnd | Operator::LogicalOr => {
                let (l, r, t) =
                    Self::binary_short_circuit_operand_add_cast(lhs, rhs, &lhs_ty, &rhs_ty);
                (
                    Expr::ShortCircuit(Box::new(BinExp {
                        op: binop.op,
                        lhs: l,
                        rhs: r,
                    })),
                    t,
                )
            }
            _ => panic!("unsupported binary operator: {:?}", binop.op),
        }
    }
    fn eval_binop(
        &mut self,
        operator: Operator,
        lhs: &Expr,
        rhs: &Expr,
        lhs_ty: &AstType,
        rhs_ty: &AstType,
    ) -> (Literal, AstType) {
        let lhs = match lhs {
            Expr::Literal(l) => l,
            _ => panic!("expect literal"),
        };
        let rhs = match rhs {
            Expr::Literal(r) => r,
            _ => panic!("expect literal"),
        };

        match operator {
            Operator::Add => lhs.add(rhs),
            Operator::Sub => lhs.sub(rhs),
            Operator::Mul => lhs.mul(rhs),
            Operator::Div => lhs.div(rhs),
            Operator::Mod => lhs.mod_op(rhs),
            Operator::Eq => (Literal::Int(lhs.value_equals(rhs) as i32), AstType::Bool),
            Operator::Ne => (Literal::Int((!lhs.value_equals(rhs)) as i32), AstType::Bool),
            Operator::Gt => (
                Literal::Int((lhs.value_cmp(rhs) == std::cmp::Ordering::Greater) as i32),
                AstType::Bool,
            ),
            Operator::Ge => {
                let cmp = lhs.value_cmp(rhs);
                match cmp {
                    std::cmp::Ordering::Less => (Literal::Int(0), AstType::Bool),
                    std::cmp::Ordering::Greater => (Literal::Int(1), AstType::Bool),
                    std::cmp::Ordering::Equal => (Literal::Int(1), AstType::Bool),
                }
            }
            Operator::Lt => (
                Literal::Int((lhs.value_cmp(rhs) == std::cmp::Ordering::Less) as i32),
                AstType::Bool,
            ),
            Operator::Le => {
                let cmp = lhs.value_cmp(rhs);
                match cmp {
                    std::cmp::Ordering::Less => (Literal::Int(1), AstType::Bool),
                    std::cmp::Ordering::Greater => (Literal::Int(0), AstType::Bool),
                    std::cmp::Ordering::Equal => (Literal::Int(1), AstType::Bool),
                }
            }
            Operator::LogicalAnd => {
                assert_eq!(*lhs_ty, AstType::Bool);
                assert_eq!(*rhs_ty, AstType::Bool);
                match (lhs, rhs) {
                    (Literal::Int(0), _) | (_, Literal::Int(0)) => (Literal::Int(0), AstType::Bool),
                    _ => (Literal::Int(1), AstType::Bool),
                }
            }
            Operator::LogicalOr => {
                assert_eq!(*lhs_ty, AstType::Bool);
                assert_eq!(*rhs_ty, AstType::Bool);
                match (lhs, rhs) {
                    (Literal::Int(0), Literal::Int(0)) => (Literal::Int(0), AstType::Bool),
                    _ => (Literal::Int(1), AstType::Bool),
                }
            }
            Operator::LogicalNot => {
                assert_eq!(*lhs_ty, AstType::Bool);
                assert_eq!(*rhs_ty, AstType::Bool);
                match (lhs, rhs) {
                    (Literal::Int(0), _) => (Literal::Int(1), AstType::Bool),
                    (_, Literal::Int(0)) => (Literal::Int(1), AstType::Bool),
                    _ => (Literal::Int(0), AstType::Bool),
                }
            }
            _ => panic!("unsupported operator: {:?}", operator),
        }
    }

    pub(super) fn operand_lvalue_to_rvalue(operand: Expr, operand_ty: &AstType) -> Expr {
        match &operand {
            Expr::Ident(_) | Expr::ArrayIndex(_) => Expr::ImplicitCast(Box::new(ImplicitCast {
                kind: Operator::LValueToRValue,
                expr: operand,
                target: operand_ty.clone(),
            })),
            _ => operand,
        }
    }

    /// BinOP 操作数类型归一化
    ///
    /// ### 规则
    ///
    /// - `int & float`, `float & int` -> `itof` cast `int` to float
    /// - `* & bool`, `bool & *` -> panic
    /// - else -> no cast
    fn binary_alu_operand_add_cast(
        operator: Operator,
        lhs: Expr,
        rhs: Expr,
        lhs_ty: &AstType,
        rhs_ty: &AstType,
    ) -> (Expr, Expr, AstType) {
        if lhs_ty == rhs_ty {
            return (lhs, rhs, lhs_ty.clone());
        }
        match (lhs_ty, rhs_ty) {
            (AstType::Int, AstType::Float) => {
                let lhs = if let Expr::Literal(Literal::Int(i)) = lhs {
                    Expr::Literal(Literal::Float(i as f32))
                } else {
                    Expr::ImplicitCast(Box::new(ImplicitCast {
                        kind: Operator::ItoF,
                        expr: lhs,
                        target: AstType::Float,
                    }))
                };
                (lhs, rhs, AstType::Float)
            }
            (AstType::Float, AstType::Int) => {
                let rhs = if let Expr::Literal(Literal::Int(i)) = rhs {
                    Expr::Literal(Literal::Float(i as f32))
                } else {
                    Expr::ImplicitCast(Box::new(ImplicitCast {
                        kind: Operator::ItoF,
                        expr: rhs,
                        target: AstType::Float,
                    }))
                };
                (lhs, rhs, AstType::Float)
            }
            (AstType::Bool, AstType::Int) => {
                let lhs = if let Expr::Literal(Literal::Int(_)) = lhs {
                    lhs
                } else {
                    Expr::ImplicitCast(Box::new(ImplicitCast {
                        kind: Operator::BoolToInt,
                        expr: lhs,
                        target: AstType::Int,
                    }))
                };
                (lhs, rhs, AstType::Int)
            }
            (AstType::Int, AstType::Bool) => {
                let rhs = if let Expr::Literal(Literal::Int(_)) = rhs {
                    rhs
                } else {
                    Expr::ImplicitCast(Box::new(ImplicitCast {
                        kind: Operator::BoolToInt,
                        expr: rhs,
                        target: AstType::Int,
                    }))
                };
                (lhs, rhs, AstType::Int)
            }
            (AstType::Bool, AstType::Float) => {
                let lhs = if let Expr::Literal(Literal::Int(i)) = lhs {
                    Expr::Literal(Literal::Float(i as f32))
                } else {
                    Expr::ImplicitCast(Box::new(ImplicitCast {
                        kind: Operator::BoolToFloat,
                        expr: lhs,
                        target: AstType::Float,
                    }))
                };
                (lhs, rhs, AstType::Float)
            }
            (AstType::Float, AstType::Bool) => {
                let rhs = if let Expr::Literal(Literal::Int(i)) = rhs {
                    Expr::Literal(Literal::Float(i as f32))
                } else {
                    Expr::ImplicitCast(Box::new(ImplicitCast {
                        kind: Operator::BoolToFloat,
                        expr: rhs,
                        target: AstType::Float,
                    }))
                };
                (lhs, rhs, AstType::Float)
            }
            _ => panic!(
                "unsupported operand types on operator {:?}: {:?} and {:?}",
                operator, lhs_ty, rhs_ty
            ),
        }
    }

    fn binary_cmp_operand_add_cast(
        operator: Operator,
        lhs: Expr,
        rhs: Expr,
        lhs_ty: &AstType,
        rhs_ty: &AstType,
    ) -> (Expr, Expr, AstType) {
        let (lhs, rhs, _) = Self::binary_alu_operand_add_cast(operator, lhs, rhs, lhs_ty, rhs_ty);
        (lhs, rhs, AstType::Bool)
    }

    fn binary_short_circuit_operand_add_cast(
        lhs: Expr,
        rhs: Expr,
        lhs_ty: &AstType,
        rhs_ty: &AstType,
    ) -> (Expr, Expr, AstType) {
        let lhs = Self::operand_cast_to_bool(lhs, lhs_ty);
        let rhs = Self::operand_cast_to_bool(rhs, rhs_ty);
        (lhs, rhs, AstType::Bool)
    }
    pub fn operand_cast_to_bool(operand: Expr, operand_ty: &AstType) -> Expr {
        match operand_ty {
            AstType::Bool => operand,
            AstType::Int => {
                if let Expr::Literal(Literal::Int(i)) = operand {
                    Expr::Literal(Literal::Int(if i == 0 { 0 } else { 1 }))
                } else {
                    Expr::CmpOP(Box::new(BinExp {
                        op: Operator::Ne,
                        lhs: operand,
                        rhs: Expr::Literal(Literal::Int(0)),
                    }))
                }
            }
            AstType::Float => {
                if let Expr::Literal(Literal::Float(f)) = operand {
                    Expr::Literal(Literal::Int(if f == 0.0 { 0 } else { 1 }))
                } else {
                    Expr::CmpOP(Box::new(BinExp {
                        op: Operator::Ne,
                        lhs: operand,
                        rhs: Expr::Literal(Literal::Float(0.0)),
                    }))
                }
            }
            _ => panic!(
                "unsupported operand type for boolean conversion: {:?}",
                operand_ty
            ),
        }
    }

    pub fn operand_cast_to_value(operand: Expr, operand_ty: &AstType) -> (Expr, AstType) {
        match operand_ty {
            AstType::Int | AstType::Float => (operand, operand_ty.clone()),
            AstType::Bool => {
                let operand = Expr::ImplicitCast(Box::new(ImplicitCast {
                    kind: Operator::BoolToInt,
                    expr: operand,
                    target: AstType::Int,
                }));
                (operand, AstType::Int)
            }
            _ => panic!(
                "unsupported operand type for value conversion: {:?}",
                operand_ty
            ),
        }
    }

    fn normalize_unary_op(&mut self, unary: &UnaryExp) -> (Expr, AstType) {
        let (op, expr) = (unary.op, &unary.expr);
        let (expr, ty) = self.normalize_build_expr(&expr, None);

        // 先尝试直接计算结果
        if let Some((expr, ty)) = Self::eval_unary_op(op, &expr, ty.clone()) {
            return (expr, ty);
        } else if self.should_eval {
            panic!("cannot eval unary operator: {:?}", op);
        }
        // 左值转换
        let expr = Self::operand_lvalue_to_rvalue(expr, &ty);

        match op {
            Operator::LogicalNot => {
                let operand = Self::operand_cast_to_bool(expr, &ty);
                if let Expr::CmpOP(mut cmp) = operand {
                    cmp.op = match cmp.op {
                        Operator::Eq => Operator::Ne,
                        Operator::Ne => Operator::Eq,
                        Operator::Gt => Operator::Le,
                        Operator::Ge => Operator::Lt,
                        Operator::Lt => Operator::Ge,
                        Operator::Le => Operator::Gt,
                        _ => panic!("unsupported operator for logical not: {:?}", cmp.op),
                    };
                    (Expr::CmpOP(cmp), AstType::Bool)
                } else {
                    (operand, AstType::Bool)
                }
            }
            Operator::Add | Operator::Positive | Operator::Sub | Operator::Neg => {
                let (expr, ty) = Self::operand_cast_to_value(expr, &ty);
                (Expr::UnaryOP(Box::new(UnaryExp { op, expr })), ty)
            }
            _ => panic!("unsupported unary operator: {:?}", op),
        }
    }
    fn eval_unary_op(
        operator: Operator,
        operand: &Expr,
        operand_ty: AstType,
    ) -> Option<(Expr, AstType)> {
        let operand = match operand {
            Expr::Literal(l) => l,
            _ => return None,
        };
        let ret = match operator {
            Operator::LogicalNot => match operand {
                Literal::Int(0) => (Expr::Literal(Literal::Int(1)), AstType::Bool),
                _ => (Expr::Literal(Literal::Int(0)), AstType::Bool),
            },
            Operator::Add | Operator::Positive => (Expr::Literal(operand.clone()), operand_ty),
            Operator::Sub | Operator::Neg => (Expr::Literal(operand.neg().0), operand_ty),
            _ => panic!("unsupported unary operator: {:?}", operator),
        };
        Some(ret)
    }
    fn normalize_call_expr(&mut self, call: &Call) -> (Expr, AstType) {
        if self.should_eval {
            panic!("cannot eval function call");
        }
        let (func, ident_line) = self.normalize_call_name_ident(&call.name);
        let func = match func {
            Ident::Func(func) => func.upgrade().expect("function should be valid"),
            _ => panic!("expect function"),
        };

        match func.intrinsic_get_id() {
            Some(id) => match id.as_str() {
                "starttime" => return (Expr::IntrinsicTimeStart(ident_line), AstType::Void),
                "endtime" => return (Expr::IntrinsicTimeEnd(ident_line), AstType::Void),
                _ => panic!("unsupported intrinsic function: {}", id),
            },
            None => {
                let mut args = Vec::with_capacity(call.args.len());
                for (index, arg) in call.args.iter().enumerate() {
                    let (arg, arg_ty) = self.normalize_build_expr(arg, None);
                    let arg = match arg_ty {
                        AstType::Int | AstType::Float | AstType::Str => {
                            Self::operand_lvalue_to_rvalue(arg, &arg_ty)
                        }
                        _ => arg,
                    };
                    let arg = match func.get_arg_type(index) {
                        Some(funcarg_ty) => {
                            Self::call_arg_type_cast(funcarg_ty, &arg_ty, arg, ident_line)
                        }
                        None => {
                            if !func.is_vararg {
                                panic!(
                                    "Error at line {}: function `{}` cannot accept more than {} arguments",
                                    ident_line,
                                    func.name,
                                    func.resolved_args.len()
                                );
                            }
                            arg
                        }
                    };
                    args.push(arg);
                }
                (
                    Expr::Call(Box::new(Call {
                        name: Ident::Func(Rc::downgrade(&func)),
                        args,
                    })),
                    func.ret_type.clone(),
                )
            }
        }
    }

    fn call_arg_type_cast(
        required: &AstType,
        operand_ty: &AstType,
        operand: Expr,
        ident_line: usize,
    ) -> Expr {
        if required == operand_ty {
            return operand;
        }
        match (required, operand_ty) {
            (AstType::Int, AstType::Float) => {
                if let Expr::Literal(Literal::Float(f)) = operand {
                    return Expr::Literal(Literal::Int(f as i32));
                }
                Expr::ImplicitCast(Box::new(ImplicitCast {
                    kind: Operator::FtoI,
                    expr: operand,
                    target: AstType::Float,
                }))
            }
            (AstType::Float, AstType::Int) => {
                if let Expr::Literal(Literal::Int(i)) = operand {
                    return Expr::Literal(Literal::Float(i as f32));
                }
                Expr::ImplicitCast(Box::new(ImplicitCast {
                    kind: Operator::ItoF,
                    expr: operand,
                    target: AstType::Float,
                }))
            }
            // 若函数参数是数组类型则会被 decay 成对应元素的指针, 此时长度失效.
            // Remusys 为了简单没有实现指针类型, 因此直接在数组类型上做判断.
            (AstType::FixedArray(reqarr), AstType::DynArray(dynarr)) => {
                assert_eq!(&reqarr.elemty, dynarr.as_ref());
                operand
            }
            (AstType::DynArray(reqarr), AstType::FixedArray(fixedarr)) => {
                assert_eq!(reqarr.as_ref(), &fixedarr.elemty);
                operand
            }
            (AstType::DynArray(reqarr), AstType::DynArray(dynarr)) => {
                assert_eq!(reqarr.as_ref(), dynarr.as_ref());
                operand
            }
            (AstType::FixedArray(reqarr), AstType::FixedArray(fixedarr)) => {
                assert_eq!(&reqarr.elemty, &fixedarr.elemty);
                operand
            }
            _ => panic!(
                "unsupported function argument type cast at line {}: {:?} to {:?}",
                ident_line, operand_ty, required
            ),
        }
    }

    fn normalize_assign_expr(&mut self, assign: &Assign) -> (Expr, AstType) {
        if self.should_eval {
            panic!("cannot eval assign");
        }

        let (lhs, lhs_ty) = self.normalize_build_expr(&assign.lhs, None);
        let (rhs, rhs_ty) = self.normalize_build_expr(&assign.rhs, None);

        // 左右值匹配: 左侧是左值, 右侧是右值
        if lhs.is_rvalue() {
            panic!("expect lvalue, but got rvalue {:?}", lhs);
        }
        let rhs = Self::operand_lvalue_to_rvalue(rhs, &rhs_ty);

        // 类型匹配: 把右侧的类型对齐到左侧
        let rhs = Self::assign_type_cast(&lhs_ty, &rhs_ty, rhs);

        (Expr::Assign(Box::new(Assign { lhs, rhs })), AstType::Void)
    }
    pub(super) fn assign_type_cast(lhs_ty: &AstType, rhs_ty: &AstType, rhs: Expr) -> Expr {
        if lhs_ty == rhs_ty {
            return rhs;
        }
        match (lhs_ty, rhs_ty) {
            (AstType::Int, AstType::Float) => {
                if let Expr::Literal(Literal::Float(f)) = rhs {
                    return Expr::Literal(Literal::Int(f as i32));
                }
                Expr::ImplicitCast(Box::new(ImplicitCast {
                    kind: Operator::FtoI,
                    expr: rhs,
                    target: AstType::Int,
                }))
            }
            (AstType::Float, AstType::Int) => {
                if let Expr::Literal(Literal::Int(i)) = rhs {
                    return Expr::Literal(Literal::Float(i as f32));
                }
                Expr::ImplicitCast(Box::new(ImplicitCast {
                    kind: Operator::ItoF,
                    expr: rhs,
                    target: AstType::Float,
                }))
            }
            _ => panic!("unsupported assign type cast"),
        }
    }
}
