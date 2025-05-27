use crate::{
    ast::{
        AstModule,
        expr::{Expr, ident::Ident},
        stmt::*,
    },
    grammar,
};

pub fn parse_sysy_file(input_file: &str) -> AstModule {
    let content = std::fs::read_to_string(input_file)
        .unwrap_or_else(|_| panic!("Failed to read file: {}", input_file));
    let mut module = grammar::AstModuleParser::new()
        .parse(&content)
        .expect("Failed to parse SysY file");
    module.file = input_file.to_string();
    fix_module_ident_line(&mut module, &content);
    module
}

pub fn fix_module_ident_line(module: &mut AstModule, input: &str) {
    let line_map = LineMap::new(input);
    for gdef in &module.global_defs {
        fix_module_ident_line_in_stmt(&line_map, gdef);
    }
}

fn fix_module_ident_line_in_stmt(line_map: &LineMap, stmt: &Stmt) {
    let mut stmt_stack = Vec::new();
    stmt_stack.push(stmt);
    while let Some(stmt) = stmt_stack.pop() {
        match stmt {
            Stmt::None | Stmt::Break | Stmt::Continue | Stmt::BreakTo(_) | Stmt::ContinueTo(_) => {}
            Stmt::Block(block) => {
                for inner_stmt in block.stmts.iter() {
                    stmt_stack.push(inner_stmt);
                }
            }
            Stmt::UnresolvedVarDecl(uvar) => {
                for uvardef in uvar.defs.iter() {
                    for arrdim in uvardef.array_dims.iter() {
                        fix_module_ident_line_in_expr(line_map, arrdim);
                    }
                    fix_module_ident_line_in_expr(line_map, &uvardef.initval);
                }
            }
            Stmt::VarDecl(var) => {
                for vardef in var.defs.iter() {
                    fix_module_ident_line_in_expr(line_map, &vardef.initval);
                }
            }
            Stmt::FuncDecl(func) => {
                for uvardef in func.unresolved_args.iter() {
                    for arrdim in uvardef.array_dims.iter() {
                        fix_module_ident_line_in_expr(line_map, arrdim);
                    }
                    fix_module_ident_line_in_expr(line_map, &uvardef.initval);
                }
                for vardef in func.resolved_args.iter() {
                    fix_module_ident_line_in_expr(line_map, &vardef.initval);
                }
                match &*func.body.borrow() {
                    Some(block) => {
                        for inner_stmt in block.stmts.iter() {
                            fix_module_ident_line_in_stmt(line_map, inner_stmt);
                        }
                    }
                    None => {}
                }
            }
            Stmt::If(if_stmt) => {
                fix_module_ident_line_in_expr(line_map, &if_stmt.cond);
                stmt_stack.push(&if_stmt.then_stmt);
                if let Some(else_stmt) = &if_stmt.else_stmt {
                    stmt_stack.push(else_stmt);
                }
            }
            Stmt::While(while_stmt) => {
                fix_module_ident_line_in_expr(line_map, &while_stmt.cond);
                fix_module_ident_line_in_stmt(line_map, &while_stmt.body.borrow());
            }
            Stmt::ExprStmt(expr) => {
                fix_module_ident_line_in_expr(line_map, &*expr.expr.borrow());
            }
            Stmt::Return(expr) => {
                fix_module_ident_line_in_expr(line_map, expr);
            }
        }
    }
}

fn fix_module_ident_line_in_expr(line_map: &LineMap, expr: &Expr) {
    let mut expr_stack = Vec::new();
    expr_stack.push(expr);
    while let Some(expr) = expr_stack.pop() {
        match expr {
            Expr::None
            | Expr::Literal(_)
            | Expr::String(_)
            | Expr::IntrinsicTimeStart(_)
            | Expr::IntrinsicTimeEnd(_) => {}

            Expr::RawInitList(initlist) => {
                for exp in initlist.items.iter() {
                    expr_stack.push(exp);
                }
            }
            Expr::ArrayInitList(arrlist) => {
                for exp in arrlist.final_elems.iter() {
                    expr_stack.push(exp);
                }
            }
            Expr::Ident(ident) => match ident {
                Ident::Unresolved(_, pos) => pos.set(line_map.get_line(pos.get())),
                _ => {}
            },
            Expr::ArrayIndex(arridx) => {
                for idx in arridx.indices.iter() {
                    fix_module_ident_line_in_expr(line_map, idx);
                }
            }
            Expr::BinOP(bin_exp) | Expr::CmpOP(bin_exp) | Expr::ShortCircuit(bin_exp) => {
                expr_stack.push(&bin_exp.lhs);
                expr_stack.push(&bin_exp.rhs);
            }
            Expr::UnaryOP(unary_exp) => expr_stack.push(&unary_exp.expr),
            Expr::ImplicitCast(cast) => expr_stack.push(&cast.expr),
            Expr::Call(call) => {
                if let Ident::Unresolved(_, pos) = &call.name {
                    pos.set(line_map.get_line(pos.get()));
                }
                for arg in call.args.iter() {
                    fix_module_ident_line_in_expr(line_map, arg);
                }
            }
            Expr::Assign(assign) => {
                expr_stack.push(&assign.lhs);
                expr_stack.push(&assign.rhs);
            }
        }
    }
}

struct LineMap {
    line_feed_pos: Vec<usize>,
}
impl LineMap {
    fn new(input: &str) -> Self {
        let mut line_feed_pos = vec![0];
        for (i, c) in input.char_indices() {
            if c == '\n' {
                line_feed_pos.push(i + 1);
            }
        }
        LineMap { line_feed_pos }
    }
    fn get_line(&self, pos: usize) -> usize {
        match self.line_feed_pos.binary_search(&pos) {
            Ok(idx) => idx + 1,
            Err(idx) => idx,
        }
    }
}
