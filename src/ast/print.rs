use core::panic;
use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use super::{
    AstModule,
    expr::{Expr, ident::Ident},
    stmt::{
        Stmt,
        block::Block,
        decl::{Function, UnresolvedVarDecl, UnresolvedVariable, VarDecl, Variable},
    },
};

pub struct AstPrinter<'a> {
    pub ast_module: &'a AstModule,
    pub writer: RefCell<&'a mut dyn std::io::Write>,
    indent: Cell<usize>,
}

enum ExprState<'a> {
    Enter {
        ident: usize,
        should_wrap: bool,
        prefix: String,
        expr: &'a Expr,
    },
    Exit {
        indent: usize,
        should_wrap: bool,
        expr: &'a Expr,
    },
}

impl<'a> AstPrinter<'a> {
    pub fn new(ast_module: &'a AstModule, writer: &'a mut dyn std::io::Write) -> Self {
        Self {
            ast_module,
            writer: RefCell::new(writer),
            indent: Cell::new(0),
        }
    }

    fn wrap_indent(&self) {
        let indent_str = if self.indent.get() <= 30 {
            "  ".repeat(self.indent.get())
        } else {
            format!(
                "/*level {} (too long)*/{}",
                self.indent.get(),
                "  ".repeat(18)
            )
        };
        self.write_str("\n");
        self.write_str(indent_str.as_str());
    }
    fn push_indent(&self) {
        self.indent.set(self.indent.get() + 1);
    }
    fn pop_indent(&self) {
        self.indent.set(self.indent.get().saturating_sub(1));
    }
    fn write_str(&self, s: &str) {
        let mut writer = self.writer.borrow_mut();
        write!(writer, "{}", s).expect("Failed to write to writer");
    }
    fn write_string(&self, s: String) {
        let mut writer = self.writer.borrow_mut();
        write!(writer, "{}", s).expect("Failed to write string to writer");
    }
    fn write_fmt(&self, fmtarg: std::fmt::Arguments) {
        let mut writer = self.writer.borrow_mut();
        writer
            .write_fmt(fmtarg)
            .expect("Failed to write formatted string to writer");
    }

    pub fn print_module(&mut self) {
        for gdef in self.ast_module.global_defs.iter() {
            self.wrap_indent();
            match gdef {
                Stmt::UnresolvedVarDecl(var) => self.run_on_unresolved_vardecl(var),
                Stmt::VarDecl(var_decl) => self.run_on_vardecl(var_decl),
                Stmt::FuncDecl(function) => self.print_function(function),
                _ => panic!("Unexpected global definition in module"),
            }
        }
    }
    fn run_on_unresolved_vardecl(&mut self, vardecl: &UnresolvedVarDecl) {
        self.write_fmt(format_args!(
            "UnresolvedVarDecl (is_const = {}, base_type = {:?}, n_defs = {}), ",
            vardecl.is_const,
            vardecl.base_type,
            vardecl.defs.len()
        ));
        self.run_on_unresolved_vars("Vars", &vardecl.defs, true);
    }
    fn run_on_vardecl(&mut self, vardecl: &VarDecl) {
        self.write_fmt(format_args!(
            "VarDecl (is_const = {}, base_type = {:?}, n_defs = {}), ",
            vardecl.is_const,
            vardecl.base_type,
            vardecl.defs.len()
        ));
        self.write_str("Vars: [");
        if !vardecl.defs.is_empty() {
            self.run_on_resolved_vars(&vardecl.defs);
        }
        self.write_str("]");
    }
    fn run_on_resolved_vars(&mut self, vars: &[Rc<Variable>]) {
        self.push_indent();
        for (idx, var) in vars.iter().enumerate() {
            self.wrap_indent();
            self.write_fmt(format_args!(
                "[{}] = \"{}\" (kind = {:?}, type = {})",
                idx, var.name, var.kind, var.var_type.to_string()
            ));
            if !matches!(var.initval, Expr::None) {
                self.write_str(" = ");
                self.run_on_expr(&var.initval);
            }
        }
        self.pop_indent();
        self.wrap_indent();
    }

    pub fn print_function(&mut self, func: &Function) {
        self.write_fmt(format_args!(
            "Function (name = {}, return type = {:?}, is_vararg = {}, is_extern = {}, attr = {:?}) {{",
            func.name,
            func.ret_type,
            func.is_vararg,
            func.is_extern(),
            func.attr
        ));
        self.push_indent();
        self.wrap_indent();
        self.run_on_unresolved_vars("Arguments", &func.unresolved_args, false);
        if !func.resolved_args.is_empty() {
            self.wrap_indent();
            self.write_str("Resolved Arguments: [");
            self.run_on_resolved_vars(&func.resolved_args);
            self.write_str("]");
        }

        self.wrap_indent();
        if let Some(body) = func.body.borrow().as_ref() {
            self.write_str("Body: ");
            self.run_on_block(body);
        } else {
            self.write_str("Body: None");
        }
        self.pop_indent();
        self.wrap_indent();
        self.write_str("}");
    }
    fn run_on_unresolved_vars(
        &mut self,
        label: &str,
        args: &[UnresolvedVariable],
        print_init: bool,
    ) {
        self.write_fmt(format_args!("{label} ({} items): [", args.len()));
        if !args.is_empty() {
            self.push_indent();
            for (idx, arg) in args.iter().enumerate() {
                self.wrap_indent();
                self.write_fmt(format_args!(
                    "[{}] = `{}` (base_type = {:?}) {{",
                    idx, arg.name, arg.base_type,
                ));
                let mut should_wrap = false;
                if !arg.array_dims.is_empty() {
                    self.push_indent();
                    self.wrap_indent();
                    self.write_str("array_dims: [");
                    self.push_indent();
                    for (idx, dim) in arg.array_dims.iter().enumerate() {
                        self.wrap_indent();
                        self.write_fmt(format_args!("[{}] = ", idx));
                        self.run_on_expr(dim);
                    }
                    self.pop_indent();
                    self.wrap_indent();
                    self.write_str("]");
                    self.pop_indent();
                    should_wrap = true;
                }
                if print_init && !matches!(arg.initval, Expr::None) {
                    self.push_indent();
                    self.wrap_indent();
                    self.write_str("initval: ");
                    self.run_on_expr(&arg.initval);
                    self.pop_indent();
                    should_wrap = true;
                }
                if should_wrap {
                    self.wrap_indent();
                }
                self.write_str("}");
            }
            self.pop_indent();
            self.wrap_indent();
        }
        self.write_str("]");
    }

    fn run_on_block(&mut self, block: &Block) {
        self.write_fmt(format_args!("Block (n_stmts: {}) {{", block.stmts.len()));
        if block.stmts.is_empty() {
            self.write_str("}");
            return;
        }
        self.push_indent();
        for (idx, stmt) in block.stmts.iter().enumerate() {
            self.wrap_indent();
            self.write_fmt(format_args!("[{}] = ", idx));
            self.run_on_stmt(stmt);
        }
        self.pop_indent();
        self.wrap_indent();
        self.write_str("}");
    }
    fn run_on_stmt(&mut self, stmt: &Stmt) {
        // 还没碰到那种递归太深爆栈的 stmt, 先用递归解法吧.
        match stmt {
            Stmt::None => self.write_str("Stmt::None"),
            Stmt::Block(block) => self.run_on_block(block),
            Stmt::UnresolvedVarDecl(var) => self.run_on_unresolved_vardecl(&var),
            Stmt::VarDecl(var_decl) => self.run_on_vardecl(var_decl),
            Stmt::FuncDecl(function) => panic!(
                "Unexpected function declaration in statement: {}",
                function.name
            ),
            Stmt::If(if_stmt) => {
                self.write_fmt(format_args!(
                    "If (has_else = {}) {{",
                    if_stmt.else_stmt.is_some()
                ));
                self.push_indent();
                self.wrap_indent();
                self.write_str("condition: ");
                self.run_on_expr(&if_stmt.cond);
                self.wrap_indent();
                self.write_str("then: ");
                self.run_on_stmt(&if_stmt.then_stmt);
                if let Some(else_stmt) = &if_stmt.else_stmt {
                    self.wrap_indent();
                    self.write_str("else: ");
                    self.run_on_stmt(else_stmt);
                }
                self.pop_indent();
                self.wrap_indent();
                self.write_str("}");
            }
            Stmt::While(while_stmt) => {
                self.write_str("While {");
                self.push_indent();
                self.wrap_indent();
                self.write_str("condition: ");
                self.run_on_expr(&while_stmt.cond);
                self.wrap_indent();
                self.write_str("body: ");
                self.run_on_stmt(&while_stmt.body.borrow());
                self.pop_indent();
                self.wrap_indent();
                self.write_str("}");
            }
            Stmt::ExprStmt(expr_stmt) => {
                self.write_str("ExprStmt { ");
                self.run_on_expr(&expr_stmt.expr.borrow());
                self.write_str(" }");
            }
            Stmt::Return(expr) => {
                self.write_str("Return { ");
                self.run_on_expr(expr);
                self.write_str(" }");
            }
            Stmt::Break => self.write_str("Stmt::Break"),
            Stmt::Continue => self.write_str("Stmt::Continue"),
            Stmt::BreakTo(_) => {
                self.write_str("BreakTo (???)");
            }
            Stmt::ContinueTo(_) => {
                self.write_str("ContinueTo (???)");
            }
        }
    }

    fn run_on_expr(&mut self, expr: &Expr) {
        let mut expr_stack = vec![ExprState::Enter {
            ident: self.indent.get(),
            should_wrap: false,
            prefix: String::new(),
            expr,
        }];

        while let Some(state) = expr_stack.pop() {
            match state {
                ExprState::Enter { .. } => self.run_on_expr_enter(&mut expr_stack, state),
                ExprState::Exit { .. } => self.run_on_expr_exit(state),
            }
        }
    }
    fn run_on_expr_enter<'b>(
        &mut self,
        expr_stack: &mut Vec<ExprState<'b>>,
        curr_state: ExprState<'b>,
    ) {
        let ExprState::Enter {
            ident,
            should_wrap,
            prefix,
            expr,
        } = curr_state
        else {
            panic!("Expected Enter state");
        };

        self.indent.set(ident);
        if should_wrap {
            self.wrap_indent();
        }
        self.write_string(prefix);
        match expr {
            Expr::None => self.write_str("None"),
            Expr::Literal(literal) => self.write_string(literal.to_string()),
            Expr::String(s) => self.write_fmt(format_args!("String({})", s)),
            Expr::RawInitList(initlist) => {
                self.write_fmt(format_args!(
                    "RawInitList (nitems: {})",
                    initlist.items.len()
                ));
                if initlist.items.is_empty() {
                    self.write_str("[]");
                    return;
                }
                self.write_str("[");
                expr_stack.push(ExprState::Exit {
                    indent: self.indent.get(),
                    should_wrap: true,
                    expr,
                });
                self.push_indent();
                for (idx, item) in initlist.items.iter().enumerate().rev() {
                    expr_stack.push(ExprState::Enter {
                        ident: self.indent.get(),
                        should_wrap: true,
                        prefix: format!("[{}] = ", idx),
                        expr: item,
                    });
                }
            }
            Expr::ArrayInitList(arrinit) => {
                self.write_fmt(format_args!(
                    "ArrayInitList (n_final_elems: {}, type: {:?}) {{",
                    arrinit.final_elems.len(),
                    arrinit.type_levels[0]
                ));
                self.push_indent();
                self.write_fmt(format_args!("dimensions: {:?}", arrinit.dimensions));
                self.write_str("final_elems: [");
                expr_stack.push(ExprState::Exit {
                    indent: self.indent.get(),
                    should_wrap: true,
                    expr,
                });
                self.push_indent();
                for (idx, item) in arrinit.final_elems.iter().enumerate().rev() {
                    expr_stack.push(ExprState::Enter {
                        ident: self.indent.get(),
                        should_wrap: true,
                        prefix: format!("[{}] = ", idx),
                        expr: item,
                    });
                }
            }
            Expr::Ident(ident) => self.write_fmt(format_args!("Ident({})", ident.get_name())),
            Expr::ArrayIndex(array_index) => {
                self.write_fmt(format_args!(
                    "ArrayIndex (array: {}, n_indices: {}) [",
                    array_index.indexee.get_name(),
                    array_index.indices.len()
                ));
                if array_index.indices.is_empty() {
                    self.write_str("]");
                    return;
                }
                expr_stack.push(ExprState::Exit {
                    indent: self.indent.get(),
                    should_wrap: true,
                    expr,
                });
                self.push_indent();
                for (idx, index) in array_index.indices.iter().enumerate().rev() {
                    expr_stack.push(ExprState::Enter {
                        ident: self.indent.get(),
                        should_wrap: true,
                        prefix: format!("[{}] = ", idx),
                        expr: index,
                    });
                }
            }
            Expr::BinOP(bin) => {
                self.write_fmt(format_args!("BinOP (op: {:?}) {{", bin.op));
                expr_stack.push(ExprState::Exit {
                    indent: self.indent.get(),
                    should_wrap: true,
                    expr,
                });
                self.push_indent();
                expr_stack.push(ExprState::Enter {
                    ident: self.indent.get(),
                    should_wrap: true,
                    prefix: "rhs: ".to_string(),
                    expr: &bin.rhs,
                });
                expr_stack.push(ExprState::Enter {
                    ident: self.indent.get(),
                    should_wrap: true,
                    prefix: "lhs: ".to_string(),
                    expr: &bin.lhs,
                });
            }
            Expr::CmpOP(cmp) => {
                self.write_fmt(format_args!("CmpOP (op: {:?}) {{", cmp.op));
                expr_stack.push(ExprState::Exit {
                    indent: self.indent.get(),
                    should_wrap: true,
                    expr,
                });
                self.push_indent();
                expr_stack.push(ExprState::Enter {
                    ident: self.indent.get(),
                    should_wrap: true,
                    prefix: "rhs: ".to_string(),
                    expr: &cmp.rhs,
                });
                expr_stack.push(ExprState::Enter {
                    ident: self.indent.get(),
                    should_wrap: true,
                    prefix: "lhs: ".to_string(),
                    expr: &cmp.lhs,
                });
            }
            Expr::ShortCircuit(logic) => {
                self.write_fmt(format_args!("ShortCircuit (op: {:?}) {{", logic.op));
                expr_stack.push(ExprState::Exit {
                    indent: self.indent.get(),
                    should_wrap: true,
                    expr,
                });
                self.push_indent();
                expr_stack.push(ExprState::Enter {
                    ident: self.indent.get(),
                    should_wrap: true,
                    prefix: "rhs: ".to_string(),
                    expr: &logic.rhs,
                });
                expr_stack.push(ExprState::Enter {
                    ident: self.indent.get(),
                    should_wrap: true,
                    prefix: "lhs: ".to_string(),
                    expr: &logic.lhs,
                });
            }
            Expr::UnaryOP(una) => {
                self.write_fmt(format_args!("UnaryOP (op: {:?}) {{", una.op));
                expr_stack.push(ExprState::Exit {
                    indent: self.indent.get(),
                    should_wrap: true,
                    expr,
                });
                self.push_indent();
                expr_stack.push(ExprState::Enter {
                    ident: self.indent.get(),
                    should_wrap: true,
                    prefix: "operand: ".to_string(),
                    expr: &una.expr,
                });
            }
            Expr::ImplicitCast(cast) => {
                self.write_fmt(format_args!(
                    "ImplicitCast (kind: {:?}, to_type: {:?}) {{",
                    cast.kind, cast.target
                ));
                expr_stack.push(ExprState::Exit {
                    indent: self.indent.get(),
                    should_wrap: true,
                    expr,
                });
                self.push_indent();
                expr_stack.push(ExprState::Enter {
                    ident: self.indent.get(),
                    should_wrap: true,
                    prefix: "operand: ".to_string(),
                    expr: &cast.expr,
                });
            }
            Expr::Call(call) => {
                self.write_fmt(format_args!(
                    "Call (func: {}, line: {}, n_args: {}) [",
                    call.name.get_name(),
                    if let Ident::Unresolved(_, pos) = &call.name {
                        pos.get()
                    } else {
                        0
                    },
                    call.args.len()
                ));
                if call.args.is_empty() {
                    self.write_str("]");
                    return;
                }
                expr_stack.push(ExprState::Exit {
                    indent: self.indent.get(),
                    should_wrap: true,
                    expr,
                });
                self.push_indent();
                for (idx, arg) in call.args.iter().enumerate().rev() {
                    expr_stack.push(ExprState::Enter {
                        ident: self.indent.get(),
                        should_wrap: true,
                        prefix: format!("[{}] = ", idx),
                        expr: arg,
                    });
                }
            }
            Expr::Assign(assign) => {
                self.write_str("Assign {");
                expr_stack.push(ExprState::Exit {
                    indent: self.indent.get(),
                    should_wrap: true,
                    expr,
                });
                self.push_indent();
                expr_stack.push(ExprState::Enter {
                    ident: self.indent.get(),
                    should_wrap: true,
                    prefix: "from: ".to_string(),
                    expr: &assign.rhs,
                });
                expr_stack.push(ExprState::Enter {
                    ident: self.indent.get(),
                    should_wrap: true,
                    prefix: "to: ".to_string(),
                    expr: &assign.lhs,
                });
            }
            Expr::IntrinsicTimeStart(line) => {
                self.write_fmt(format_args!("IntrinsicTimeStart (line: {})", line));
            }
            Expr::IntrinsicTimeEnd(line) => {
                self.write_fmt(format_args!("IntrinsicTimeEnd (line: {})", line));
            }
        }
    }

    fn run_on_expr_exit<'b>(&mut self, curr_state: ExprState<'b>) {
        let ExprState::Exit {
            indent,
            should_wrap,
            expr,
        } = curr_state
        else {
            panic!("Expected Exit state");
        };

        self.indent.set(indent);
        if should_wrap {
            self.wrap_indent();
        }

        match expr {
            Expr::RawInitList(_) | Expr::Call(_) => {
                self.write_str("]");
            }
            Expr::ArrayInitList(_) => {
                self.write_str("]");
                self.pop_indent();
                self.wrap_indent();
                self.write_str("}");
            }
            Expr::ArrayIndex(_) => {
                self.write_str("]");
            }
            Expr::BinOP(_)
            | Expr::CmpOP(_)
            | Expr::ShortCircuit(_)
            | Expr::UnaryOP(_)
            | Expr::ImplicitCast(_)
            | Expr::Assign(_) => {
                self.write_str("}");
            }
            _ => {}
        }
    }
}
