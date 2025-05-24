use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{
        expr::Expr,
        stmt::decl::{Function, Variable},
    },
    typing::AstType,
};

pub struct ScopeStack {
    pub items: Vec<Scope>,
}

pub trait ScopeContainer {
    fn make_scope(&self) -> Scope;
}

pub struct Scope {
    pub symbols: HashMap<String, ScopeSymbol>,
    pub layer: usize,
}

pub enum ScopeKind {
    Global,
    FuncArg,
    Local(usize),
}

pub enum ScopeSymbol {
    Const(Expr, AstType, Rc<Variable>),
    Var(Rc<Variable>),
    Func(Rc<Function>),
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            items: vec![Scope::new(0)],
        }
    }

    pub fn push(&mut self) {
        self.items.push(Scope::new(self.items.len()));
    }

    pub fn pop(&mut self) {
        self.items.pop();
    }

    pub fn current_scope(&self) -> &Scope {
        self.items.last().unwrap()
    }

    pub fn nlayers(&self) -> usize {
        self.items.len()
    }
}

impl Scope {
    pub fn new(layer: usize) -> Self {
        Self {
            symbols: HashMap::new(),
            layer,
        }
    }

    pub fn add_const(&mut self, name: String, value: Expr, ty: AstType, var: Rc<Variable>) {
        if self.symbols.contains_key(&name) {
            panic!("Symbol {} already exists in the current scope", name);
        }
        self.symbols
            .insert(name, ScopeSymbol::Const(value, ty, var));
    }
    pub fn add_var(&mut self, name: String, var: Rc<Variable>) {
        if self.symbols.contains_key(&name) {
            panic!("Symbol {} already exists in the current scope", name);
        }
        self.symbols.insert(name, ScopeSymbol::Var(var));
    }
    pub fn add_func(&mut self, name: String, func: Rc<Function>) {
        if !self.is_global_scope() {
            panic!("Cannot add function to non-global scope");
        }
        if self.symbols.contains_key(&name) {
            panic!("Symbol {} already exists in the current scope", name);
        }
        self.symbols.insert(name, ScopeSymbol::Func(func));
    }

    pub fn has_symbol(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }
    pub fn get_symbol(&self, name: &str) -> Option<&ScopeSymbol> {
        self.symbols.get(name)
    }
    pub fn get_var(&self, name: &str) -> Option<&Rc<Variable>> {
        if let Some(ScopeSymbol::Var(var)) = self.symbols.get(name) {
            Some(var)
        } else {
            None
        }
    }
    pub fn get_func(&self, name: &str) -> Option<&Rc<Function>> {
        if let Some(ScopeSymbol::Func(func)) = self.symbols.get(name) {
            Some(func)
        } else {
            None
        }
    }
    pub fn get_const(&self, name: &str) -> Option<(&Expr, &AstType, &Rc<Variable>)> {
        if let Some(ScopeSymbol::Const(expr, ty, var)) = self.symbols.get(name) {
            Some((expr, ty, var))
        } else {
            None
        }
    }

    pub fn get_kind(&self) -> ScopeKind {
        if self.layer == 0 {
            ScopeKind::Global
        } else if self.layer == 1 {
            ScopeKind::FuncArg
        } else {
            ScopeKind::Local(self.layer)
        }
    }

    pub fn is_global_scope(&self) -> bool {
        self.layer == 0
    }
    pub fn is_func_arg_scope(&self) -> bool {
        self.layer == 1
    }
    pub fn is_local_scope(&self) -> bool {
        self.layer > 1
    }
}
