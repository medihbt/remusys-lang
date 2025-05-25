use std::{cell::RefCell, rc::Rc};

use attr::Attr;
use expr::Expr;
use stmt::decl::{Function, VarKind, Variable};

use crate::typing::AstType;

pub mod attr;
pub mod expr;
pub mod operator;
pub mod stmt;

pub struct AstModule {
    pub file: String,
    pub global_defs: Vec<stmt::Stmt>,
}

impl AstModule {
    pub fn new_empty_ast(file: String) -> Self {
        Self {
            file,
            global_defs: vec![],
        }
    }
    pub fn new_empty_sst(file: String) -> Self {
        let mut module = Self::new_empty_ast(file);
        module.add_stdlib();
        module
    }

    fn add_stdlib(&mut self) {
        let dynarray_int = AstType::DynArray(Rc::new(AstType::Int));
        let dynarray_float = AstType::DynArray(Rc::new(AstType::Float));
        // [[Intrinsic(id="starttime")]]
        // void starttime();
        self.declare_intrin("starttime");
        // [[Intrinsic(id="endtime")]]
        // void endtime();
        self.declare_intrin("endtime");

        // int getint();
        self.declare_function("getint", AstType::Int, &[]);
        // int getch();
        self.declare_function("getch", AstType::Int, &[]);
        // float getfloat();
        self.declare_function("getfloat", AstType::Float, &[]);
        // int getarray(int a[]);
        self.declare_function(
            "getarray",
            AstType::Int,
            &[Variable::new_arg("a", dynarray_int)],
        );
        // int getfarray(float f[]);
        self.declare_function(
            "getfarray",
            AstType::Int,
            &[Variable::new_arg("f", dynarray_float)],
        );
        // void putint(int i);
        self.declare_function(
            "putint",
            AstType::Void,
            &[Variable::new_arg("i", AstType::Int)],
        );
        // void putch(int c);
        self.declare_function(
            "putch",
            AstType::Void,
            &[Variable::new_arg("c", AstType::Int)],
        );
        // void putfloat(float f);
        self.declare_function(
            "putfloat",
            AstType::Void,
            &[Variable::new_arg("f", AstType::Float)],
        );
        // void putf(__builtin_str fmt, ...);
        self.declare_function_full(
            "putf",
            AstType::Void,
            &[Variable::new_arg("fmt", AstType::Str)],
            true,
            None,
        );
    }

    pub fn declare_function_full(
        &mut self,
        name: &str,
        ret_type: AstType,
        args: &[Rc<Variable>],
        is_vararg: bool,
        attr: Option<Attr>,
    ) -> Rc<Function> {
        let func = Rc::new(Function {
            name: name.into(),
            ret_type,
            resolved_args: args.to_vec().into_boxed_slice(),
            unresolved_args: vec![],
            is_vararg,
            body: RefCell::new(None),
            attr,
        });
        self.global_defs.push(stmt::Stmt::FuncDecl(func.clone()));
        func
    }
    pub fn declare_intrin(&mut self, name: &str) -> Rc<Function> {
        let func = Rc::new(Function {
            name: name.into(),
            ret_type: AstType::Void,
            resolved_args: Box::new([]),
            unresolved_args: vec![],
            is_vararg: false,
            body: RefCell::new(None),
            attr: Some(Attr::Intrinsic { id: name.into() }),
        });
        self.global_defs.push(stmt::Stmt::FuncDecl(func.clone()));
        func
    }
    pub fn declare_function(
        &mut self,
        name: &str,
        ret_type: AstType,
        args: &[Rc<Variable>],
    ) -> Rc<Function> {
        self.declare_function_full(name, ret_type, args, false, None)
    }
}
