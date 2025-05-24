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
    pub fn new_empty(file: String) -> Self {
        Self {
            file,
            global_defs: vec![],
        }
    }
    pub fn new_with_stdlib(file: String) -> Self {
        let mut module = Self::new_empty(file);
        module.add_stdlib();
        module
    }

    fn add_stdlib(&mut self) {
        // [[Intrinsic(id="starttime")]]
        // void starttime();
        self.declare_function_full(
            "starttime".into(),
            AstType::Void,
            &[],
            false,
            Some(Attr::Intrinsic {
                id: "starttime".into(),
            }),
        );
        // [[Intrinsic(id="endtime")]]
        // void endtime();
        self.declare_function_full(
            "endtime".into(),
            AstType::Void,
            &[],
            false,
            Some(Attr::Intrinsic {
                id: "endtime".into(),
            }),
        );

        // int getint();
        self.declare_function_full(
            "getint".into(),
            AstType::Int,
            &[],
            false,
            Some(Attr::Intrinsic {
                id: "getint".into(),
            }),
        );
        // int getch();
        self.declare_function_full(
            "getch".into(),
            AstType::Int,
            &[],
            false,
            Some(Attr::Intrinsic { id: "getch".into() }),
        );
        // float getfloat();
        self.declare_function_full(
            "getfloat".into(),
            AstType::Float,
            &[],
            false,
            Some(Attr::Intrinsic {
                id: "getfloat".into(),
            }),
        );
        // int getarray(int a[]);
        self.declare_function_full(
            "getarray".into(),
            AstType::Int,
            &[Rc::new(Variable {
                name: "a".into(),
                var_type: AstType::DynArray(Rc::new(AstType::Int)),
                initval: RefCell::new(Expr::None),
                kind: VarKind::FuncArg,
            })],
            false,
            Some(Attr::Intrinsic {
                id: "getarray".into(),
            }),
        );
        // int getfarray(float f[]);
        self.declare_function_full(
            "getfarray".into(),
            AstType::Int,
            &[Rc::new(Variable {
                name: "f".into(),
                var_type: AstType::DynArray(Rc::new(AstType::Float)),
                initval: RefCell::new(Expr::None),
                kind: VarKind::FuncArg,
            })],
            false,
            Some(Attr::Intrinsic {
                id: "getfarray".into(),
            }),
        );
        // void putint(int i);
        self.declare_function_full(
            "putint".into(),
            AstType::Void,
            &[Rc::new(Variable {
                name: "i".into(),
                var_type: AstType::Int,
                initval: RefCell::new(Expr::None),
                kind: VarKind::FuncArg,
            })],
            false,
            Some(Attr::Intrinsic {
                id: "putint".into(),
            }),
        );
        // void putch(int c);
        self.declare_function_full(
            "putch".into(),
            AstType::Void,
            &[Rc::new(Variable {
                name: "c".into(),
                var_type: AstType::Int,
                initval: RefCell::new(Expr::None),
                kind: VarKind::FuncArg,
            })],
            false,
            Some(Attr::Intrinsic { id: "putch".into() }),
        );
        // void putfloat(float f);
        self.declare_function_full(
            "putfloat".into(),
            AstType::Void,
            &[Rc::new(Variable {
                name: "f".into(),
                var_type: AstType::Float,
                initval: RefCell::new(Expr::None),
                kind: VarKind::FuncArg,
            })],
            false,
            Some(Attr::Intrinsic {
                id: "putfloat".into(),
            }),
        );
        // void putf(__builtin_str fmt, ...);
        self.declare_function_full(
            "putf".into(),
            AstType::Void,
            &[Rc::new(Variable {
                name: "fmt".into(),
                var_type: AstType::Str,
                initval: RefCell::new(Expr::None),
                kind: VarKind::FuncArg,
            })],
            true,
            Some(Attr::Intrinsic { id: "putf".into() }),
        );
    }

    pub fn declare_function_full(
        &mut self,
        name: String,
        ret_type: AstType,
        args: &[Rc<Variable>],
        is_vararg: bool,
        attr: Option<Attr>,
    ) -> Rc<RefCell<Function>> {
        let func = Rc::new(RefCell::new(Function {
            name: name.clone(),
            ret_type,
            resolved_args: args.to_vec().into_boxed_slice(),
            unresolved_args: vec![],
            is_vararg,
            body: None,
            attr,
        }));
        self.global_defs.push(stmt::Stmt::FuncDecl(func.clone()));
        func
    }
}
