//! ## SysY 扩展语法: 注解
//!
//! SysY 标准库里有一些需要编译器开洞才能实现的函数. 为了把这些函数
//! 归一化处理, 我们为函数添加注解语法.
//!
//! ### 使用示例
//!
//! ```C
//! [[Intrinsic(id="starttime")]]
//! void starttime();
//!
//! [[Intrinsic(id="endtime")]]
//! void endtime();
//! ```
//!
//! ### 语法
//!
//! ```BNF
//! Attr ::= '[[' Idnetifier '(' AttrItem ')' ']]'
//! AttrItem ::= Idnetifier '=' String
//! ```

pub enum Attr {
    Unresolved(UnresolvedAttr),
    Intrinsic { id: String },
}

pub struct UnresolvedAttr {
    pub id: String,
    pub attr_item: Vec<(String, String)>,
}

impl UnresolvedAttr {
    pub fn resolve(&self) -> Attr {
        match self.id.as_str() {
            "Intrinsic" => {
                if self.attr_item.len() != 1 {
                    panic!("`Intrinsic` attribute must have exactly one item");
                }
                let (key, value) = &self.attr_item[0];
                if key != "id" {
                    panic!("`Intrinsic` attribute item must be `id`");
                }
                if value.is_empty() {
                    panic!("`Intrinsic` attribute item `id` must not be empty");
                }
                Attr::Intrinsic { id: value.clone() }
            }
            _ => panic!("Unknown attribute: {}", self.id),
        }
    }
}
