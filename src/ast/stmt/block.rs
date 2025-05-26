use std::fmt::Debug;

use super::Stmt;

#[derive(Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.stmts.iter()).finish()
    }
}
