//! Expand an initializer list into its normalized form.

use crate::{
    ast::expr::{Expr, Literal},
    typing::AstType,
};

/// Expand an initializer list into its normalized form.
///
/// ### Expansion Example
///
/// ```SysY
/// int a[4][2] = {};
///     => {{0, 0}, {0, 0}, {0, 0}, {0, 0}}
///
/// int b[4][2] = {1, 2, 3, 4, 5, 6, 7, 8};
/// int c[4][2] = {1, 2, 3, 4, {5, 6}, {7, 8}};
/// int d[4][2] = {{1, 2}, {3, 4}, 5, 6, 7, 8};
///     => {{1, 2}, {3, 4}, {5, 6}, {7, 8}}
///
/// int e[4][2] = {1, 2, {3}, {5}, 7, 8};
///     => {{1, 2}, {3, 0}, {5, 0}, {7, 8}}
/// ```
///
/// ### Expansion Rule
///
/// 1. Scan and fill the array by row with final elements. The position tuple of the elements
///    is calculated from the increasing number.
/// 2. If encountering an internal initializer list, recursively expand it and skip the next-level
///    index.
/// 3. If this level of initializer list is comsumed, fill the rest of the array with 0.
pub fn expand_init_list(raw: &[Expr], value_type: &AstType) -> Expr {
    match value_type {
        AstType::FixedArray(..) => {}
        _ => panic!(
            "Initializer list must be a fixed array, but got {:?}",
            value_type
        ),
    }
}

struct MultiDimensionIndex {
    indices: Box<[u32]>,
    ending: Box<[u32]>,
    skip_level: usize,
}

impl MultiDimensionIndex {
    pub fn from_array_type<'a>(ty: &'a AstType) -> (Self, &'a AstType) {
        let ndimensions = ty.get_array_level() as usize;
        let mut ending = Vec::with_capacity(ndimensions);

        let mut current = ty;
        while let AstType::FixedArray(ty, nelems) = current {
            ending.push(*nelems);
            current = ty;
        }
        ending.reverse();
        let indices = vec![0; ndimensions].into_boxed_slice();
        let ret = Self {
            indices,
            ending: ending.into_boxed_slice(),
            skip_level: 0,
        };
        (ret, current)
    }

    pub fn next(&mut self) -> bool {
        self._range_succ(self.indices.len() - 1)
    }
    pub fn skip_this_level(&mut self) -> bool {
        if self.skip_level >= self.indices.len() {
            return false;
        }
        for i in (self.skip_level + 1)..self.indices.len() {
            self.indices[i] = 0;
        }
        self._range_succ(self.skip_level)
    }
    fn _range_succ(&mut self, init_index: usize) -> bool {
        let mut curr_level = init_index;
        loop {
            let next_index = self.indices[curr_level] + 1;

            if next_index < self.ending[curr_level] {
                self.indices[curr_level] = next_index;
                return true;
            } else {
                if curr_level == 0 {
                    return false;
                }
                self.indices[curr_level] = 0;
                curr_level -= 1;
            }
        }
    }

    pub fn push_skip_level(&mut self) {
        if self.skip_level < self.indices.len() {
            self.skip_level += 1;
        }
    }
    pub fn pop_skip_level(&mut self) {
        if self.skip_level > 0 {
            self.skip_level -= 1;
        }
    }
    pub fn ends(&self) -> bool {
        self.indices[0] == self.ending[0]
    }
}

struct ArrayListBuilder<'a> {
    to_build: &'a mut [Expr],
    arr_type: &'a AstType,
    val_type: &'a AstType,
    index: MultiDimensionIndex,
}

impl<'a> ArrayListBuilder<'a> {
    pub fn build_init_array(arr_type: &AstType) -> Box<[Expr]> {
        let len = match arr_type {
            AstType::FixedArray(_, len) => len.clone(),
            _ => panic!(""),
        };
        vec![Expr::None; len as usize].into_boxed_slice()
    }

    pub fn new(arr_type: &'a AstType, to_build: &'a mut [Expr]) -> Self {
        let (index, val_type) = MultiDimensionIndex::from_array_type(arr_type);

        let init_val = match val_type {
            AstType::Int => Literal::Int(0),
            AstType::Float => Literal::Float(0.0),
            _ => panic!(
                "Array final element type should be int or float, but got {:?}",
                val_type
            ),
        };
        Self::fill_zero_to_build(&init_val, &index.ending, to_build);
        Self {
            to_build,
            arr_type,
            val_type,
            index,
        }
    }

    fn fill_zero_to_build(initval: &Literal, dimensions: &[u32], to_build: &mut [Expr]) {
        if dimensions.is_empty() {
            return;
        }
        if dimensions.len() == 1 {
            for i in to_build {
                *i = Expr::Literal(initval.clone())
            }
        } else {
            for i in to_build {
                let dimension = dimensions[0];
                let mut child_exp = vec![Expr::None; dimension as usize].into_boxed_slice();
                Self::fill_zero_to_build(initval, &dimensions[1..], &mut child_exp);
                *i = Expr::InitList(child_exp);
            }
        }
    }

    pub fn push_value(&mut self, value: Literal) -> bool {

    }

    fn index_get_value(&self) -> &Expr {
        let mut ret = &self.to_build[0];
        for i in &self.index.indices {
            if let Expr::InitList(list) = ret {
                ret = &list[*i as usize];
            } else {
                panic!("Index out of range");
            }
        }
        ret
    }
    fn index_set_value(&mut self, value: Expr) {
        let mut ret = &mut self.to_build[0];
        for i in &self.index.indices {
            if let Expr::InitList(list) = ret {
                ret = &mut list[*i as usize];
            } else {
                panic!("Index out of range");
            }
        }
        *ret = value;
    }
}
