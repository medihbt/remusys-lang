use crate::typing::{AstType, FixedArrayType};

use super::{Expr, literal::Literal};

use std::rc::Rc;

#[derive(Debug, Clone, Hash)]
pub struct ArrayInitList {
    pub final_elems: Box<[Expr]>,
    pub type_levels: Box<[AstType]>,
    pub dimensions: Box<[usize]>,
    pub n_final_elems: Box<[usize]>,
}

impl ArrayInitList {
    pub fn new_zero(arr_type: Rc<FixedArrayType>) -> Self {
        let type_levels = Self::_build_type_levels(Rc::clone(&arr_type));
        let (dimensions, n_elems) = Self::_build_dimensions(&type_levels);

        let final_elems = {
            let final_type = type_levels.last().unwrap();
            let elem = match final_type {
                AstType::Int => Literal::Int(0),
                AstType::Float => Literal::Float(0.0),
                _ => panic!("Unsupported type {:?} for array initialization", final_type),
            };
            vec![Expr::Literal(elem); n_elems[0]].into_boxed_slice()
        };

        Self {
            final_elems,
            type_levels,
            dimensions,
            n_final_elems: n_elems,
        }
    }

    pub fn n_dimensions(&self) -> usize {
        self.dimensions.len() - 1
    }
    pub fn n_final_elements(&self) -> usize {
        self.dimensions[0]
    }
    pub fn final_elem_type(&self) -> &AstType {
        self.type_levels.last().unwrap()
    }

    /// Creates a dimension stack for a fixed array type.
    ///
    /// * Elements: The elemeent [0] is the original array type, while every level
    /// in the stack is the element type of the previous level.
    /// * Length: `n+1` for `n` dimensions.
    fn _build_type_levels(arr_type: Rc<FixedArrayType>) -> Box<[AstType]> {
        let mut curr_type = AstType::FixedArray(arr_type.clone());
        let mut type_levels = Vec::new();
        loop {
            type_levels.push(curr_type.clone());
            match &curr_type {
                AstType::FixedArray(farr) => {
                    curr_type = farr.elemty.clone();
                }
                _ => break type_levels.into_boxed_slice(),
            }
        }
    }

    /// Creates a dimension stack for a fixed array type.
    ///
    /// * Elements: The elemeent [0] is the total number of elements in the array,
    ///   while every level in the stack is the number of final elements in this level.
    ///   The top level is 1.
    /// * Length: `n+1` for `n` dimensions.
    fn _build_dimensions(type_levels: &[AstType]) -> (Box<[usize]>, Box<[usize]>) {
        let n_dimensions = type_levels.len() - 1;
        let mut dimensions = vec![0; n_dimensions + 1];
        let mut n_elems = vec![1; n_dimensions + 1];
        let mut total_nelems = 1;
        for i in (0..=n_dimensions).rev() {
            let arr_type = &type_levels[i];
            let (dim, total_nelems) = match arr_type {
                AstType::FixedArray(farr) => {
                    total_nelems *= farr.nelems;
                    (farr.nelems, total_nelems)
                }
                _ => (1, 1),
            };
            dimensions[i] = dim;
            n_elems[i] = total_nelems;
        }
        (dimensions.into_boxed_slice(), n_elems.into_boxed_slice())
    }
}

#[derive(Debug, Clone, Hash)]
pub struct RawInitList {
    pub items: Vec<Expr>,
}

impl RawInitList {
    pub fn new(exprs: Vec<Expr>) -> Box<Self> {
        Box::new(Self { items: exprs })
    }

    /// Converts the raw initialization list into an array initialization list.
    pub fn into_array(&self, array_ty: &AstType) -> Box<ArrayInitList> {
        let array_ty = match array_ty {
            AstType::FixedArray(arr_ty) => arr_ty,
            _ => panic!("Expected fixed array type"),
        };
        let mut ret = ArrayInitList::new_zero(Rc::clone(array_ty));
        Self::_do_fill_array(&mut ret, &self.items, 0, 1);
        Box::new(ret)
    }

    fn _do_fill_array(to_fill: &mut ArrayInitList, exprs: &[Expr], begin_idx: usize, level: usize) {
        let max_nelems = to_fill.n_final_elems[level - 1];
        let mut curr_idx = begin_idx;
        if curr_idx >= begin_idx + max_nelems {
            panic!("Index out of bound at this level {}", level);
        }
        for exp in exprs {
            match exp {
                Expr::Literal(lit) => {
                    to_fill.final_elems[curr_idx] = Expr::Literal(lit.clone());
                    curr_idx += 1;
                }
                Expr::RawInitList(r) => {
                    if level >= to_fill.n_dimensions() {
                        panic!(
                            "Level too high ({} >= {}): cannot initialize a literal element with array",
                            level,
                            to_fill.n_dimensions()
                        );
                    }
                    let skip_nelems = to_fill.n_final_elems[level];
                    curr_idx = Self::_ceil_to_multiple_of(curr_idx, skip_nelems);
                    Self::_do_fill_array(to_fill, &r.items, curr_idx, level + 1);
                    curr_idx += skip_nelems;
                }

                // Disable non-primitive types
                Expr::ArrayInitList(..) => panic!("Found partial-initialized items"),
                Expr::Assign(..) => panic!("Found assignment items"),
                Expr::IntrinsicTimeStart(..) => panic!("Found intrinsic time start items"),
                Expr::IntrinsicTimeEnd(..) => panic!("Found intrinsic time end items"),
                Expr::ShortCircuit(..) => panic!("Found short circuit items"),
                Expr::String(..) => panic!("Found string items"),

                // Other types
                _ => {
                    to_fill.final_elems[curr_idx] = exp.clone();
                    curr_idx += 1;
                }
            }
        }
    }

    const fn _ceil_to_multiple_of(x: usize, n: usize) -> usize {
        if n == 0 {
            panic!("N should not be 0");
        }
        ((x + n - 1) / n) * n
    }
}

#[cfg(test)]
mod testing {
    use super::*;

    #[test]
    fn test_array_init_list() {
        // int a[2][3] = { 1, 2, { 3, 4 } };
        let arr_raw_list = RawInitList::new(vec![
            Expr::Literal(Literal::Int(1)),
            Expr::Literal(Literal::Int(2)),
            Expr::RawInitList(RawInitList::new(vec![
                Expr::Literal(Literal::Int(3)),
                Expr::Literal(Literal::Int(4)),
            ])),
        ]);
        // int b[2][3] = { 1, 2, 3, 4, 5 ,6 }
        let brr_raw_list = RawInitList::new(vec![
            Expr::Literal(Literal::Int(1)),
            Expr::Literal(Literal::Int(2)),
            Expr::Literal(Literal::Int(3)),
            Expr::Literal(Literal::Int(4)),
            Expr::Literal(Literal::Int(5)),
            Expr::Literal(Literal::Int(6)),
        ]);

        // int c[2][3] = { { 1, 2, 3 }, { 4, 5, 6 } }
        let crr_raw_list = RawInitList::new(vec![
            Expr::RawInitList(RawInitList::new(vec![
                Expr::Literal(Literal::Int(1)),
                Expr::Literal(Literal::Int(2)),
                Expr::Literal(Literal::Int(3)),
            ])),
            Expr::RawInitList(RawInitList::new(vec![
                Expr::Literal(Literal::Int(4)),
                Expr::Literal(Literal::Int(5)),
                Expr::Literal(Literal::Int(6)),
            ])),
        ]);

        let arr_ty = AstType::new_multidimensional_array(AstType::Int, &[3, 2]).unwrap();
        println!("Array type: {}", arr_ty.to_string());

        let arr_init_list = arr_raw_list.into_array(&arr_ty);
        println!("Array `a` init list: {:#?}", arr_init_list);

        let brr_init_list = brr_raw_list.into_array(&arr_ty);
        println!("Array `b` init list: {:#?}", brr_init_list);

        let crr_init_list = crr_raw_list.into_array(&arr_ty);
        println!("Array `c` init list: {:#?}", crr_init_list);
    }
}
