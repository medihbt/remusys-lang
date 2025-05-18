use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum AstType {
    Void,
    Bool,
    Int,
    Float,
    Str,
    FixedArray(Rc<FixedArrayType>),
    DynArray(Rc<AstType>),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FixedArrayType {
    pub elemty: AstType,
    pub nelems: usize,
}

impl ToString for FixedArrayType {
    fn to_string(&self) -> String {
        format!("[{}; {}]", self.elemty.to_string(), self.nelems)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstTypeError {
    InvalidArraySize,
    InvalidArrayType,

    DimensionsEmpty,
    DimensionsMismatch,
}

impl ToString for AstType {
    fn to_string(&self) -> String {
        match self {
            AstType::Void => "void".to_string(),
            AstType::Bool => "bool".to_string(),
            AstType::Int => "int".to_string(),
            AstType::Float => "float".to_string(),
            AstType::Str => "str".to_string(),
            AstType::FixedArray(farr) => farr.to_string(),
            AstType::DynArray(ty) => format!("{}[]", ty.to_string()),
        }
    }
}

impl AstType {
    pub fn meets_sized(&self) -> bool {
        match self {
            AstType::Void => false,
            AstType::Bool => true,
            AstType::Int => true,
            AstType::Float => true,
            AstType::Str => false,
            AstType::FixedArray(ty) => ty.elemty.meets_sized(),
            AstType::DynArray(_) => false,
        }
    }

    pub fn get_instance_size(&self) -> Option<usize> {
        match self {
            AstType::Void => None,
            AstType::Bool => Some(1),
            AstType::Int => Some(4),
            AstType::Float => Some(4),
            AstType::Str => None,
            AstType::FixedArray(ty) => {
                if let Some(size) = ty.elemty.get_instance_size() {
                    Some(size * ty.nelems)
                } else {
                    None
                }
            }
            AstType::DynArray(_) => None,
        }
    }

    pub fn get_array_level(&self) -> u32 {
        match self {
            AstType::Void => 0,
            AstType::Bool => 0,
            AstType::Int => 0,
            AstType::Float => 0,
            AstType::Str => 0,
            AstType::FixedArray(ty) => ty.elemty.get_array_level() + 1,
            AstType::DynArray(ty) => ty.get_array_level() + 1,
        }
    }
}

/// Create a new type
impl AstType {
    /// Create a multidimensional array type.
    ///
    /// e.g. `int[2][3]` -> `new_multidimensional_array(AstType::Int, &[3, 2])`
    ///
    /// # Arguments
    /// * `ty` - The base type of the array.
    /// * `dimensions` - The dimensions of the array. from outermost to innermost;
    ///    reversed from the C order and follows the Rust order. `e.g.`:
    ///   * `int[2][3]` -> `&[3, 2]`
    ///   * `[[i32;2];3]` -> `&[2, 3]`
    pub fn new_multidimensional_array(
        ty: AstType,
        dimensions: &[usize],
    ) -> Result<AstType, AstTypeError> {
        if dimensions.is_empty() {
            return Err(AstTypeError::DimensionsEmpty);
        }

        let mut current_type = ty;
        for dim in dimensions.iter() {
            if *dim == 0 {
                return Err(AstTypeError::InvalidArraySize);
            }
            current_type = AstType::FixedArray(Rc::new(FixedArrayType {
                elemty: current_type,
                nelems: *dim,
            }));
        }
        Ok(current_type)
    }
}
