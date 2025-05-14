#[derive(Debug, Clone, PartialEq)]
pub enum AstType {
    Void,
    Int,
    Float,
    FixedArray(Box<AstType>, u32),
    DynArray(Box<AstType>),
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
            AstType::Int => "int".to_string(),
            AstType::Float => "float".to_string(),
            AstType::FixedArray(ty, size) => format!("{}[{}]", ty.to_string(), size),
            AstType::DynArray(ty) => format!("{}[]", ty.to_string()),
        }
    }
}

impl AstType {
    pub fn meets_sized(&self) -> bool {
        match self {
            AstType::Void => false,
            AstType::Int => true,
            AstType::Float => true,
            AstType::FixedArray(ty, _) => ty.meets_sized(),
            AstType::DynArray(_) => false,
        }
    }

    pub fn get_instance_size(&self) -> Option<u32> {
        match self {
            AstType::Void => None,
            AstType::Int => Some(4),
            AstType::Float => Some(4),
            AstType::FixedArray(ty, nelems) => {
                if let Some(size) = ty.get_instance_size() {
                    Some(size * nelems)
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
            AstType::Int => 0,
            AstType::Float => 0,
            AstType::FixedArray(ty, _) => ty.get_array_level() + 1,
            AstType::DynArray(ty) => ty.get_array_level() + 1,
        }
    }
}

/// Create a new type
impl AstType {
    pub fn new_multidimensional_array(
        ty: AstType,
        dimensions: &[u32],
    ) -> Result<AstType, AstTypeError> {
        if dimensions.is_empty() {
            return Err(AstTypeError::DimensionsEmpty);
        }

        let mut current_type = ty;
        for dim in dimensions.iter().rev() {
            if *dim == 0 {
                return Err(AstTypeError::InvalidArraySize);
            }
            current_type = AstType::FixedArray(Box::new(current_type), *dim);
        }
        Ok(current_type)
    }
}
