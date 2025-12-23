/// Name mangling for runtime functions with type-specific overloads
///
/// Scheme:
/// - Function name + `__` + type signature
/// - Parameter types separated by `__`
/// - Vectors: `vec_<element_type>`
/// - Scalars: `<type_name>`
///
/// Examples:
/// - `system_vector_add__vec_int__vec_int` - vector<int> + vector<int>
/// - `system_vector_add__vec_int__int` - vector<int> + int
/// - `system_vector_add__vec_double__vec_double` - vector<double> + vector<double>

use crate::types::{Type, ParamKind};

/// Mangle a type into a string for function name overloading
pub fn mangle_type(ty: &Type) -> String {
    match ty {
        Type::Int => "int".to_string(),
        Type::Float => "float".to_string(),
        Type::Double => "double".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "string".to_string(),
        Type::Vector(elem_ty) => {
            format!("vec_{}", mangle_type(elem_ty))
        }
        Type::List => "list".to_string(),
        Type::Void => "void".to_string(),
        Type::Any => "any".to_string(),
        Type::VarArgs => "varargs".to_string(),
        Type::Function { params, return_type } => {
            let param_mangles: Vec<String> = params
                .iter()
                .filter_map(|p| match &p.kind {
                    ParamKind::Normal(ty) => Some(mangle_type(ty)),
                    ParamKind::VarArgs => Some("varargs".to_string()),
                })
                .collect();
            let ret_mangle = mangle_type(return_type);
            format!("fn_{}__ret_{}", param_mangles.join("_"), ret_mangle)
        }
        Type::Reference(inner) => format!("ref_{}", mangle_type(inner)),
    }
}

/// Generate a mangled function name for runtime functions
///
/// # Arguments
/// * `base_name` - The base function name (e.g., "system_vector_add")
/// * `param_types` - The parameter types
///
/// # Returns
/// Mangled function name (e.g., "system_vector_add__vec_int__vec_int")
pub fn mangle_function_name(base_name: &str, param_types: &[Type]) -> String {
    if param_types.is_empty() {
        return base_name.to_string();
    }

    let type_suffix = param_types
        .iter()
        .map(mangle_type)
        .collect::<Vec<_>>()
        .join("__");

    format!("{}___{}", base_name, type_suffix)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mangle_scalar_types() {
        assert_eq!(mangle_type(&Type::Int), "int");
        assert_eq!(mangle_type(&Type::Float), "float");
        assert_eq!(mangle_type(&Type::Double), "double");
    }

    #[test]
    fn test_mangle_vector_types() {
        assert_eq!(mangle_type(&Type::Vector(Box::new(Type::Int))), "vec_int");
        assert_eq!(mangle_type(&Type::Vector(Box::new(Type::Double))), "vec_double");
    }

    #[test]
    fn test_mangle_function_name() {
        let name = mangle_function_name(
            "system_vector_add",
            &[
                Type::Vector(Box::new(Type::Int)),
                Type::Vector(Box::new(Type::Int)),
            ],
        );
        assert_eq!(name, "system_vector_add___vec_int__vec_int");

        let name2 = mangle_function_name(
            "system_vector_add",
            &[Type::Vector(Box::new(Type::Double)), Type::Double],
        );
        assert_eq!(name2, "system_vector_add___vec_double__double");
    }
}
