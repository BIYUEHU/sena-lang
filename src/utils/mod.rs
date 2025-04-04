use crate::parser::ast::TypeExpr;

pub fn is_uppercase_first_letter(str: &str) -> bool {
    str.chars().next().map_or(false, |c| c.is_uppercase())
}

pub fn format_type_name(type_name: String, type_params: Vec<Box<TypeExpr>>) -> String {
    if type_params.is_empty() {
        type_name
    } else {
        format!(
            "{}({})",
            type_name,
            type_params
                .into_iter()
                .map(|t| { t.to_string() })
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}
