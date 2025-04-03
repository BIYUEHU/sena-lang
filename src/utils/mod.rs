pub fn is_uppercase_first_letter(str: &str) -> bool {
    str.chars().next().map_or(false, |c| c.is_uppercase())
}
