use error::PreprocessError;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use crate::utils::constant::MIHAMA_LANGUAGE_EXTENSION;

pub mod error;

pub const PRELUDE_CODE: &'static str = include_str!("../../../prelude.mh");

#[derive(Debug, Clone)]
pub struct Macro {
    pub params: Vec<String>,
    pub body: String,
}

#[derive(Debug)]
pub struct PreprocessContext {
    defines: HashMap<String, String>,
    macros: HashMap<String, Macro>,
    ifdef_stack: Vec<bool>,
    importing_files: Vec<PathBuf>,
    current_dir: PathBuf,
}

impl PreprocessContext {
    pub fn new() -> Self {
        Self {
            defines: HashMap::new(),
            macros: HashMap::new(),
            ifdef_stack: Vec::new(),
            importing_files: Vec::new(),
            current_dir: std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
        }
    }

    fn is_enabled(&self) -> bool {
        self.ifdef_stack.iter().all(|&enabled| enabled)
    }

    fn resolve_path(&self, path: &str) -> PathBuf {
        if path.starts_with("./") || path.starts_with("../") {
            self.current_dir.join(path)
        } else {
            PathBuf::from(path)
        }
    }
}

pub struct Preprocessor;

impl Preprocessor {
    pub fn process(source: &str) -> Result<String, PreprocessError> {
        let mut context = PreprocessContext::new();
        Self::process_with_context(source, &mut context)
    }

    pub fn process_file(file_path: &str) -> Result<String, PreprocessError> {
        let mut context = PreprocessContext::new();
        let path = PathBuf::from(file_path);
        if let Some(parent) = path.parent() {
            context.current_dir = parent.to_path_buf();
        }

        context.importing_files.push(path.clone());
        let source = fs::read_to_string(&path)?;
        let result = Self::process_with_context(&source, &mut context);
        context.importing_files.pop();
        result
    }

    fn process_with_context(
        source: &str,
        context: &mut PreprocessContext,
    ) -> Result<String, PreprocessError> {
        let lines: Vec<&str> = source.lines().collect();
        let mut result = Vec::new();
        let mut i = 0;

        while i < lines.len() {
            let line = lines[i].trim();

            if line.starts_with('@') {
                let (processed_lines, skip_count) =
                    Self::process_directive(line, &lines, i, context)?;
                if context.is_enabled() {
                    for processed_line in processed_lines {
                        result.push(processed_line);
                    }
                }
                i += skip_count;
            } else {
                if context.is_enabled() {
                    let expanded = Self::expand_line(line, context)?;
                    result.push(expanded);
                }
                i += 1;
            }
        }

        if !context.ifdef_stack.is_empty() {
            return Err(PreprocessError::UnbalancedConditional);
        }

        Ok(result.join("\n"))
    }

    fn process_directive(
        directive: &str,
        _lines: &[&str],
        _current_idx: usize,
        context: &mut PreprocessContext,
    ) -> Result<(Vec<String>, usize), PreprocessError> {
        let parts: Vec<&str> = directive.split_whitespace().collect();
        if parts.is_empty() {
            return Ok((vec![], 1));
        }

        match parts[0] {
            "@define" => {
                if context.is_enabled() && parts.len() >= 2 {
                    let name = parts[1].to_string();
                    let value = if parts.len() >= 3 {
                        parts[2..].join(" ")
                    } else {
                        String::new()
                    };
                    context.defines.insert(name, value);
                }
                Ok((vec![], 1))
            }

            "@macro" => {
                if context.is_enabled() && parts.len() >= 2 {
                    Self::process_macro_definition(directive, context)?;
                }
                Ok((vec![], 1))
            }

            "@import" => {
                if context.is_enabled() && parts.len() >= 2 {
                    let file_path = parts[1].trim_matches('"');
                    let content = Self::process_import(file_path, context)?;
                    Ok((vec![content], 1))
                } else {
                    Ok((vec![], 1))
                }
            }

            "@ifdef" => {
                if parts.len() >= 2 {
                    let name = parts[1];
                    let enabled = context.defines.contains_key(name) && context.is_enabled();
                    context.ifdef_stack.push(enabled);
                }
                Ok((vec![], 1))
            }

            "@ifndef" => {
                if parts.len() >= 2 {
                    let name = parts[1];
                    let enabled = !context.defines.contains_key(name) && context.is_enabled();
                    context.ifdef_stack.push(enabled);
                }
                Ok((vec![], 1))
            }

            "@endif" => {
                if context.ifdef_stack.is_empty() {
                    return Err(PreprocessError::UnbalancedConditional);
                }
                context.ifdef_stack.pop();
                Ok((vec![], 1))
            }

            _ => Err(PreprocessError::InvalidDirective(directive.to_string())),
        }
    }

    fn process_macro_definition(
        directive: &str,
        context: &mut PreprocessContext,
    ) -> Result<(), PreprocessError> {
        // 解析 @macro name(params) => body
        if let Some(arrow_pos) = directive.find("=>") {
            let left = directive[6..arrow_pos].trim(); // 跳过 "@macro"
            let body = directive[arrow_pos + 2..].trim().to_string();

            if let Some(paren_start) = left.find('(') {
                if let Some(paren_end) = left.find(')') {
                    let name = left[..paren_start].trim().to_string();
                    let params_str = &left[paren_start + 1..paren_end];
                    let params: Vec<String> = if params_str.trim().is_empty() {
                        Vec::new()
                    } else {
                        params_str
                            .split(',')
                            .map(|p| p.trim().to_string())
                            .collect()
                    };

                    context.macros.insert(name, Macro { params, body });
                    return Ok(());
                }
            }
        }
        Err(PreprocessError::InvalidDirective(directive.to_string()))
    }

    fn process_import(
        file_path: &str,
        context: &mut PreprocessContext,
    ) -> Result<String, PreprocessError> {
        if file_path == "prelude" || file_path == "prelude.mh" {
            return Ok(PRELUDE_CODE.to_string());
        }

        let resolved_path = context.resolve_path(file_path);

        // 循环导入检测
        if context.importing_files.contains(&resolved_path) {
            return Err(PreprocessError::CircularImport(file_path.to_string()));
        }

        // let source = fs::read_to_string(&resolved_path)
        //     .map_err(|_| PreprocessError::FileNotFound(file_path.to_string()))?;

        let suffix = format!(".{}", MIHAMA_LANGUAGE_EXTENSION);
        let source = fs::read_to_string(if file_path.ends_with(suffix.as_str()) {
            file_path.to_string()
        } else {
            format!("{}{}", file_path, suffix)
        })
        .map_err(|_| PreprocessError::FileNotFound(file_path.to_string()))?;

        // 递归处理
        let old_dir = context.current_dir.clone();
        if let Some(parent) = resolved_path.parent() {
            context.current_dir = parent.to_path_buf();
        }

        context.importing_files.push(resolved_path);
        let result = Self::process_with_context(&source, context);
        context.importing_files.pop();
        context.current_dir = old_dir;

        result
    }

    fn expand_line(line: &str, context: &PreprocessContext) -> Result<String, PreprocessError> {
        let mut result = line.to_string();

        // 展开简单的 define - 使用更简单的字符串替换
        for (name, value) in &context.defines {
            // 找到所有出现的位置，检查是否为独立的词
            let chars: Vec<char> = result.chars().collect();
            let name_chars: Vec<char> = name.chars().collect();
            let mut new_result = String::new();
            let mut i = 0;

            while i < chars.len() {
                if i + name_chars.len() <= chars.len() {
                    let slice: Vec<char> = chars[i..i + name_chars.len()].to_vec();
                    if slice == name_chars {
                        // 检查词边界
                        let prev_ok =
                            i == 0 || !chars[i - 1].is_alphanumeric() && chars[i - 1] != '_';
                        let next_ok = i + name_chars.len() == chars.len()
                            || (!chars[i + name_chars.len()].is_alphanumeric()
                                && chars[i + name_chars.len()] != '_');

                        if prev_ok && next_ok {
                            new_result.push_str(value);
                            i += name_chars.len();
                            continue;
                        }
                    }
                }
                new_result.push(chars[i]);
                i += 1;
            }
            result = new_result;
        }

        result = Self::expand_macro_calls(&result, context)?;

        Ok(result)
    }

    fn expand_macro_calls(
        line: &str,
        context: &PreprocessContext,
    ) -> Result<String, PreprocessError> {
        let mut result = line.to_string();

        for (macro_name, macro_def) in &context.macros {
            if let Some(call_start) = result.find(&format!("{}(", macro_name)) {
                if let Some(call_end) =
                    Self::find_matching_paren(&result, call_start + macro_name.len())
                {
                    let args_str = &result[call_start + macro_name.len() + 1..call_end];
                    let args: Vec<String> = if args_str.trim().is_empty() {
                        Vec::new()
                    } else {
                        args_str.split(',').map(|a| a.trim().to_string()).collect()
                    };

                    if args.len() != macro_def.params.len() {
                        return Err(PreprocessError::MacroExpansionError(format!(
                            "Macro {} expects {} arguments, got {}",
                            macro_name,
                            macro_def.params.len(),
                            args.len()
                        )));
                    }

                    let mut expanded = macro_def.body.clone();
                    for (param, arg) in macro_def.params.iter().zip(args.iter()) {
                        expanded = expanded.replace(param, arg);
                    }

                    let before = &result[..call_start];
                    let after = &result[call_end + 1..];
                    result = format!("{}{}{}", before, expanded, after);
                }
            }
        }

        Ok(result)
    }

    fn find_matching_paren(s: &str, start: usize) -> Option<usize> {
        let chars: Vec<char> = s.chars().collect();
        if start >= chars.len() || chars[start] != '(' {
            return None;
        }

        let mut depth = 1;
        for i in start + 1..chars.len() {
            match chars[i] {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(i);
                    }
                }
                _ => {}
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_define() {
        let source = r#"
@define PI 3.14159
let area = PI * r * r
"#;
        let result = Preprocessor::process(source).unwrap();
        assert!(result.contains("3.14159 * r * r"));
    }

    #[test]
    fn test_macro() {
        let source = r#"
@macro max(a, b) => if a > b then a else b
let result = max(x, y)
"#;
        let result = Preprocessor::process(source).unwrap();
        assert!(result.contains("if x > y then x else y"));
    }

    #[test]
    fn test_conditional_compilation() {
        let source = r#"
@define DEBUG
@ifdef DEBUG
print("Debug mode")
@endif
@ifndef RELEASE
print("Not release")
@endif
"#;
        let result = Preprocessor::process(source).unwrap();
        // println!("Processing result:\n{}", result);
        // println!("Lines: {:?}", result.lines().collect::<Vec<_>>());
        assert!(result.contains("Debug mode"));
        assert!(result.contains("Not release"));
    }
}
