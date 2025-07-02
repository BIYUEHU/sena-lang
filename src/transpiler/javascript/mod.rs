use super::traits::*;
use crate::checker::ast::*;
use crate::lexer::token::Token;
use crate::parser::ast::{Literal, Pattern};
use std::collections::HashMap;

pub struct JavaScriptTranspiler {
    generator: JavaScriptGenerator,
}

impl Transpiler for JavaScriptTranspiler {
    type Generator = JavaScriptGenerator;

    fn new() -> Self {
        Self {
            generator: JavaScriptGenerator::new(),
        }
    }

    fn transpile(&mut self, program: &[CheckedStmt]) -> String {
        self.generator.generate_program(program)
    }
}

pub struct JavaScriptGenerator {
    symbol_operators: HashMap<String, String>,
    indentation_level: usize,
}

impl JavaScriptGenerator {
    pub fn new() -> Self {
        let mut symbol_operators = HashMap::new();
        symbol_operators.insert("#:#".to_string(), "colon".to_string());
        symbol_operators.insert("#$#".to_string(), "dollar".to_string());
        symbol_operators.insert("#.#".to_string(), "dot".to_string());

        Self {
            symbol_operators,
            indentation_level: 0,
        }
    }

    fn indent(&self) -> String {
        "  ".repeat(self.indentation_level)
    }

    fn with_indent<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.indentation_level += 1;
        let result = f(self);
        self.indentation_level -= 1;
        result
    }

    fn sanitize_identifier(&self, name: &str) -> String {
        if let Some(js_name) = self.symbol_operators.get(name) {
            js_name.clone()
        } else if name.starts_with('#') && name.ends_with('#') {
            // 处理其他符号操作符
            name.chars()
                .filter(|&c| c != '#')
                .map(|c| match c {
                    ':' => "colon",
                    '$' => "dollar",
                    '.' => "dot",
                    '+' => "plus",
                    '-' => "minus",
                    '*' => "multiply",
                    '/' => "divide",
                    '%' => "modulo",
                    '=' => "equals",
                    '<' => "less",
                    '>' => "greater",
                    '!' => "not",
                    '&' => "and",
                    '|' => "or",
                    '^' => "xor",
                    _ => "_",
                })
                .collect::<Vec<_>>()
                .join("")
        } else {
            name.to_string()
        }
    }

    fn generate_let_statement(&mut self, name: &str, value: &CheckedExpr) -> String {
        let js_name = self.sanitize_identifier(name);
        let js_value = self.generate_expression(value);
        format!("const {} = {}", js_name, js_value)
    }

    fn generate_type_statement(&mut self, name: &str, variants: &[CheckedTypeVariant]) -> String {
        let mut lines = Vec::new();

        // 生成类型构造器
        lines.push(format!(
            "const {} = {}",
            name,
            self.create_type_constructor(name)
        ));

        // 生成变体构造器
        for variant in variants {
            match &variant.fields {
                CheckedTypeVariantFields::Unit => {
                    lines.push(format!(
                        "const {} = {}",
                        variant.name,
                        self.create_value_constructor(&variant.name, None)
                    ));
                }
                CheckedTypeVariantFields::Tuple(types) => {
                    let params: Vec<String> =
                        (0..types.len()).map(|i| format!("arg{}", i)).collect();

                    let constructor_body = if params.is_empty() {
                        self.create_value_constructor(&variant.name, None)
                    } else {
                        let args = format!("[{}]", params.join(", "));
                        self.create_value_constructor(&variant.name, Some(&args))
                    };

                    lines.push(format!(
                        "const {} = {}",
                        variant.name,
                        self.curry_function(params.len(), &constructor_body)
                    ));
                }
                CheckedTypeVariantFields::Record(_fields) => {
                    // TODO: 处理记录类型
                    lines.push(format!(
                        "const {} = {}",
                        variant.name,
                        self.create_value_constructor(&variant.name, None)
                    ));
                }
            }
        }

        lines.join(";\n")
    }

    fn generate_function_expression(&mut self, params: &[String], body: &CheckedExpr) -> String {
        if params.is_empty() {
            format!("() => {}", self.generate_expression(body))
        } else if params.len() == 1 {
            format!("({}) => {}", params[0], self.generate_expression(body))
        } else {
            let function_body = self.generate_expression(body);
            self.curry_function(params.len(), &function_body)
        }
    }

    fn generate_call_expression(&mut self, callee: &CheckedExpr, params: &[CheckedExpr]) -> String {
        let callee_str = self.generate_expression(callee);

        if params.is_empty() {
            format!("{}()", callee_str)
        } else if params.len() == 1 {
            format!("{}({})", callee_str, self.generate_expression(&params[0]))
        } else {
            // 对于多参数调用，生成链式调用
            params.iter().fold(callee_str, |acc, param| {
                format!("{}({})", acc, self.generate_expression(param))
            })
        }
    }

    fn generate_if_expression(
        &mut self,
        condition: &CheckedExpr,
        then_branch: &CheckedExpr,
        else_branch: &CheckedExpr,
    ) -> String {
        format!(
            "{} ? {} : {}",
            self.generate_expression(condition),
            self.generate_expression(then_branch),
            self.generate_expression(else_branch)
        )
    }

    fn generate_let_in_expression(
        &mut self,
        name: &str,
        value: &CheckedExpr,
        body: &CheckedExpr,
    ) -> String {
        format!(
            "(({}) => {})({})",
            self.sanitize_identifier(name),
            self.generate_expression(body),
            self.generate_expression(value)
        )
    }

    fn generate_block_expression(&mut self, stmts: &[CheckedStmt]) -> String {
        let mut lines = Vec::new();
        lines.push("(() => {".to_string());

        let statements: Vec<String> = self.with_indent(|gen| {
            stmts
                .iter()
                .enumerate()
                .map(|(i, stmt)| {
                    let stmt_str = gen.generate_statement(stmt);
                    if i == stmts.len() - 1 {
                        // 最后一个语句作为返回值
                        match stmt {
                            CheckedStmt::Expr(_) => format!("{}return {}", gen.indent(), stmt_str),
                            _ => format!(
                                "{}{}\n{}return undefined",
                                gen.indent(),
                                stmt_str,
                                gen.indent()
                            ),
                        }
                    } else {
                        format!("{}{}", gen.indent(), stmt_str)
                    }
                })
                .collect()
        });

        lines.extend(statements);
        lines.push("})()".to_string());
        lines.join("\n")
    }

    fn generate_infix_expression(
        &mut self,
        op: &str,
        left: &CheckedExpr,
        right: &CheckedExpr,
    ) -> String {
        let left_str = self.generate_expression(left);
        let right_str = self.generate_expression(right);

        let js_op = match op {
            "==" => "===",
            "!=" => "!==",
            "&&" => "&&",
            "||" => "||",
            _ => op,
        };

        format!("{} {} {}", left_str, js_op, right_str)
    }

    fn generate_pattern_guard(
        &mut self,
        expr: &CheckedExpr,
        pattern: &Pattern,
        guard: &CheckedExpr,
    ) -> String {
        let expr_str = self.generate_expression(expr);

        match pattern {
            Pattern::Ident(name) => {
                if name == "_" {
                    "true".to_string()
                } else {
                    format!("true") // 标识符模式总是匹配
                }
            }
            Pattern::Literal(value) => {
                let literal_str = self.generate_literal(value);
                let condition = format!("{} === {}", expr_str, literal_str);

                // 如果有守卫条件，添加到匹配条件中
                match guard {
                    CheckedExpr::Literal {
                        value: Literal::Bool(true),
                        ..
                    } => condition,
                    _ => format!("{} && {}", condition, self.generate_expression(guard)),
                }
            }
            Pattern::ADTConstructor { name, args } => {
                let tag_check = format!("{}.value_tag === \"{}\"", expr_str, name);

                if args.is_empty() {
                    match guard {
                        CheckedExpr::Literal {
                            value: Literal::Bool(true),
                            ..
                        } => tag_check,
                        _ => format!("{} && {}", tag_check, self.generate_expression(guard)),
                    }
                } else {
                    // TODO: 处理构造器参数的模式匹配
                    let mut conditions = vec![tag_check];

                    // 添加参数匹配条件
                    for (i, param) in args.iter().enumerate() {
                        match param {
                            Pattern::Ident(name) if name != "_" => {
                                // 绑定变量，不需要额外条件
                            }
                            Pattern::Literal(value) => {
                                let param_check = format!(
                                    "{}.value[{}] === {}",
                                    expr_str,
                                    i,
                                    self.generate_literal(value)
                                );
                                conditions.push(param_check);
                            }
                            _ => {
                                // TODO: 处理嵌套模式
                            }
                        }
                    }

                    let full_condition = conditions.join(" && ");
                    match guard {
                        CheckedExpr::Literal {
                            value: Literal::Bool(true),
                            ..
                        } => full_condition,
                        _ => format!("{} && {}", full_condition, self.generate_expression(guard)),
                    }
                }
            }
        }
    }
}

impl CodeGenerator for JavaScriptGenerator {
    type Output = String;

    fn generate_program(&mut self, program: &[CheckedStmt]) -> String {
        let mut lines = vec![self.runtime_support()];

        for stmt in program {
            lines.push(self.generate_statement(stmt));
        }

        lines.join("\n")
    }

    fn generate_statement(&mut self, stmt: &CheckedStmt) -> String {
        match stmt {
            CheckedStmt::Let { name, value, .. } => {
                format!("{};", self.generate_let_statement(name, value))
            }
            CheckedStmt::Type { name, variants, .. } => {
                format!("{};", self.generate_type_statement(name, variants))
            }
            CheckedStmt::Expr(expr) => self.generate_expression(expr),
            CheckedStmt::ImportAll { .. } => {
                // TODO: 处理导入语句
                "// Import statement not implemented".to_string()
            }
            CheckedStmt::ImportSome { .. } => {
                // TODO: 处理导入语句
                "// Import statement not implemented".to_string()
            }
            CheckedStmt::Export { .. } => {
                // TODO: 处理导出语句
                "// Export statement not implemented".to_string()
            }
        }
    }

    fn generate_expression(&mut self, expr: &CheckedExpr) -> String {
        match expr {
            CheckedExpr::Ident { value, .. } => self.sanitize_identifier(value),
            CheckedExpr::Literal { value, .. } => self.generate_literal(value),
            CheckedExpr::Function { params, body, .. } => {
                self.generate_function_expression(params, body)
            }
            CheckedExpr::Call { callee, params, .. } => {
                self.generate_call_expression(callee, params)
            }
            CheckedExpr::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => self.generate_if_expression(condition, then_branch, else_branch),
            CheckedExpr::Match { expr, cases, .. } => self.generate_pattern_match(expr, cases),
            CheckedExpr::LetIn {
                name, value, body, ..
            } => self.generate_let_in_expression(name, value, body),
            CheckedExpr::Block { stmts, .. } => self.generate_block_expression(stmts),
            CheckedExpr::Infix {
                op, left, right, ..
            } => {
                let op_str = match op {
                    Token::Plus => "+",
                    Token::Sub => "-",
                    Token::Mul => "*",
                    Token::Div => "/",
                    Token::Mod => "%",
                    Token::Pow => "**",
                    Token::Equal => "==",
                    Token::NotEqual => "!=",
                    Token::Less => "<",
                    Token::LessEqual => "<=",
                    Token::Greater => ">",
                    Token::GreaterEqual => ">=",
                    Token::And => "&&",
                    Token::Or => "||",
                    _ => "unknown_op",
                };
                self.generate_infix_expression(op_str, left, right)
            }
            CheckedExpr::Prefix { op, expr, .. } => {
                let op_str = match op {
                    Token::Sub => "-",
                    Token::Not => "!",
                    _ => "unknown_prefix_op",
                };
                format!("{}{}", op_str, self.generate_expression(expr))
            }
            CheckedExpr::Internal { value, .. } => {
                // 处理内部函数，如 print
                match value.as_str() {
                    "print" => "console.log".to_string(),
                    _ => value.clone(),
                }
            }
        }
    }

    fn generate_literal(&mut self, literal: &Literal) -> String {
        match literal {
            Literal::Int(n) => n.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::String(s) => format!("\"{}\"", s.replace("\"", "\\\"")),
            Literal::Char(c) => format!("'{}'", c),
            Literal::Bool(b) => b.to_string(),
            // Literal::Array(elements) => {
            //     let element_strs: Vec<String> = elements.iter()
            //         .map(|elem| self.generate_literal(elem))
            //         .collect();
            //     format!("[{}]", element_strs.join(", "))
            // }
            _ => unimplemented!(),
        }
    }

    fn generate_pattern_match(&mut self, expr: &CheckedExpr, cases: &[CheckedCase]) -> String {
        let mut conditions = Vec::new();

        for case in cases {
            let condition = self.generate_pattern_guard(expr, &case.pattern, &case.guard);
            let body = self.generate_expression(&case.body);
            conditions.push(format!("{} ? {}", condition, body));
        }

        // 添加默认的错误抛出
        conditions.push(
            "(() => { throw new MihamaError(\"Match pattern not exhaustive\") })()".to_string(),
        );

        conditions.join(" :\n  ")
    }

    fn runtime_support(&self) -> String {
        r#"class MihamaError extends Error { }

const mihamaCurry = (f) => {
  const curried = (expectedLength, args) => ((...rest) => {
    if (rest.length === 0 || expectedLength === 0) {
      throw new MihamaError(`Arguments cannot be empty for function ${f.name}`);
    }

    if (rest.length === expectedLength) {
      return f(...args, ...rest);
    }

    if (rest.length > expectedLength) {
      throw new MihamaError(`Too many arguments for function ${f.name}`);
    }

    return curried(expectedLength - rest.length, [...args, ...rest])
  });

  return curried(f.length, []);
}

const createMihamaType = (tag) => ({ type_tag: tag })
const createMihamaValue = (tag, value) => ({ value_tag: tag, value })

const [MihamaKind, MihamaString, MihamaInt, MihamaBool, MihamaChar] = ["Kind", "String", "Int", "Bool", "Char"].map(createMihamaType)

// 以上为预定义内容"#.to_string()
    }

    fn curry_function(&self, params_count: usize, body: &str) -> String {
        if params_count <= 1 {
            format!("(arg0) => {}", body)
        } else {
            let params: Vec<String> = (0..params_count).map(|i| format!("arg{}", i)).collect();
            format!("mihamaCurry(({}) => {})", params.join(", "), body)
        }
    }

    fn create_type_constructor(&self, name: &str) -> String {
        format!("createMihamaType(\"{}\")", name)
    }

    fn create_value_constructor(&self, tag: &str, value: Option<&str>) -> String {
        match value {
            Some(val) => format!("createMihamaValue(\"{}\", {})", tag, val),
            None => format!("createMihamaValue(\"{}\")", tag),
        }
    }
}
