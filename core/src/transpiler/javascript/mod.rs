use super::traits::*;
use crate::checker::ast::*;
use crate::checker::object::TypeObject;
use crate::lexer::token::Token;
use crate::parser::ast::{Literal, Pattern};
use crate::utils::{is_op_char, to_checked_expr};

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
    indentation_level: usize,
}

impl JavaScriptGenerator {
    pub fn new() -> Self {
        Self {
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
        name.chars()
            .map(|ch| match ch {
                '+' => 'P',
                '-' => 'S',
                '*' => 'M',
                '/' => 'D',
                '%' => 'M',
                '^' => 'P',
                '=' => 'A',
                '>' => 'G',
                '<' => 'L',
                '!' => 'N',
                '|' => 'R',
                '$' => 'O',
                ':' => 'C',
                '.' => 'T',
                s => s,
            })
            .collect()
    }

    fn generate_let_statement(&mut self, name: &str, value: &CheckedExpr) -> String {
        let js_name = self.sanitize_identifier(name);
        let js_value = self.generate_expression(value);
        format!("const {} = {}", js_name, js_value)
    }

    fn generate_type_statement(
        &mut self,
        name: &str,
        params: &[String],
        variants: &[CheckedTypeVariant],
    ) -> String {
        let mut lines = Vec::new();

        // 生成类型构造器
        lines.push(format!(
            "const {} = {}",
            name,
            self.create_type_constructor(name, params)
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
                        self.curry_function(&params, &constructor_body)
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
            self.curry_function(params, &function_body)
        }
    }

    fn generate_call_expression(&mut self, callee: &CheckedExpr, params: &[CheckedExpr]) -> String {
        let callee_str = self.generate_expression(callee);

        if params.is_empty() {
            format!("{}()", callee_str)
        } else if params.len() == 1 {
            format!("{}({})", callee_str, self.generate_expression(&params[0]))
        } else {
            // params.iter().fold(callee_str, |acc, param| {
            //     format!("{}({})", acc, self.generate_expression(param))
            // })
            format!(
                "{}({})",
                callee_str,
                params
                    .into_iter()
                    .map(|param| self.generate_expression(param))
                    .collect::<Vec<String>>()
                    .join(", "),
            )
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
        let mut lines = Vec::new();
        lines.push("(() => {".to_string());

        self.with_indent(|gen| {
            lines.push(format!(
                "{}const {} = {}",
                gen.indent(),
                gen.sanitize_identifier(name),
                gen.generate_expression(value)
            ));
            lines.push(format!(
                "{}return {}",
                gen.indent(),
                gen.generate_expression(body)
            ));
        });

        lines.push("})()".to_string());
        lines.join("\n")
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
        op: String,
        left: &CheckedExpr,
        right: &CheckedExpr,
    ) -> String {
        let left_str = self.generate_expression(left);
        let right_str = self.generate_expression(right);

        if op.chars().next().map(is_op_char).unwrap_or(false) {
            let js_op = match op.as_str() {
                "==" => "===",
                "!=" => "!==",
                "&&" => "&&",
                "||" => "||",
                _ => op.as_str(),
            };
            format!("{} {} {}", left_str, js_op, right_str)
        } else {
            format!("{}({}, {})", op, left_str, right_str)
        }
    }

    fn generate_pattern_guard(
        &mut self,
        expr: &CheckedExpr,
        pattern: &Pattern,
        guard: &CheckedExpr,
    ) -> String {
        let expr_str = self.generate_expression(expr);
        let guard = match guard {
            CheckedExpr::Literal {
                value: Literal::Bool(true),
                ..
            } => "".to_string(),
            _ => format!(" && {}", self.generate_expression(guard)),
        };

        match pattern {
            Pattern::Ident(_) => {
                // if name == "_" {
                "true".to_string()
                // } else {
                // format!("true")
                // }
            }
            Pattern::Literal(value) => {
                let literal_str = self.generate_literal(value);
                let condition = format!("{} === {}", expr_str, literal_str);
                format!("{}{}", condition, guard)
            }
            Pattern::ADTConstructor { name, args } => {
                let tag_check = format!("{}.value_tag === \"{}\"", expr_str, name);

                if args.is_empty() {
                    format!("{}{}", tag_check, guard)
                } else {
                    let mut conditions = vec![tag_check];
                    let mut bindings = Vec::new();

                    for (i, param) in args.iter().enumerate() {
                        match param {
                            Pattern::Ident(name) if name != "_" => {
                                bindings.push(format!(
                                    "globalThis.{} = {}.value[{}]",
                                    self.sanitize_identifier(name),
                                    expr_str,
                                    i
                                ));
                                // bindings.push((self.sanitize_identifier(name), i));
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
                            Pattern::ADTConstructor {
                                name: nested_name,
                                args: nested_args,
                            } => {
                                let nested_expr_str = format!("{}.value[{}]", expr_str, i);
                                let nested_pattern = Pattern::ADTConstructor {
                                    name: nested_name.clone(),
                                    args: nested_args.clone(),
                                };
                                let nested_guard = CheckedExpr::Literal {
                                    value: Literal::Bool(true),
                                    type_annotation: TypeObject::Any,
                                };

                                let temp_expr = CheckedExpr::Internal {
                                    value: nested_expr_str,
                                    type_annotation: TypeObject::Any,
                                };

                                let nested_condition = self.generate_pattern_guard(
                                    &temp_expr,
                                    &nested_pattern,
                                    &nested_guard,
                                );
                                conditions.push(nested_condition);
                            }
                            _ => {}
                        }
                    }

                    let full_condition = conditions.join(" && ");
                    if bindings.is_empty() {
                        format!("{}{}", full_condition, guard)
                    } else {
                        // let mut bindings_left = vec![];
                        // let mut should_index = 0;
                        // for (name, index) in bindings {
                        //     let distance = index - should_index;
                        //     // if distance > 0 {
                        //     for _ in 0..distance {
                        //         bindings_left.push(" ".to_string());
                        //     }
                        //     bindings_left.push(name);
                        //     should_index = index + 1;
                        //     // }
                        // }
                        // format!(
                        //     "{}__BINDINGS__const [{}] = {}.value;",
                        //     final_condition,
                        //     bindings_left.join(", "),
                        //     expr_str
                        // )
                        format!(
                            "({} && (() => {{ {}; return true; }})()){}",
                            full_condition,
                            bindings.join("; "),
                            guard
                        )
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
            CheckedStmt::Type {
                name,
                variants,
                params,
                ..
            } => {
                format!("{};", self.generate_type_statement(name, params, variants))
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
            CheckedExpr::Ident { value, .. } => match value.as_str() {
                "Kind" | "String" | "Int" | "Bool" | "Char" => format!("Mihama{}", value),
                _ => self.sanitize_identifier(value),
            },
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
            } => self.generate_infix_expression(
                match op {
                    Token::Plus => "+".to_string(),
                    Token::Sub => "-".to_string(),
                    Token::Mul => "*".to_string(),
                    Token::Div => "/".to_string(),
                    Token::Mod => "%".to_string(),
                    Token::Pow => "**".to_string(),
                    Token::Equal => "==".to_string(),
                    Token::NotEqual => "!=".to_string(),
                    Token::Less => "<".to_string(),
                    Token::LessEqual => "<=".to_string(),
                    Token::Greater => ">".to_string(),
                    Token::GreaterEqual => ">=".to_string(),
                    Token::And => "&&".to_string(),
                    Token::Or => "||".to_string(),
                    Token::InfixIdent(str) => str.clone(),
                    Token::InfixFixity(tokens) => self.sanitize_identifier(
                        tokens
                            .into_iter()
                            .map(|t| t.to_string())
                            .collect::<String>()
                            .as_str(),
                    ),
                    _ => self.sanitize_identifier(op.to_string().as_str()),
                },
                left,
                right,
            ),
            CheckedExpr::Prefix { op, expr, .. } => {
                let op_str = match op {
                    Token::Sub => "-",
                    Token::Not => "!",
                    _ => unreachable!(),
                };
                format!("{}{}", op_str, self.generate_expression(expr))
            }
            // CheckedExpr::Internal { value, .. } => match value.as_str() {
            //     "print" => "console.log".to_string(),
            //     _ => value.clone(),
            // },
            _ => unreachable!(),
        }
    }

    fn generate_literal(&mut self, literal: &Literal) -> String {
        match literal {
            Literal::Int(n) => n.to_string(),
            Literal::Float(f) => f.to_string(),
            Literal::String(s) => format!("\"{}\"", s.replace("\"", "\\\"")),
            Literal::Char(c) => format!("'{}'", c),
            Literal::Bool(b) => b.to_string(),
            Literal::Array(elements) => {
                let element_strs: Vec<String> = elements
                    .iter()
                    .map(|elem| self.generate_expression(&to_checked_expr(elem.clone())))
                    .collect();
                format!("[{}]", element_strs.join(", "))
            }
            Literal::Unit => "undefined".to_string(),
        }
    }

    fn generate_pattern_match(&mut self, expr: &CheckedExpr, cases: &[CheckedCase]) -> String {
        let mut conditions = Vec::new();

        for case in cases {
            let body = self.generate_expression(&case.body);
            let condition = self.generate_pattern_guard(expr, &case.pattern, &case.guard);
            let split: Vec<&str> = condition.split("__BINDINGS__").into_iter().collect();
            conditions.push(if split.len() == 1 {
                format!("{} ? {}", condition, body)
            } else {
                let condition = split[0];
                let bindings = split[1];
                format!(
                    "{} ? (() => {{ {}; return {}; }})()",
                    condition, bindings, body
                )
            })
        }

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

const print = console.log
const get_timestamp = () => new Date().getTime() / 1000
const concat = (a, b) => `${a}${b}`
"#.to_string()
    }

    fn curry_function(&self, params: &[String], body: &str) -> String {
        // let params: Vec<String> = (0..params_count).map(|i| format!("arg{}", i)).collect();
        format!("mihamaCurry(({}) => {})", params.join(", "), body)
    }

    fn create_type_constructor(&self, name: &str, params: &[String]) -> String {
        if params.is_empty() {
            format!("createMihamaType(\"{}\")", name)
        } else {
            self.curry_function(
                params,
                &format!("createMihamaType(\"{}<{}>\")", name, params.join(",")),
            )
        }
    }

    fn create_value_constructor(&self, tag: &str, value: Option<&str>) -> String {
        match value {
            Some(val) => format!("createMihamaValue(\"{}\", {})", tag, val),
            None => format!("createMihamaValue(\"{}\")", tag),
        }
    }
}
