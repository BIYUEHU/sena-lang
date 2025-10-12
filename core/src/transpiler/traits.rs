use crate::{
    checker::ast::{CheckedCase, CheckedExpr, CheckedStmt},
    parser::ast::Literal,
};

pub trait CodeGenerator {
    type Output;
    fn generate_program(&mut self, program: &[CheckedStmt]) -> Self::Output;
    fn generate_statement(&mut self, stmt: &CheckedStmt) -> String;
    fn generate_expression(&mut self, expr: &CheckedExpr) -> String;
    fn generate_literal(&mut self, literal: &Literal) -> String;
    fn generate_pattern_match(&mut self, expr: &CheckedExpr, cases: &[CheckedCase]) -> String;

    fn curry_function(&self, params: &[String], body: &str) -> String;
    fn create_type_constructor(&self, name: &str, params: &[String]) -> String;
    fn create_value_constructor(&self, tag: &str, value: Option<&str>) -> String;
}

pub trait Transpiler {
    type Generator: CodeGenerator;

    fn new() -> Self;
    fn transpile(&mut self, program: &[CheckedStmt]) -> <Self::Generator as CodeGenerator>::Output;
}
