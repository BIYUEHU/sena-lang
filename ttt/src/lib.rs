use std::{collections::HashMap, mem::take};

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    String(String),
    Char(char),
    Int(i64),
    Float(f64),
    Bool(bool),
    Unit,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Con(String),
    Var(usize),
    App(Box<Type>, Box<Type>),
    Arrow(Box<Type>, Box<Type>),
    Forall(String, Box<Type>),
}

macro_rules! gen_type_constructors {
    ($($name:ident),* $(,)?) => {
        impl Type {
            $(
                pub fn $name() -> Self {
                    let mut chars_iter = stringify!($name).chars().clone();
                    Type::Con(format!("{}{}", chars_iter.next().unwrap().to_uppercase(), chars_iter.collect::<String>()))
                }
            )*
        }
    };
}

gen_type_constructors!(int, float, bool, string, char, unit);

impl Type {
    pub fn arrow(arg: Type, ret: Type) -> Type {
        Type::Arrow(Box::new(arg), Box::new(ret))
    }

    pub fn forall(var: String, ty: Type) -> Type {
        Type::Forall(var, Box::new(ty))
    }

    pub fn app(self, arg: Type) -> Type {
        Type::App(Box::new(self), Box::new(arg))
    }
}

pub enum Expr {
    Ident(String),
    Call {
        callee: Box<Expr>,
        params: Vec<Expr>,
    },
    Lambda {
        params: Vec<(String, Option<Type>)>,
        body: Box<Expr>,
        return_type: Option<Type>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Literal(Literal),
}
#[derive(Clone, Debug)]
struct TypeEnv {
    bindings: HashMap<String, Type>,
    parent: Option<Box<TypeEnv>>,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    fn extend(parent: TypeEnv) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    fn insert(&mut self, name: String, ty: Type) {
        self.bindings.insert(name, ty);
    }

    fn get(&self, name: &str) -> Option<Type> {
        self.bindings
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }
}

struct InferState {
    next_var_id: usize,
    substitutions: HashMap<usize, Type>,
    constraints: Vec<(Type, Type)>,
}

impl InferState {
    fn new() -> Self {
        Self {
            next_var_id: 0,
            substitutions: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        let var = Type::Var(self.next_var_id);
        self.next_var_id += 1;
        var
    }

    fn add_constraint(&mut self, t1: Type, t2: Type) {
        self.constraints.push((t1, t2));
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), String> {
        let t1 = self.apply_substitutions(t1);
        let t2 = self.apply_substitutions(t2);

        match (&t1, &t2) {
            (Type::Con(n1), Type::Con(n2)) if n1 == n2 => Ok(()),
            (Type::Var(id), Type::Var(other_id)) if id == other_id => Ok(()),
            (Type::Var(id), t) | (t, Type::Var(id)) => self.bind_var(*id, &t),
            (Type::Arrow(p1, r1), Type::Arrow(p2, r2)) => {
                self.unify(p1, p2)?;
                self.unify(r1, r2)
            }
            (Type::App(t11, t12), Type::App(t21, t22)) => {
                self.unify(t11, t21)?;
                self.unify(t12, t22)
            }
            _ => Err(format!(
                "Type mismatch: cannot unify {:?} with {:?}",
                t1, t2
            )),
        }
    }

    fn bind_var(&mut self, id: usize, ty: &Type) -> Result<(), String> {
        if let Some(existing) = self.substitutions.get(&id) {
            self.unify(&existing.clone(), ty)
        } else if self.occurs_in(id, ty) {
            Err(format!("Infinite type: {} occurs in {:?}", id, ty))
        } else {
            self.substitutions.insert(id, ty.clone());
            Ok(())
        }
    }

    // circular type check
    fn occurs_in(&self, id: usize, ty: &Type) -> bool {
        match ty {
            Type::Var(other_id) => *other_id == id,
            Type::Arrow(param, ret) => self.occurs_in(id, param) || self.occurs_in(id, ret),
            Type::App(t1, t2) => self.occurs_in(id, t1) || self.occurs_in(id, t2),
            Type::Forall(_, body) => self.occurs_in(id, body),
            _ => false,
        }
    }

    fn _instantiate_forall(&self, body: &Type, _fresh_ty: &Type) -> Type {
        // 这里简化处理：实际需要替换绑定变量
        // 为了简单，我们直接返回body，实际应该用fresh_ty替换绑定的类型变量
        body.clone()
    }

    fn apply_substitutions(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => {
                if let Some(subst) = self.substitutions.get(id) {
                    self.apply_substitutions(subst)
                } else {
                    ty.clone()
                }
            }
            Type::Arrow(param, ret) => Type::Arrow(
                Box::new(self.apply_substitutions(param)),
                Box::new(self.apply_substitutions(ret)),
            ),
            Type::App(t1, t2) => Type::App(
                Box::new(self.apply_substitutions(t1)),
                Box::new(self.apply_substitutions(t2)),
            ),
            Type::Forall(param, body) => {
                Type::Forall(param.clone(), Box::new(self.apply_substitutions(body)))
            }
            _ => ty.clone(),
        }
    }

    fn solve_constraints(&mut self) -> Result<(), String> {
        for (t1, t2) in take(&mut self.constraints) {
            self.unify(&t1, &t2)?;
        }
        Ok(())
    }
}

pub struct TypeChecker {
    env: TypeEnv,
}

impl TypeChecker {
    fn infer(expr: &Expr, env: &TypeEnv, state: &mut InferState) -> Result<Type, String> {
        match expr {
            Expr::Literal(literal) => Self::infer_literal(literal),
            Expr::Ident(name) => env
                .get(name)
                .ok_or_else(|| format!("Unbound variable: {}", name)),
            Expr::Lambda {
                params,
                body,
                return_type,
            } => Self::infer_lambda(params, body, return_type, env, state),
            Expr::Call { callee, params } => Self::infer_call(callee, params, env, state),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => Self::infer_if(condition, then_branch, else_branch, env, state),
        }
    }

    fn infer_literal(literal: &Literal) -> Result<Type, String> {
        match literal {
            Literal::Int(_) => Ok(Type::int()),
            Literal::Float(_) => Ok(Type::float()),
            Literal::Bool(_) => Ok(Type::bool()),
            Literal::String(_) => Ok(Type::string()),
            Literal::Char(_) => Ok(Type::char()),
            Literal::Unit => Ok(Type::unit()),
        }
    }

    fn infer_lambda(
        params: &[(String, Option<Type>)],
        body: &Expr,
        return_type: &Option<Type>,
        env: &TypeEnv,
        state: &mut InferState,
    ) -> Result<Type, String> {
        let mut new_env = TypeEnv::extend(env.clone());

        let param_types: Vec<Type> = params
            .iter()
            .map(|(name, ann_ty)| {
                let param_ty = ann_ty.clone().unwrap_or_else(|| state.fresh_var());
                new_env.insert(name.clone(), param_ty.clone());
                param_ty
            })
            .collect();

        let body_ty = Self::infer(body, &new_env, state)?;

        if let Some(return_ann) = return_type {
            state.add_constraint(body_ty.clone(), return_ann.clone());
        }

        let func_ty = param_types.into_iter().rfold(body_ty, |ret_ty, param_ty| {
            Type::Arrow(Box::new(param_ty), Box::new(ret_ty))
        });

        Ok(func_ty)
    }

    fn infer_call(
        callee: &Expr,
        params: &[Expr],
        env: &TypeEnv,
        state: &mut InferState,
    ) -> Result<Type, String> {
        let callee_ty = Self::infer(callee, env, state)?;
        let mut current_ty = callee_ty;

        for param in params {
            let param_ty = Self::infer(param, env, state)?;
            let ret_ty = state.fresh_var();
            let expected_ty = Type::Arrow(Box::new(param_ty), Box::new(ret_ty.clone()));

            state.add_constraint(current_ty, expected_ty);
            current_ty = ret_ty;
        }

        Ok(current_ty)
    }

    fn infer_if(
        condition: &Expr,
        then_branch: &Expr,
        else_branch: &Expr,
        env: &TypeEnv,
        state: &mut InferState,
    ) -> Result<Type, String> {
        // 推断条件类型
        let cond_ty = Self::infer(condition, env, state)?;
        state.add_constraint(cond_ty, Type::bool());

        // 推断两个分支的类型
        let then_ty = Self::infer(then_branch, env, state)?;
        let else_ty = Self::infer(else_branch, env, state)?;

        // 两个分支类型必须一致
        state.add_constraint(then_ty.clone(), else_ty);

        Ok(then_ty)
    }

    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
        }
    }

    pub fn type_of(&self, expr: &Expr) -> Result<Type, String> {
        let mut state = InferState::new();
        let ty = Self::infer(expr, &self.env, &mut state)?;
        state.solve_constraints()?;
        Ok(state.apply_substitutions(&ty))
    }

    pub fn check(&self, expr: &Expr, expected: &Type) -> Result<(), String> {
        let mut state = InferState::new();
        let actual = Self::infer(expr, &self.env, &mut state)?;
        state.add_constraint(actual, expected.clone());
        state.solve_constraints()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_types() {
        let checker = TypeChecker::new();

        assert_eq!(
            checker.type_of(&Expr::Literal(Literal::Int(42))).unwrap(),
            Type::int()
        );

        assert_eq!(
            checker
                .type_of(&Expr::Literal(Literal::Bool(true)))
                .unwrap(),
            Type::bool()
        );

        assert_eq!(
            checker.type_of(&Expr::Literal(Literal::Unit)).unwrap(),
            Type::unit()
        );
    }

    #[test]
    fn test_identity_function() {
        let checker = TypeChecker::new();

        let id_func = Expr::Lambda {
            params: vec![("x".to_string(), None)],
            body: Box::new(Expr::Ident("x".to_string())),
            return_type: None,
        };

        let ty = checker.type_of(&id_func).unwrap();
        // 对于 λx.x，应该得到 α → α 的形式
        if let Type::Arrow(left, right) = &ty {
            // 左右应该是相同的类型变量
            assert_eq!(left, right);
        } else {
            panic!("Expected arrow type, got {:?}", ty);
        }
    }

    #[test]
    fn test_annotated_identity_function() {
        let checker = TypeChecker::new();

        let id_func = Expr::Lambda {
            params: vec![("x".to_string(), Some(Type::int()))],
            body: Box::new(Expr::Ident("x".to_string())),
            return_type: Some(Type::int()),
        };

        let ty = checker.type_of(&id_func).unwrap();
        assert_eq!(
            ty,
            Type::Arrow(Box::new(Type::int()), Box::new(Type::int()))
        );
    }

    #[test]
    fn test_function_application() {
        let checker = TypeChecker::new();

        let app = Expr::Call {
            callee: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), Some(Type::int()))],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: Some(Type::int()),
            }),
            params: vec![Expr::Literal(Literal::Int(42))],
        };

        assert_eq!(checker.type_of(&app).unwrap(), Type::int());
    }

    #[test]
    fn test_conditional_expression() {
        let checker = TypeChecker::new();

        let if_expr = Expr::If {
            condition: Box::new(Expr::Literal(Literal::Bool(true))),
            then_branch: Box::new(Expr::Literal(Literal::Int(1))),
            else_branch: Box::new(Expr::Literal(Literal::Int(2))),
        };

        assert_eq!(checker.type_of(&if_expr).unwrap(), Type::int());
    }

    #[test]
    fn test_type_checking() {
        let checker = TypeChecker::new();

        let expr = Expr::Literal(Literal::Int(42));

        assert!(checker.check(&expr, &Type::int()).is_ok());
        assert!(checker.check(&expr, &Type::bool()).is_err());
    }

    #[test]
    fn test_type_mismatch_error() {
        let checker = TypeChecker::new();

        let bad_app = Expr::Call {
            callee: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), Some(Type::int()))],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            }),
            params: vec![Expr::Literal(Literal::Bool(true))],
        };

        assert!(checker.type_of(&bad_app).is_err());
    }

    #[test]
    fn test_unbound_variable_error() {
        let checker = TypeChecker::new();

        let unbound = Expr::Ident("nonexistent".to_string());

        assert!(checker.type_of(&unbound).is_err());
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    #[test]
    fn test_nested_lambdas() {
        let checker = TypeChecker::new();

        // λa. λb. a
        let nested = Expr::Lambda {
            params: vec![("a".to_string(), None)],
            body: Box::new(Expr::Lambda {
                params: vec![("b".to_string(), None)],
                body: Box::new(Expr::Ident("a".to_string())),
                return_type: None,
            }),
            return_type: None,
        };

        let result = checker.type_of(&nested);
        assert!(result.is_ok());
        // 应该得到 α → β → α
    }
}

#[cfg(test)]
mod error_tests {
    use super::*;

    #[test]
    fn test_branch_type_mismatch() {
        let checker = TypeChecker::new();

        let bad_if = Expr::If {
            condition: Box::new(Expr::Literal(Literal::Bool(true))),
            then_branch: Box::new(Expr::Literal(Literal::Int(1))),
            else_branch: Box::new(Expr::Literal(Literal::Bool(false))),
        };

        assert!(checker.type_of(&bad_if).is_err());
    }
}

#[cfg(test)]
mod basic_tests {
    use super::*;

    #[test]
    fn test_complex_expression() {
        let checker = TypeChecker::new();

        let complex = Expr::Call {
            callee: Box::new(Expr::Call {
                callee: Box::new(Expr::Lambda {
                    params: vec![("f".to_string(), None)],
                    body: Box::new(Expr::Lambda {
                        params: vec![("x".to_string(), None)],
                        body: Box::new(Expr::Call {
                            callee: Box::new(Expr::Ident("f".to_string())),
                            params: vec![Expr::Call {
                                callee: Box::new(Expr::Ident("f".to_string())),
                                params: vec![Expr::Ident("x".to_string())],
                            }],
                        }),
                        return_type: None,
                    }),
                    return_type: None,
                }),
                params: vec![Expr::Lambda {
                    params: vec![("y".to_string(), Some(Type::int()))],
                    body: Box::new(Expr::Ident("y".to_string())),
                    return_type: Some(Type::int()),
                }],
            }),
            params: vec![Expr::Literal(Literal::Int(0))],
        };

        let result = checker.type_of(&complex);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Type::int());
    }

    #[test]
    fn test_basic_literals() {
        let checker = TypeChecker::new();

        let tests = vec![
            (Literal::Int(42), Type::int()),
            (Literal::Bool(true), Type::bool()),
            (Literal::String("hello".to_string()), Type::string()),
            (Literal::Unit, Type::unit()),
        ];

        for (literal, expected) in tests {
            let expr = Expr::Literal(literal);
            assert_eq!(checker.type_of(&expr).unwrap(), expected);
        }
    }

    #[test]
    fn test_simple_let_binding() {
        let _checker = TypeChecker::new();

        // 这个测试假设你将来会实现 let 绑定
        // let x = 5 in x
        // 目前先跳过或者用其他方式测试
    }

    #[test]
    fn test_function_with_annotation() {
        let checker = TypeChecker::new();

        let expr = Expr::Lambda {
            params: vec![("x".to_string(), Some(Type::int()))],
            body: Box::new(Expr::Ident("x".to_string())),
            return_type: Some(Type::int()),
        };

        let result = checker.type_of(&expr);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Type::Arrow(Box::new(Type::int()), Box::new(Type::int()))
        );
    }
}
