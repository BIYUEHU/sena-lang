use mihama_core::parser::ast::{
    Expr as OldExpr, Literal as OldLiteral, MatchCase, Pattern, Stmt, TypeExpr, TypeVariant,
    TypeVariantFields, UnsafeProgram,
};
use ttt::{self, Kind, Type};

// ── TypeExpr → ttt::Type ──────────────────────────────────────────────────────
// 按你实际的 TypeExpr 结构填充，这里按常见形式猜

fn convert_type(te: TypeExpr) -> Type {
    match te {
        TypeExpr::Var(name) => Type::TyVar(name),

        TypeExpr::Con(name) => Type::Con(name),

        // App(f, [a, b, c]) → ((f a) b) c — 左结合柯里化
        TypeExpr::App(f, args) => args.into_iter().fold(convert_type(*f), |acc, arg| {
            Type::App(Box::new(acc), Box::new(convert_type(arg)))
        }),

        TypeExpr::Arrow(p, r) => Type::arrow(convert_type(*p), convert_type(*r)),

        // Forall([(a,k1),(b,k2)], body) → ∀(a:k1). ∀(b:k2). body
        TypeExpr::Forall(binders, body) => binders
            .into_iter()
            .rfold(convert_type(*body), |acc, (name, kind)| {
                Type::Forall(name, ast_kind_to_ttt(kind), Box::new(acc))
            }),

        // Lambda((param, param_kind), body) → TyAbs
        TypeExpr::Lambda((param, param_kind), body) => Type::TyAbs {
            param,
            kind: convert_kind(*param_kind),
            body: Box::new(convert_type(*body)),
        },

        // 字面量类型：F(1) 里的 1，ttt::Type 暂无对应，后续加 Type::Lit 再填
        TypeExpr::Literal(_) => todo!("TypeExpr::Literal has no ttt::Type representation yet"),

        // Kind 当 TypeExpr 用，只在签名位置出现，不进 ttt::Type
        TypeExpr::Kind(_) => {
            panic!("TypeExpr::Kind should not appear in value-level type position")
        }
    }
}

use mihama_core::parser::ast::Kind as AstKind;
fn ast_kind_to_ttt(k: AstKind) -> Kind {
    match k {
        AstKind::Star => Kind::Star,
        AstKind::Arrow(p, r) => Kind::arrow(ast_kind_to_ttt(*p), ast_kind_to_ttt(*r)),
    }
}

fn convert_kind(te: TypeExpr) -> Kind {
    match te {
        TypeExpr::Con(n) if n == "Type" => Kind::Star,
        TypeExpr::Arrow(p, r) => Kind::arrow(convert_kind(*p), convert_kind(*r)),
        TypeExpr::Kind(k) => ast_kind_to_ttt(k),
        _ => Kind::Star,
    }
}
fn convert_type_opt(te: Option<TypeExpr>) -> Option<Type> {
    te.map(convert_type)
}

// ── Literal ───────────────────────────────────────────────────────────────────

fn convert_literal(lit: OldLiteral) -> ttt::Literal {
    match lit {
        OldLiteral::String(s) => ttt::Literal::String(s),
        OldLiteral::Char(c) => ttt::Literal::Char(c),
        OldLiteral::Int(i) => ttt::Literal::Int(i),
        OldLiteral::Float(f) => ttt::Literal::Float(f),
        OldLiteral::Bool(b) => ttt::Literal::Bool(b),
        OldLiteral::Unit => ttt::Literal::Unit,
        OldLiteral::Array(_) => panic!("Array literal has no ttt representation"),
    }
}

// ── Expr ──────────────────────────────────────────────────────────────────────

fn convert_expr(expr: OldExpr) -> ttt::Expr {
    match expr {
        OldExpr::Literal(lit) => ttt::Expr::Literal(convert_literal(lit)),

        OldExpr::Ident(name) => ttt::Expr::Ident(name),

        // Prefix/Infix → Call (operator desugaring)
        OldExpr::Prefix(op, operand) => ttt::Expr::Call {
            callee: Box::new(ttt::Expr::Ident(op.to_string())),
            params: vec![convert_expr(*operand)],
        },
        OldExpr::Infix(op, lhs, rhs) => ttt::Expr::Call {
            callee: Box::new(ttt::Expr::Ident(op.to_string())),
            params: vec![convert_expr(*lhs), convert_expr(*rhs)],
        },

        OldExpr::Call { callee, params } => ttt::Expr::Call {
            callee: Box::new(convert_expr(*callee)),
            params: params.into_iter().map(convert_expr).collect(),
        },

        OldExpr::Function {
            params,
            body,
            return_type,
        } => ttt::Expr::Lambda {
            params: params
                .into_iter()
                .map(|(name, ann)| (name, convert_type_opt(ann)))
                .collect(),
            body: Box::new(convert_expr(*body)),
            return_type: convert_type_opt(return_type),
        },

        OldExpr::If {
            condition,
            then_branch,
            else_branch,
        } => ttt::Expr::If {
            condition: Box::new(convert_expr(*condition)),
            then_branch: Box::new(convert_expr(*then_branch)),
            else_branch: Box::new(convert_expr(*else_branch)),
        },

        OldExpr::LetIn {
            name,
            type_annotation,
            value,
            body,
        } => ttt::Expr::Let {
            name,
            ann: convert_type_opt(type_annotation),
            value: Box::new(convert_expr(*value)),
            body: Box::new(convert_expr(*body)),
        },

        // Block: 把 Vec<Stmt> 脱糖成嵌套 Let，末尾必须是 Expr stmt
        OldExpr::Block(stmts) => desugar_block(stmts),

        OldExpr::Match { .. } => todo!("Match has no ttt::Expr representation yet"),

        OldExpr::Internal(s) => ttt::Expr::Ident(format!("__internal_{}", s)),
    }
}

/// Block [Let a = 1, Let b = 2, Expr(body)]  →  Let { a, Let { b, body } }
fn desugar_block(stmts: Vec<Stmt>) -> ttt::Expr {
    // 收集，末尾必须是 Stmt::Expr 作为整个块的值
    let mut iter = stmts.into_iter().peekable();
    desugar_block_inner(&mut iter)
}

fn desugar_block_inner(iter: &mut std::iter::Peekable<impl Iterator<Item = Stmt>>) -> ttt::Expr {
    match iter.next() {
        None => panic!("Empty block"),
        Some(Stmt::Expr(e)) => {
            // 应该是最后一个
            convert_expr(e)
        }
        Some(Stmt::Let {
            name,
            type_annotation,
            value,
        }) => ttt::Expr::Let {
            name,
            ann: convert_type_opt(type_annotation),
            value: Box::new(convert_expr(*value)),
            body: Box::new(desugar_block_inner(iter)),
        },
        Some(other) => panic!(
            "Block contains non-let/expr statement: {:?}",
            std::mem::discriminant(&other)
        ),
    }
}

// ── TypeVariant → (ctor_name, field_types) ────────────────────────────────────

fn convert_variant(v: TypeVariant) -> (String, Vec<Type>) {
    let fields = match v.fields {
        TypeVariantFields::Tuple(tys) => tys.into_iter().map(convert_type).collect(),
        TypeVariantFields::Record(fields) => {
            fields.into_iter().map(|(_, ty)| convert_type(ty)).collect()
        }
        TypeVariantFields::Unit => vec![],
    };
    (v.name, fields)
}

// ── Stmt → Option<ttt::Statement> ────────────────────────────────────────────

fn convert_stmt(stmt: Stmt) -> Option<ttt::Statement> {
    match stmt {
        Stmt::Let {
            name,
            type_annotation,
            value,
        } => Some(ttt::Statement::Let {
            name,
            ann: convert_type_opt(type_annotation),
            value: convert_expr(*value),
        }),

        Stmt::Type {
            name,
            params,
            kind_annotation: _,
            variants,
        } => {
            // params: Vec<String> → Vec<(String, Kind)>，全部默认 Kind::Star
            // 如果 kind_annotation 是 Kind -> Kind 形式可以进一步解析，暂时忽略
            let ttt_params: Vec<(String, Kind)> =
                params.into_iter().map(|p| (p, Kind::Star)).collect();

            let constructors = variants.into_iter().map(convert_variant).collect();

            Some(ttt::Statement::Type {
                name,
                params: ttt_params,
                ann: None, // type alias 路径由调用方处理或后续 pass 补
                constructors,
            })
        }

        // Import/Export 不进入 type checker，直接丢掉
        Stmt::ImportAll { .. } | Stmt::ImportSome { .. } => None,

        Stmt::Export { body, .. } => convert_stmt(*body),

        // 裸表达式语句在顶层没有对应物，忽略
        Stmt::Expr(_) => None,
    }
}

// ── 入口 ──────────────────────────────────────────────────────────────────────

pub fn convert_program(program: UnsafeProgram) -> Vec<ttt::Statement> {
    program.into_iter().filter_map(convert_stmt).collect()
}
