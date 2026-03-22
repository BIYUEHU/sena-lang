//! System Fω — extends the λ2 implementation with:
//!
//! * **Kind system** (`Kind::Star`, `Kind::Arrow`)
//! * **`Type::TyAbs`** — type-level lambda `Λ(α: κ). τ`
//! * **`Type::normalize`** — beta-reduce type-level redexes before unification
//! * **`TypeRegistry`** — global registry of type declarations; pre-seeded with
//!   built-in primitive types
//! * **`KindEnv`** — kind environment threaded through kind/type checking
//! * **`resolve_type`** — post-parser pass: `Con("a")` → `TyVar("a")` for bound names
//! * **`Statement`** — top-level `let` and `type` declarations
//! * **`Interpreter`** — processes statements, growing the shared environments
//!
//! All existing λ2 tests pass unchanged.

#![allow(dead_code)]

use std::{
    collections::{HashMap, HashSet},
    fmt,
    mem::take,
};

// ==============================================================================
// Literals
// ==============================================================================

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    String(String),
    Char(char),
    Int(i64),
    Float(f64),
    Bool(bool),
    Unit,
}

// ==============================================================================
// Kind
// ==============================================================================

/// The kind of a type expression — "the type of types".
///
/// ```text
/// *           — proper type (can classify values)
/// * → *       — unary type constructor (e.g. Maybe)
/// * → * → *   — binary type constructor (e.g. Either)
/// ```
#[derive(Clone, PartialEq, Debug)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

impl Kind {
    pub fn star() -> Self {
        Kind::Star
    }

    pub fn arrow(k1: Kind, k2: Kind) -> Self {
        Kind::Arrow(Box::new(k1), Box::new(k2))
    }

    /// Build `* → * → … → *` (n arrows) for an n-parameter type constructor.
    ///
    /// `n_ary(0)` = `*`, `n_ary(1)` = `* → *`, `n_ary(2)` = `* → * → *`.
    pub fn n_ary(n: usize) -> Self {
        (0..n).fold(Kind::Star, |acc, _| {
            Kind::Arrow(Box::new(Kind::Star), Box::new(acc))
        })
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Kind::Star => write!(f, "*"),
            Kind::Arrow(k1, k2) => match k1.as_ref() {
                Kind::Arrow(_, _) => write!(f, "({}) → {}", k1, k2),
                _ => write!(f, "{} → {}", k1, k2),
            },
        }
    }
}

// ==============================================================================
// Types
// ==============================================================================

/// Type AST for System Fω.
///
/// Two distinct notions of "type variable":
/// * `Var(usize)` — unification variable, internal to inference, substituted away.
/// * `TyVar(String)` — rigid named variable, bound by `∀` or `Λ` (never unified).
///
/// Free `TyVar`s whose kind is not in the `KindEnv` are assumed to have kind `*`
/// (open-term friendly; strict mode can be added later).
#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Con(String),
    Var(usize),
    TyVar(String),
    App(Box<Type>, Box<Type>),
    Arrow(Box<Type>, Box<Type>),
    Forall(String, Box<Type>),
    /// **λω** — type-level lambda: `Λ(param : kind). body`.
    /// Has kind `kind → kind(body)`.
    TyAbs {
        param: String,
        kind: Kind,
        body: Box<Type>,
    },
}

// ── Smart constructors ─────────────────────────────────────────────────────────

macro_rules! gen_type_constructors {
    ($($name:ident),* $(,)?) => {
        impl Type {
            $(
                pub fn $name() -> Self {
                    let mut chars = stringify!($name).chars();
                    Type::Con(format!(
                        "{}{}",
                        chars.next().unwrap().to_uppercase(),
                        chars.collect::<String>()
                    ))
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
    pub fn forall(var: impl Into<String>, body: Type) -> Type {
        Type::Forall(var.into(), Box::new(body))
    }
    pub fn app(self, arg: Type) -> Type {
        Type::App(Box::new(self), Box::new(arg))
    }
    pub fn ty_var(name: impl Into<String>) -> Type {
        Type::TyVar(name.into())
    }
    pub fn ty_abs(param: impl Into<String>, kind: Kind, body: Type) -> Type {
        Type::TyAbs {
            param: param.into(),
            kind,
            body: Box::new(body),
        }
    }

    // ── Substitution ──────────────────────────────────────────────────────────

    /// Substitute all free occurrences of named type variable `var` with
    /// `replacement`, respecting shadowing in `∀` and `Λ` binders.
    pub fn subst_ty(&self, var: &str, replacement: &Type) -> Type {
        match self {
            Type::TyVar(n) if n == var => replacement.clone(),
            Type::TyVar(_) | Type::Con(_) | Type::Var(_) => self.clone(),
            Type::Arrow(p, r) => Type::Arrow(
                Box::new(p.subst_ty(var, replacement)),
                Box::new(r.subst_ty(var, replacement)),
            ),
            Type::App(t1, t2) => Type::App(
                Box::new(t1.subst_ty(var, replacement)),
                Box::new(t2.subst_ty(var, replacement)),
            ),
            Type::Forall(bound, body) => {
                if bound == var {
                    self.clone()
                } else {
                    Type::Forall(bound.clone(), Box::new(body.subst_ty(var, replacement)))
                }
            }
            Type::TyAbs { param, kind, body } => {
                if param == var {
                    self.clone()
                } else {
                    Type::TyAbs {
                        param: param.clone(),
                        kind: kind.clone(),
                        body: Box::new(body.subst_ty(var, replacement)),
                    }
                }
            }
        }
    }

    // ── Free variable sets ────────────────────────────────────────────────────

    pub fn free_ty_vars(&self) -> HashSet<String> {
        match self {
            Type::TyVar(n) => std::iter::once(n.clone()).collect(),
            Type::Con(_) | Type::Var(_) => HashSet::new(),
            Type::Arrow(p, r) => {
                let mut s = p.free_ty_vars();
                s.extend(r.free_ty_vars());
                s
            }
            Type::App(t1, t2) => {
                let mut s = t1.free_ty_vars();
                s.extend(t2.free_ty_vars());
                s
            }
            Type::Forall(b, body) => {
                let mut s = body.free_ty_vars();
                s.remove(b);
                s
            }
            Type::TyAbs { param, body, .. } => {
                let mut s = body.free_ty_vars();
                s.remove(param);
                s
            }
        }
    }

    pub fn free_unif_vars(&self) -> HashSet<usize> {
        match self {
            Type::Var(id) => std::iter::once(*id).collect(),
            Type::Con(_) | Type::TyVar(_) => HashSet::new(),
            Type::Arrow(p, r) => {
                let mut s = p.free_unif_vars();
                s.extend(r.free_unif_vars());
                s
            }
            Type::App(t1, t2) => {
                let mut s = t1.free_unif_vars();
                s.extend(t2.free_unif_vars());
                s
            }
            Type::Forall(_, body) | Type::TyAbs { body, .. } => body.free_unif_vars(),
        }
    }

    // ── Beta normalization ────────────────────────────────────────────────────

    /// Reduce all type-level beta redexes: `(Λα. τ) σ → τ[σ/α]`.
    ///
    /// Called before unification so that type aliases (defined as `TyAbs`) are
    /// transparent to the unifier and `App(Ref, arg)` reduces correctly.
    pub fn normalize(self) -> Type {
        match self {
            Type::App(f, a) => {
                let f = f.normalize();
                let a = a.normalize();
                match f {
                    Type::TyAbs { param, body, .. } => body.subst_ty(&param, &a).normalize(),
                    _ => Type::App(Box::new(f), Box::new(a)),
                }
            }
            Type::Arrow(p, r) => Type::Arrow(Box::new(p.normalize()), Box::new(r.normalize())),
            Type::Forall(v, b) => Type::Forall(v, Box::new(b.normalize())),
            Type::TyAbs { param, kind, body } => Type::TyAbs {
                param,
                kind,
                body: Box::new(body.normalize()),
            },
            other => other,
        }
    }
}

// ── resolve_type ──────────────────────────────────────────────────────────────

/// Post-parser pass: convert `Con(name)` → `TyVar(name)` for names in `bound`.
///
/// The parser cannot distinguish a type variable from a type constant
/// syntactically, so it emits everything as `Con`. Call this function on all
/// `Type` values produced by the parser before handing them to the type checker.
/// `bound` grows as we descend into `∀` and `Λ` binders.
///
/// When using *explicit* forall parameter lists (e.g. `∀[a, b]. a → b → a`),
/// initialise `bound` with those names before calling.
pub fn resolve_type(ty: Type, bound: &HashSet<String>) -> Type {
    match ty {
        Type::Con(ref n) if bound.contains(n.as_str()) => Type::TyVar(n.clone()),
        Type::Con(_) | Type::Var(_) | Type::TyVar(_) => ty,
        Type::Arrow(p, r) => Type::Arrow(
            Box::new(resolve_type(*p, bound)),
            Box::new(resolve_type(*r, bound)),
        ),
        Type::App(f, a) => Type::App(
            Box::new(resolve_type(*f, bound)),
            Box::new(resolve_type(*a, bound)),
        ),
        Type::Forall(var, body) => {
            let mut b = bound.clone();
            b.insert(var.clone());
            Type::Forall(var, Box::new(resolve_type(*body, &b)))
        }
        Type::TyAbs { param, kind, body } => {
            let mut b = bound.clone();
            b.insert(param.clone());
            Type::TyAbs {
                param,
                kind,
                body: Box::new(resolve_type(*body, &b)),
            }
        }
    }
}

// ── Display ───────────────────────────────────────────────────────────────────

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Con(n) => write!(f, "{}", n),
            Type::Var(id) => write!(f, "?{}", id),
            Type::TyVar(n) => write!(f, "{}", n),
            Type::App(func, arg) => write!(f, "({} {})", func, arg),
            Type::Arrow(p, r) => match p.as_ref() {
                Type::Arrow(_, _) | Type::Forall(_, _) | Type::TyAbs { .. } => {
                    write!(f, "({}) → {}", p, r)
                }
                _ => write!(f, "{} → {}", p, r),
            },
            Type::Forall(v, body) => write!(f, "∀{}. {}", v, body),
            Type::TyAbs { param, kind, body } => {
                write!(f, "Λ({}: {}). {}", param, kind, body)
            }
        }
    }
}

// ==============================================================================
// Type Registry
// ==============================================================================

/// A declared type in the global registry.
#[derive(Clone, Debug)]
pub enum TypeDecl {
    /// Algebraic data type.
    ///
    /// `params` — type-parameter names (each has kind `*` in ADT position).
    /// `constructors` — `(ctor_name, field_types)` with field types using
    /// `TyVar` for parameter references (produced by `resolve_type`).
    Adt {
        params: Vec<String>,
        constructors: Vec<(String, Vec<Type>)>,
    },
    /// Primitive / built-in type constant with an explicit kind.
    Primitive(Kind),
}

/// Global registry of type-level declarations.
///
/// Pre-seeded with all built-in primitive types (all `*`).
/// User-defined ADTs are added via `Statement::Type`.
#[derive(Clone, Debug)]
pub struct TypeRegistry {
    decls: HashMap<String, TypeDecl>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        let mut reg = Self {
            decls: HashMap::new(),
        };
        for name in &["Int", "Float", "Bool", "String", "Char", "Unit"] {
            reg.decls
                .insert(name.to_string(), TypeDecl::Primitive(Kind::Star));
        }
        reg
    }

    pub fn register(&mut self, name: String, decl: TypeDecl) {
        self.decls.insert(name, decl);
    }

    pub fn get(&self, name: &str) -> Option<&TypeDecl> {
        self.decls.get(name)
    }

    fn adt_kind(params: &[String]) -> Kind {
        Kind::n_ary(params.len())
    }

    // ── Kind inference ────────────────────────────────────────────────────────

    /// Infer the kind of a type expression in `kenv`.
    ///
    /// Free `TyVar`s not present in `kenv` are assumed kind `*` (open-term
    /// friendly — a strict mode can reject them if needed).
    pub fn kind_of(&self, ty: &Type, kenv: &KindEnv) -> Result<Kind, String> {
        match ty {
            // Type constant: look up registry.
            Type::Con(name) => match self.decls.get(name) {
                Some(TypeDecl::Primitive(k)) => Ok(k.clone()),
                Some(TypeDecl::Adt { params, .. }) => Ok(Self::adt_kind(params)),
                None => Err(format!("Unknown type constructor: '{}'", name)),
            },

            // Named type variable: look up kind env; assume * if free.
            Type::TyVar(name) => Ok(kenv.get(name).unwrap_or(Kind::Star)),

            // Unification variable: assumed *.
            Type::Var(_) => Ok(Kind::Star),

            // τ₁ → τ₂: both sides must be *, result is *.
            Type::Arrow(p, r) => {
                self.check_kind(p, &Kind::Star, kenv)?;
                self.check_kind(r, &Kind::Star, kenv)?;
                Ok(Kind::Star)
            }

            // ∀α. τ: α bound at kind *, body must be *, result is *.
            Type::Forall(var, body) => {
                let ext = kenv.extend(var.clone(), Kind::Star);
                self.check_kind(body, &Kind::Star, &ext)?;
                Ok(Kind::Star)
            }

            // f a: f must have kind k₁ → k₂, a must have kind k₁, result k₂.
            //
            // If f is a TyAbs, normalize first so the App reduces before kind
            // checking (handles inline type aliases).
            Type::App(f, a) => {
                // Try beta-reduce first (handles TyAbs on the left).
                let normalized = ty.clone().normalize();
                if normalized != *ty {
                    return self.kind_of(&normalized, kenv);
                }
                match self.kind_of(f, kenv)? {
                    Kind::Arrow(k1, k2) => {
                        self.check_kind(a, &k1, kenv)?;
                        Ok(*k2)
                    }
                    other => Err(format!(
                        "Kind mismatch in type application: \
                         left-hand side has kind '{}', expected an arrow kind\n  \
                         in: ({}) ({})",
                        other, f, a
                    )),
                }
            }

            // Λ(α: κ). τ: result kind is κ → kind(τ).
            Type::TyAbs { param, kind, body } => {
                let ext = kenv.extend(param.clone(), kind.clone());
                let body_kind = self.kind_of(body, &ext)?;
                Ok(Kind::Arrow(Box::new(kind.clone()), Box::new(body_kind)))
            }
        }
    }

    pub fn check_kind(&self, ty: &Type, expected: &Kind, kenv: &KindEnv) -> Result<(), String> {
        let actual = self.kind_of(ty, kenv)?;
        if actual == *expected {
            Ok(())
        } else {
            Err(format!(
                "Kind mismatch: expected '{}', got '{}'\n  in type: {}",
                expected, actual, ty
            ))
        }
    }

    // ── Constructor type synthesis ────────────────────────────────────────────

    /// Synthesise the polytype for a data constructor.
    ///
    /// ```text
    /// type Maybe a = Nothing | Just a
    /// → Nothing : ∀a. Maybe a
    /// → Just    : ∀a. a → Maybe a
    /// ```
    pub fn synthesize_ctor_type(adt_name: &str, params: &[String], fields: &[Type]) -> Type {
        // Applied result type: `((Con(name) a₁) a₂) … aₙ`
        let result_ty = params
            .iter()
            .fold(Type::Con(adt_name.to_string()), |acc, p| {
                Type::App(Box::new(acc), Box::new(Type::TyVar(p.to_string())))
            });
        // Arrow chain over fields.
        let inner = fields.iter().rfold(result_ty, |ret, field| {
            Type::Arrow(Box::new(field.clone()), Box::new(ret))
        });
        // Wrap in ∀ for each type parameter (outermost = first param).
        params.iter().rfold(inner, |body, param| {
            Type::Forall(param.clone(), Box::new(body))
        })
    }
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ==============================================================================
// Kind Environment
// ==============================================================================

/// Mapping from type-variable names to their kinds.
/// Immutable-extend style — no parent chain needed (always small).
#[derive(Clone, Debug, Default)]
pub struct KindEnv {
    bindings: HashMap<String, Kind>,
}

impl KindEnv {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn extend(&self, name: String, kind: Kind) -> Self {
        let mut new = self.clone();
        new.bindings.insert(name, kind);
        new
    }

    pub fn get(&self, name: &str) -> Option<Kind> {
        self.bindings.get(name).cloned()
    }
}

// ==============================================================================
// Expressions
// ==============================================================================

pub enum Expr {
    Ident(String),
    Literal(Literal),
    Lambda {
        params: Vec<(String, Option<Type>)>,
        body: Box<Expr>,
        return_type: Option<Type>,
    },
    Call {
        callee: Box<Expr>,
        params: Vec<Expr>,
    },
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    /// System F — type abstraction: `Λα. e`. Produces `∀α. T(e)`.
    TyLam {
        ty_param: String,
        body: Box<Expr>,
    },
    /// System F — type application: `e [τ]`. Requires `e : ∀α. σ`.
    TyApp {
        expr: Box<Expr>,
        ty: Box<Type>,
    },
    /// Let with generalization (inside expressions).
    Let {
        name: String,
        ann: Option<Type>,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    /// Type ascription: `(e : τ)`.
    Annot {
        expr: Box<Expr>,
        ty: Type,
    },
}

// ==============================================================================
// Type Environment
// ==============================================================================

#[derive(Clone, Debug)]
pub struct TypeEnv {
    bindings: HashMap<String, Type>,
    parent: Option<Box<TypeEnv>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn extend(parent: TypeEnv) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn insert(&mut self, name: String, ty: Type) {
        self.bindings.insert(name, ty);
    }

    pub fn get(&self, name: &str) -> Option<Type> {
        self.bindings
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.get(name)))
    }

    fn all_types(&self) -> Vec<Type> {
        let mut ts: Vec<Type> = self.bindings.values().cloned().collect();
        if let Some(p) = &self.parent {
            ts.extend(p.all_types());
        }
        ts
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

// ==============================================================================
// Inference State
// ==============================================================================

struct InferState {
    next_var: usize,
    next_skolem: usize,
    next_gen: usize,
    subs: HashMap<usize, Type>,
    constraints: Vec<(Type, Type)>,
}

impl InferState {
    fn new() -> Self {
        Self {
            next_var: 0,
            next_skolem: 0,
            next_gen: 0,
            subs: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        let id = self.next_var;
        self.next_var += 1;
        Type::Var(id)
    }

    fn fresh_skolem(&mut self) -> Type {
        let name = format!("$s{}", self.next_skolem);
        self.next_skolem += 1;
        Type::TyVar(name)
    }

    fn fresh_gen_name(&mut self) -> String {
        let idx = self.next_gen;
        self.next_gen += 1;
        let ch = (b'a' + (idx % 26) as u8) as char;
        if idx < 26 {
            ch.to_string()
        } else {
            format!("{}{}", ch, idx / 26)
        }
    }

    fn push_constraint(&mut self, t1: Type, t2: Type) {
        self.constraints.push((t1, t2));
    }

    fn apply_subs(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => self
                .subs
                .get(id)
                .map(|s| self.apply_subs(s))
                .unwrap_or_else(|| ty.clone()),
            Type::Arrow(p, r) => {
                Type::Arrow(Box::new(self.apply_subs(p)), Box::new(self.apply_subs(r)))
            }
            Type::App(t1, t2) => {
                Type::App(Box::new(self.apply_subs(t1)), Box::new(self.apply_subs(t2)))
            }
            Type::Forall(v, b) => Type::Forall(v.clone(), Box::new(self.apply_subs(b))),
            Type::TyAbs { param, kind, body } => Type::TyAbs {
                param: param.clone(),
                kind: kind.clone(),
                body: Box::new(self.apply_subs(body)),
            },
            _ => ty.clone(),
        }
    }

    /// Replace all leading `∀` binders with fresh unification variables.
    fn instantiate(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Forall(var, body) => {
                let fresh = self.fresh_var();
                self.instantiate(&body.subst_ty(var, &fresh))
            }
            _ => ty.clone(),
        }
    }

    /// Close a type over unification variables not free in the environment.
    fn generalize(&mut self, ty: &Type, env_free: &HashSet<usize>) -> Type {
        let ty = self.apply_subs(ty);
        let mut to_gen: Vec<usize> = ty.free_unif_vars().difference(env_free).copied().collect();
        to_gen.sort();
        if to_gen.is_empty() {
            return ty;
        }

        let assignments: Vec<(usize, String)> = to_gen
            .iter()
            .map(|&id| (id, self.fresh_gen_name()))
            .collect();
        let mut result = ty;
        for (var_id, name) in &assignments {
            result = Self::replace_unif_var(result, *var_id, &Type::TyVar(name.clone()));
        }
        for (_, name) in assignments.iter().rev() {
            result = Type::Forall(name.clone(), Box::new(result));
        }
        result
    }

    fn replace_unif_var(ty: Type, id: usize, rep: &Type) -> Type {
        match ty {
            Type::Var(vid) if vid == id => rep.clone(),
            Type::Var(_) | Type::Con(_) | Type::TyVar(_) => ty,
            Type::Arrow(p, r) => Type::Arrow(
                Box::new(Self::replace_unif_var(*p, id, rep)),
                Box::new(Self::replace_unif_var(*r, id, rep)),
            ),
            Type::App(t1, t2) => Type::App(
                Box::new(Self::replace_unif_var(*t1, id, rep)),
                Box::new(Self::replace_unif_var(*t2, id, rep)),
            ),
            Type::Forall(v, b) => Type::Forall(v, Box::new(Self::replace_unif_var(*b, id, rep))),
            Type::TyAbs { param, kind, body } => Type::TyAbs {
                param,
                kind,
                body: Box::new(Self::replace_unif_var(*body, id, rep)),
            },
        }
    }

    fn env_free_unif_vars(&self, env: &TypeEnv) -> HashSet<usize> {
        env.all_types()
            .iter()
            .flat_map(|ty| self.apply_subs(ty).free_unif_vars())
            .collect()
    }

    // ── Unification ──────────────────────────────────────────────────────────

    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), String> {
        // Apply substitutions and beta-normalize before comparing.
        let t1 = self.apply_subs(t1).normalize();
        let t2 = self.apply_subs(t2).normalize();

        match (&t1, &t2) {
            (Type::Con(a), Type::Con(b)) if a == b => Ok(()),

            (Type::TyVar(a), Type::TyVar(b)) if a == b => Ok(()),
            (Type::TyVar(a), Type::TyVar(b)) => Err(format!(
                "Cannot unify rigid type variables '{}' and '{}'",
                a, b
            )),

            (Type::Var(id), _) => self.bind_var(*id, &t2),
            (_, Type::Var(id)) => self.bind_var(*id, &t1),

            (Type::Arrow(p1, r1), Type::Arrow(p2, r2)) => {
                let (p1, r1, p2, r2) = (p1.clone(), r1.clone(), p2.clone(), r2.clone());
                self.unify(&p1, &p2)?;
                self.unify(&r1, &r2)
            }

            (Type::App(f1, a1), Type::App(f2, a2)) => {
                let (f1, a1, f2, a2) = (f1.clone(), a1.clone(), f2.clone(), a2.clone());
                self.unify(&f1, &f2)?;
                self.unify(&a1, &a2)
            }

            // ∀: unify bodies under a fresh Skolem.
            (Type::Forall(a, b1), Type::Forall(b, b2)) => {
                let sk = self.fresh_skolem();
                self.unify(&b1.subst_ty(a, &sk), &b2.subst_ty(b, &sk))
            }

            // Λ: kinds must match, then unify bodies under a fresh Skolem.
            (
                Type::TyAbs {
                    param: p1,
                    kind: k1,
                    body: b1,
                },
                Type::TyAbs {
                    param: p2,
                    kind: k2,
                    body: b2,
                },
            ) => {
                if k1 != k2 {
                    return Err(format!(
                        "Kind mismatch in TyAbs unification: {} vs {}",
                        k1, k2
                    ));
                }
                let sk = self.fresh_skolem();
                self.unify(&b1.subst_ty(p1, &sk), &b2.subst_ty(p2, &sk))
            }

            _ => Err(format!(
                "Type mismatch: cannot unify\n    {}\nwith\n    {}",
                t1, t2
            )),
        }
    }

    fn bind_var(&mut self, id: usize, ty: &Type) -> Result<(), String> {
        if let Type::Var(other) = ty {
            if *other == id {
                return Ok(());
            }
        }
        if let Some(existing) = self.subs.get(&id).cloned() {
            return self.unify(&existing, ty);
        }
        if self.occurs_in(id, ty) {
            return Err(format!("Occurs check: ?{} occurs in {}", id, ty));
        }
        self.subs.insert(id, ty.clone());
        Ok(())
    }

    fn occurs_in(&self, id: usize, ty: &Type) -> bool {
        match ty {
            Type::Var(other) => {
                if *other == id {
                    return true;
                }
                self.subs
                    .get(other)
                    .map(|s| self.occurs_in(id, &s.clone()))
                    .unwrap_or(false)
            }
            Type::Arrow(p, r) => self.occurs_in(id, p) || self.occurs_in(id, r),
            Type::App(t1, t2) => self.occurs_in(id, t1) || self.occurs_in(id, t2),
            Type::Forall(_, b) | Type::TyAbs { body: b, .. } => self.occurs_in(id, b),
            _ => false,
        }
    }

    fn solve_constraints(&mut self) -> Result<(), String> {
        for (t1, t2) in take(&mut self.constraints) {
            self.unify(&t1, &t2)?;
        }
        Ok(())
    }
}

// ==============================================================================
// Type Checker
// ==============================================================================

pub struct TypeChecker {
    pub env: TypeEnv,
    pub registry: TypeRegistry,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
            registry: TypeRegistry::new(),
        }
    }

    pub fn define(&mut self, name: impl Into<String>, ty: Type) {
        self.env.insert(name.into(), ty);
    }

    pub fn kind_of(&self, ty: &Type) -> Result<Kind, String> {
        self.registry.kind_of(ty, &KindEnv::new())
    }

    // ── Core inference ────────────────────────────────────────────────────────
    //
    // `kenv` grows when entering `TyLam` — it carries the kind of each type
    // parameter in scope so that type annotations referencing them kind-check.

    fn infer(
        expr: &Expr,
        env: &TypeEnv,
        state: &mut InferState,
        reg: &TypeRegistry,
        kenv: &KindEnv,
    ) -> Result<Type, String> {
        match expr {
            Expr::Literal(lit) => Self::infer_literal(lit),

            Expr::Ident(name) => {
                let ty = env
                    .get(name)
                    .ok_or_else(|| format!("Unbound variable: '{}'", name))?;
                Ok(state.instantiate(&ty))
            }

            Expr::Lambda {
                params,
                body,
                return_type,
            } => Self::infer_lambda(params, body, return_type, env, state, reg, kenv),

            Expr::Call { callee, params } => {
                Self::infer_call(callee, params, env, state, reg, kenv)
            }

            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => Self::infer_if(condition, then_branch, else_branch, env, state, reg, kenv),

            // Λα. e — bind α in kenv (kind *), infer body, wrap result in ∀.
            Expr::TyLam { ty_param, body } => {
                let ext = kenv.extend(ty_param.clone(), Kind::Star);
                let body_ty = Self::infer(body, env, state, reg, &ext)?;
                let body_ty = state.apply_subs(&body_ty);
                Ok(Type::Forall(ty_param.clone(), Box::new(body_ty)))
            }

            // e [τ] — type application.
            Expr::TyApp { expr, ty } => {
                reg.kind_of(ty, kenv)
                    .map_err(|e| format!("In type-application argument: {}", e))?;
                let expr_ty = Self::infer(expr, env, state, reg, kenv)?;
                match state.apply_subs(&expr_ty).normalize() {
                    Type::Forall(var, body) => Ok(body.subst_ty(&var, ty).normalize()),
                    other => Err(format!("Type application requires ∀α. τ, got: {}", other)),
                }
            }

            Expr::Let {
                name,
                ann,
                value,
                body,
            } => Self::infer_let(name, ann.as_ref(), value, body, env, state, reg, kenv),

            // (e : τ) — ascription.
            Expr::Annot { expr, ty } => {
                reg.check_kind(ty, &Kind::Star, kenv)
                    .map_err(|e| format!("In type annotation: {}", e))?;
                let inferred = Self::infer(expr, env, state, reg, kenv)?;
                state.push_constraint(inferred, ty.clone());
                Ok(ty.clone())
            }
        }
    }

    fn infer_literal(lit: &Literal) -> Result<Type, String> {
        Ok(match lit {
            Literal::Int(_) => Type::int(),
            Literal::Float(_) => Type::float(),
            Literal::Bool(_) => Type::bool(),
            Literal::String(_) => Type::string(),
            Literal::Char(_) => Type::char(),
            Literal::Unit => Type::unit(),
        })
    }

    fn infer_lambda(
        params: &[(String, Option<Type>)],
        body: &Expr,
        return_type: &Option<Type>,
        env: &TypeEnv,
        state: &mut InferState,
        reg: &TypeRegistry,
        kenv: &KindEnv,
    ) -> Result<Type, String> {
        let mut new_env = TypeEnv::extend(env.clone());

        let param_tys = params
            .iter()
            .map(|(name, ann)| {
                let ty = if let Some(ann_ty) = ann {
                    reg.check_kind(ann_ty, &Kind::Star, kenv)
                        .map_err(|e| format!("Parameter '{}': {}", name, e))?;
                    ann_ty.clone()
                } else {
                    state.fresh_var()
                };
                new_env.insert(name.clone(), ty.clone());
                Ok(ty)
            })
            .collect::<Result<Vec<_>, String>>()?;

        let body_ty = Self::infer(body, &new_env, state, reg, kenv)?;

        if let Some(ret) = return_type {
            reg.check_kind(ret, &Kind::Star, kenv)
                .map_err(|e| format!("Return type annotation: {}", e))?;
            state.push_constraint(body_ty.clone(), ret.clone());
        }

        Ok(param_tys
            .into_iter()
            .rfold(body_ty, |ret, p| Type::Arrow(Box::new(p), Box::new(ret))))
    }

    fn infer_call(
        callee: &Expr,
        params: &[Expr],
        env: &TypeEnv,
        state: &mut InferState,
        reg: &TypeRegistry,
        kenv: &KindEnv,
    ) -> Result<Type, String> {
        let mut cur = Self::infer(callee, env, state, reg, kenv)?;
        for param in params {
            let param_ty = Self::infer(param, env, state, reg, kenv)?;
            let ret_ty = state.fresh_var();
            state.push_constraint(
                cur,
                Type::Arrow(Box::new(param_ty), Box::new(ret_ty.clone())),
            );
            cur = ret_ty;
        }
        Ok(cur)
    }

    fn infer_if(
        condition: &Expr,
        then_branch: &Expr,
        else_branch: &Expr,
        env: &TypeEnv,
        state: &mut InferState,
        reg: &TypeRegistry,
        kenv: &KindEnv,
    ) -> Result<Type, String> {
        let cond_ty = Self::infer(condition, env, state, reg, kenv)?;
        state.push_constraint(cond_ty, Type::bool());
        let then_ty = Self::infer(then_branch, env, state, reg, kenv)?;
        let else_ty = Self::infer(else_branch, env, state, reg, kenv)?;
        state.push_constraint(then_ty.clone(), else_ty);
        Ok(then_ty)
    }

    fn infer_let(
        name: &str,
        ann: Option<&Type>,
        value: &Expr,
        body: &Expr,
        env: &TypeEnv,
        state: &mut InferState,
        reg: &TypeRegistry,
        kenv: &KindEnv,
    ) -> Result<Type, String> {
        let outer = take(&mut state.constraints);

        let poly_ty = if let Some(ann_ty) = ann {
            reg.check_kind(ann_ty, &Kind::Star, kenv)
                .map_err(|e| format!("In let annotation for '{}': {}", name, e))?;
            // Instantiate the annotation before constraining (fixes the
            // `∀a. a → a  ~  ?0 → ?0` mismatch from the λ2 version).
            let inst = state.instantiate(ann_ty);
            let inferred = Self::infer(value, env, state, reg, kenv)?;
            state.push_constraint(inferred, inst);
            state.solve_constraints()?;
            ann_ty.clone()
        } else {
            let inferred = Self::infer(value, env, state, reg, kenv)?;
            state.solve_constraints()?;
            let env_free = state.env_free_unif_vars(env);
            state.generalize(&inferred, &env_free)
        };

        state.constraints = outer;
        let mut new_env = TypeEnv::extend(env.clone());
        new_env.insert(name.to_string(), poly_ty);
        Self::infer(body, &new_env, state, reg, kenv)
    }

    // ── Public API ────────────────────────────────────────────────────────────

    pub fn type_of(&self, expr: &Expr) -> Result<Type, String> {
        let mut state = InferState::new();
        let ty = Self::infer(expr, &self.env, &mut state, &self.registry, &KindEnv::new())?;
        state.solve_constraints()?;
        Ok(state.apply_subs(&ty))
    }

    pub fn check(&self, expr: &Expr, expected: &Type) -> Result<(), String> {
        let mut state = InferState::new();
        let actual = Self::infer(expr, &self.env, &mut state, &self.registry, &KindEnv::new())?;
        state.push_constraint(actual, expected.clone());
        state.solve_constraints()
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

// ==============================================================================
// Statement
// ==============================================================================

/// A top-level declaration.
pub enum Statement {
    /// `let name [: ann] = value`
    ///
    /// The value expression is type-checked in the current environment. The
    /// resulting (possibly generalised) type is added for subsequent statements.
    Let {
        name: String,
        ann: Option<Type>,
        value: Expr,
    },
    /// `type Name a b … = Ctor₁ T… | Ctor₂ T… | …`
    ///
    /// * Registers the ADT in `TypeRegistry`.
    /// * Injects each constructor's polytype into the value `TypeEnv`.
    ///
    /// Field types may use `Con("a")` for type parameters — `resolve_type` is
    /// applied automatically with the declared parameter names as the bound set.
    Type {
        name: String,
        params: Vec<String>,
        constructors: Vec<(String, Vec<Type>)>,
    },
}

// ==============================================================================
// Interpreter
// ==============================================================================

/// Processes a sequence of top-level `Statement`s, growing the registry and
/// value environment incrementally.
pub struct Interpreter {
    pub checker: TypeChecker,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            checker: TypeChecker::new(),
        }
    }

    pub fn process(&mut self, stmt: Statement) -> Result<(), String> {
        match stmt {
            Statement::Type {
                name,
                params,
                constructors,
            } => self.process_type(name, params, constructors),
            Statement::Let { name, ann, value } => self.process_let(name, ann, value),
        }
    }

    fn process_type(
        &mut self,
        name: String,
        params: Vec<String>,
        constructors: Vec<(String, Vec<Type>)>,
    ) -> Result<(), String> {
        // Param names must be distinct.
        let mut seen = HashSet::new();
        for p in &params {
            if !seen.insert(p.clone()) {
                return Err(format!(
                    "Duplicate type parameter '{}' in type '{}'",
                    p, name
                ));
            }
        }

        // Kind env for checking field types: each param has kind *.
        let kenv = params
            .iter()
            .fold(KindEnv::new(), |e, p| e.extend(p.clone(), Kind::Star));
        // Bound set for resolve_type.
        let bound: HashSet<String> = params.iter().cloned().collect();

        // Resolve and kind-check constructor field types.
        let resolved_ctors = constructors
            .into_iter()
            .map(|(ctor_name, fields)| {
                let resolved_fields = fields
                    .into_iter()
                    .map(|field| {
                        let resolved = resolve_type(field, &bound);
                        if let Err(err) = self
                            .checker
                            .registry
                            .check_kind(&resolved, &Kind::Star, &kenv)
                            .map_err(|e| {
                                format!("Constructor '{}' of type '{}': {}", ctor_name, name, e)
                            })
                        {
                            return Err(err);
                        }
                        Ok(resolved)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok((ctor_name, resolved_fields))
            })
            .collect::<Result<Vec<_>, String>>()?;

        // Register the ADT.
        self.checker.registry.register(
            name.clone(),
            TypeDecl::Adt {
                params: params.clone(),
                constructors: resolved_ctors.clone(),
            },
        );

        // Inject constructor polytypes into the value environment.
        for (ctor_name, fields) in &resolved_ctors {
            let ctor_ty = TypeRegistry::synthesize_ctor_type(&name, &params, fields);
            self.checker.env.insert(ctor_name.clone(), ctor_ty);
        }

        Ok(())
    }

    fn process_let(&mut self, name: String, ann: Option<Type>, value: Expr) -> Result<(), String> {
        // Resolve and kind-check annotation if provided.
        let ann = ann
            .map(|ty| {
                let resolved = resolve_type(ty, &HashSet::new());
                if let Err(err) = self
                    .checker
                    .registry
                    .check_kind(&resolved, &Kind::Star, &KindEnv::new())
                    .map_err(|e| format!("Annotation for '{}': {}", name, e))
                {
                    return Err(err);
                }
                Ok(resolved)
            })
            .transpose()?;

        let mut state = InferState::new();
        let outer = take(&mut state.constraints);

        let poly_ty = if let Some(ref ann_ty) = ann {
            let inst = state.instantiate(ann_ty);
            let inferred = TypeChecker::infer(
                &value,
                &self.checker.env,
                &mut state,
                &self.checker.registry,
                &KindEnv::new(),
            )?;
            state.push_constraint(inferred, inst);
            state.solve_constraints()?;
            ann_ty.clone()
        } else {
            let inferred = TypeChecker::infer(
                &value,
                &self.checker.env,
                &mut state,
                &self.checker.registry,
                &KindEnv::new(),
            )?;
            state.solve_constraints()?;
            let env_free = state.env_free_unif_vars(&self.checker.env);
            state.generalize(&inferred, &env_free)
        };

        state.constraints = outer;
        self.checker.env.insert(name, poly_ty);
        Ok(())
    }

    // ── Query helpers ─────────────────────────────────────────────────────────

    pub fn type_of(&self, expr: &Expr) -> Result<Type, String> {
        self.checker.type_of(expr)
    }

    pub fn kind_of(&self, ty: &Type) -> Result<Kind, String> {
        self.checker.kind_of(ty)
    }

    pub fn type_of_name(&self, name: &str) -> Option<Type> {
        self.checker.env.get(name)
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

// ==============================================================================
// Tests — λ2 suite (all should pass unchanged)
// ==============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_types() {
        let c = TypeChecker::new();
        assert_eq!(
            c.type_of(&Expr::Literal(Literal::Int(42))).unwrap(),
            Type::int()
        );
        assert_eq!(
            c.type_of(&Expr::Literal(Literal::Bool(true))).unwrap(),
            Type::bool()
        );
        assert_eq!(
            c.type_of(&Expr::Literal(Literal::Unit)).unwrap(),
            Type::unit()
        );
    }

    #[test]
    fn test_identity_function() {
        let c = TypeChecker::new();
        let id = Expr::Lambda {
            params: vec![("x".to_string(), None)],
            body: Box::new(Expr::Ident("x".to_string())),
            return_type: None,
        };
        if let Type::Arrow(l, r) = c.type_of(&id).unwrap() {
            assert_eq!(l, r);
        } else {
            panic!("Expected arrow type");
        }
    }

    #[test]
    fn test_annotated_identity() {
        let c = TypeChecker::new();
        let id = Expr::Lambda {
            params: vec![("x".to_string(), Some(Type::int()))],
            body: Box::new(Expr::Ident("x".to_string())),
            return_type: Some(Type::int()),
        };
        assert_eq!(
            c.type_of(&id).unwrap(),
            Type::arrow(Type::int(), Type::int())
        );
    }

    #[test]
    fn test_function_application() {
        let c = TypeChecker::new();
        let app = Expr::Call {
            callee: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), Some(Type::int()))],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: Some(Type::int()),
            }),
            params: vec![Expr::Literal(Literal::Int(42))],
        };
        assert_eq!(c.type_of(&app).unwrap(), Type::int());
    }

    #[test]
    fn test_conditional() {
        let c = TypeChecker::new();
        let e = Expr::If {
            condition: Box::new(Expr::Literal(Literal::Bool(true))),
            then_branch: Box::new(Expr::Literal(Literal::Int(1))),
            else_branch: Box::new(Expr::Literal(Literal::Int(2))),
        };
        assert_eq!(c.type_of(&e).unwrap(), Type::int());
    }

    #[test]
    fn test_type_checking() {
        let c = TypeChecker::new();
        let e = Expr::Literal(Literal::Int(42));
        assert!(c.check(&e, &Type::int()).is_ok());
        assert!(c.check(&e, &Type::bool()).is_err());
    }

    #[test]
    fn test_type_mismatch() {
        let c = TypeChecker::new();
        let e = Expr::Call {
            callee: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), Some(Type::int()))],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            }),
            params: vec![Expr::Literal(Literal::Bool(true))],
        };
        assert!(c.type_of(&e).is_err());
    }

    #[test]
    fn test_unbound_variable() {
        let c = TypeChecker::new();
        assert!(c.type_of(&Expr::Ident("nope".to_string())).is_err());
    }

    #[test]
    fn test_branch_mismatch() {
        let c = TypeChecker::new();
        let e = Expr::If {
            condition: Box::new(Expr::Literal(Literal::Bool(true))),
            then_branch: Box::new(Expr::Literal(Literal::Int(1))),
            else_branch: Box::new(Expr::Literal(Literal::Bool(false))),
        };
        assert!(c.type_of(&e).is_err());
    }

    #[test]
    fn test_occurs_check() {
        let c = TypeChecker::new();
        let omega = Expr::Lambda {
            params: vec![("x".to_string(), None)],
            body: Box::new(Expr::Call {
                callee: Box::new(Expr::Ident("x".to_string())),
                params: vec![Expr::Ident("x".to_string())],
            }),
            return_type: None,
        };
        assert!(c.type_of(&omega).is_err());
    }

    #[test]
    fn test_nested_lambdas() {
        let c = TypeChecker::new();
        // K combinator: λa. λb. a
        let k = Expr::Lambda {
            params: vec![("a".to_string(), None)],
            body: Box::new(Expr::Lambda {
                params: vec![("b".to_string(), None)],
                body: Box::new(Expr::Ident("a".to_string())),
                return_type: None,
            }),
            return_type: None,
        };
        let ty = c.type_of(&k).unwrap();
        if let Type::Arrow(_, ret) = ty {
            assert!(matches!(*ret, Type::Arrow(_, _)));
        } else {
            panic!("Expected α → β → α");
        }
    }
}

// ==============================================================================
// Tests — System F (λ2) features
// ==============================================================================

#[cfg(test)]
mod system_f_tests {
    use super::*;

    fn poly_id() -> Expr {
        Expr::TyLam {
            ty_param: "α".to_string(),
            body: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), Some(Type::ty_var("α")))],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            }),
        }
    }

    #[test]
    fn test_ty_lam_produces_forall() {
        let c = TypeChecker::new();
        let ty = c.type_of(&poly_id()).unwrap();
        assert_eq!(
            ty,
            Type::Forall(
                "α".to_string(),
                Box::new(Type::Arrow(
                    Box::new(Type::TyVar("α".to_string())),
                    Box::new(Type::TyVar("α".to_string())),
                ))
            )
        );
    }

    #[test]
    fn test_ty_app_int() {
        let c = TypeChecker::new();
        let e = Expr::TyApp {
            expr: Box::new(poly_id()),
            ty: Box::new(Type::int()),
        };
        assert_eq!(
            c.type_of(&e).unwrap(),
            Type::arrow(Type::int(), Type::int())
        );
    }

    #[test]
    fn test_ty_app_on_monotype_fails() {
        let c = TypeChecker::new();
        let e = Expr::TyApp {
            expr: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), Some(Type::int()))],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            }),
            ty: Box::new(Type::int()),
        };
        assert!(c.type_of(&e).is_err());
    }

    #[test]
    fn test_forall_alpha_equivalent() {
        let c = TypeChecker::new();
        let expected = Type::Forall(
            "β".to_string(),
            Box::new(Type::Arrow(
                Box::new(Type::TyVar("β".to_string())),
                Box::new(Type::TyVar("β".to_string())),
            )),
        );
        assert!(c.check(&poly_id(), &expected).is_ok());
    }

    #[test]
    fn test_let_polymorphic_two_types() {
        let c = TypeChecker::new();
        // let id = λx. x in if id true then id 1 else id 2
        let e = Expr::Let {
            name: "id".to_string(),
            ann: None,
            value: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), None)],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            }),
            body: Box::new(Expr::If {
                condition: Box::new(Expr::Call {
                    callee: Box::new(Expr::Ident("id".to_string())),
                    params: vec![Expr::Literal(Literal::Bool(true))],
                }),
                then_branch: Box::new(Expr::Call {
                    callee: Box::new(Expr::Ident("id".to_string())),
                    params: vec![Expr::Literal(Literal::Int(1))],
                }),
                else_branch: Box::new(Expr::Call {
                    callee: Box::new(Expr::Ident("id".to_string())),
                    params: vec![Expr::Literal(Literal::Int(2))],
                }),
            }),
        };
        assert_eq!(c.type_of(&e).unwrap(), Type::int());
    }

    #[test]
    fn test_let_annotated() {
        let c = TypeChecker::new();
        // let id : ∀a. a → a = λx. x in id 99
        let e = Expr::Let {
            name: "id".to_string(),
            ann: Some(Type::forall(
                "a",
                Type::arrow(Type::ty_var("a"), Type::ty_var("a")),
            )),
            value: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), None)],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            }),
            body: Box::new(Expr::Call {
                callee: Box::new(Expr::Ident("id".to_string())),
                params: vec![Expr::Literal(Literal::Int(99))],
            }),
        };
        assert_eq!(c.type_of(&e).unwrap(), Type::int());
    }

    #[test]
    fn test_let_self_application_with_polymorphism() {
        let c = TypeChecker::new();
        // let id = λx. x in id id — requires let-polymorphism
        let e = Expr::Let {
            name: "id".to_string(),
            ann: None,
            value: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), None)],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            }),
            body: Box::new(Expr::Call {
                callee: Box::new(Expr::Ident("id".to_string())),
                params: vec![Expr::Ident("id".to_string())],
            }),
        };
        assert!(c.type_of(&e).is_ok());
    }
}

// ==============================================================================
// Tests — Kind system
// ==============================================================================

#[cfg(test)]
mod kind_tests {
    use super::*;

    fn reg() -> TypeRegistry {
        TypeRegistry::new()
    }
    fn kenv() -> KindEnv {
        KindEnv::new()
    }

    #[test]
    fn test_primitive_kinds() {
        let r = reg();
        assert_eq!(r.kind_of(&Type::int(), &kenv()).unwrap(), Kind::Star);
        assert_eq!(r.kind_of(&Type::bool(), &kenv()).unwrap(), Kind::Star);
        assert_eq!(r.kind_of(&Type::string(), &kenv()).unwrap(), Kind::Star);
    }

    #[test]
    fn test_arrow_kind() {
        let r = reg();
        let ty = Type::arrow(Type::int(), Type::bool());
        assert_eq!(r.kind_of(&ty, &kenv()).unwrap(), Kind::Star);
    }

    #[test]
    fn test_forall_kind_is_star() {
        let r = reg();
        let ty = Type::forall("a", Type::arrow(Type::ty_var("a"), Type::ty_var("a")));
        assert_eq!(r.kind_of(&ty, &kenv()).unwrap(), Kind::Star);
    }

    #[test]
    fn test_adt_kind_unary() {
        let mut r = reg();
        r.register(
            "Maybe".to_string(),
            TypeDecl::Adt {
                params: vec!["a".to_string()],
                constructors: vec![
                    ("Nothing".to_string(), vec![]),
                    ("Just".to_string(), vec![Type::ty_var("a")]),
                ],
            },
        );
        // Maybe alone: * → *
        assert_eq!(
            r.kind_of(&Type::Con("Maybe".to_string()), &kenv()).unwrap(),
            Kind::arrow(Kind::Star, Kind::Star)
        );
        // Maybe Int: *
        let applied = Type::App(
            Box::new(Type::Con("Maybe".to_string())),
            Box::new(Type::int()),
        );
        assert_eq!(r.kind_of(&applied, &kenv()).unwrap(), Kind::Star);
    }

    #[test]
    fn test_adt_kind_binary() {
        let mut r = reg();
        r.register(
            "Either".to_string(),
            TypeDecl::Adt {
                params: vec!["a".to_string(), "b".to_string()],
                constructors: vec![
                    ("Left".to_string(), vec![Type::ty_var("a")]),
                    ("Right".to_string(), vec![Type::ty_var("b")]),
                ],
            },
        );
        // Either: * → * → *
        assert_eq!(
            r.kind_of(&Type::Con("Either".to_string()), &kenv())
                .unwrap(),
            Kind::arrow(Kind::Star, Kind::arrow(Kind::Star, Kind::Star))
        );
    }

    #[test]
    fn test_ty_abs_kind() {
        let r = reg();
        // Λ(a: *). Maybe a  — but we need Maybe registered, so use a raw TyAbs:
        // Λ(a: *). a → a   has kind  * → *
        let ty = Type::ty_abs(
            "a",
            Kind::Star,
            Type::arrow(Type::ty_var("a"), Type::ty_var("a")),
        );
        assert_eq!(
            r.kind_of(&ty, &kenv()).unwrap(),
            Kind::arrow(Kind::Star, Kind::Star)
        );
    }

    #[test]
    fn test_ty_abs_applied_reduces_kind() {
        let r = reg();
        // (Λ(a: *). a → a) Int  normalises to  Int → Int  which has kind *
        let ty = Type::App(
            Box::new(Type::ty_abs(
                "a",
                Kind::Star,
                Type::arrow(Type::ty_var("a"), Type::ty_var("a")),
            )),
            Box::new(Type::int()),
        );
        assert_eq!(r.kind_of(&ty, &kenv()).unwrap(), Kind::Star);
    }

    #[test]
    fn test_wrong_kind_app_fails() {
        let r = reg();
        // Int Int — Int has kind *, not * → *, so application is ill-kinded
        let ty = Type::App(Box::new(Type::int()), Box::new(Type::int()));
        assert!(r.kind_of(&ty, &kenv()).is_err());
    }

    #[test]
    fn test_unknown_type_con_fails() {
        let r = reg();
        assert!(
            r.kind_of(&Type::Con("Banana".to_string()), &kenv())
                .is_err()
        );
    }

    #[test]
    fn test_kind_mismatch_in_annotation_fails() {
        let mut r = reg();
        r.register(
            "Maybe".to_string(),
            TypeDecl::Adt {
                params: vec!["a".to_string()],
                constructors: vec![],
            },
        );
        // Maybe alone (kind * → *) cannot annotate a lambda parameter (needs kind *)
        let ty = Type::Con("Maybe".to_string());
        assert!(r.check_kind(&ty, &Kind::Star, &kenv()).is_err());
    }
}

// ==============================================================================
// Tests — Type normalization
// ==============================================================================

#[cfg(test)]
mod normalize_tests {
    use super::*;

    #[test]
    fn test_ty_abs_beta_reduces() {
        // (Λa. a → a) Int  →  Int → Int
        let ty = Type::App(
            Box::new(Type::ty_abs(
                "a",
                Kind::Star,
                Type::arrow(Type::ty_var("a"), Type::ty_var("a")),
            )),
            Box::new(Type::int()),
        );
        assert_eq!(ty.normalize(), Type::arrow(Type::int(), Type::int()));
    }

    #[test]
    fn test_nested_reduction() {
        // (Λa. Λb. a) Int Bool  →  Int
        let ty = Type::App(
            Box::new(Type::App(
                Box::new(Type::ty_abs(
                    "a",
                    Kind::Star,
                    Type::ty_abs("b", Kind::Star, Type::ty_var("a")),
                )),
                Box::new(Type::int()),
            )),
            Box::new(Type::bool()),
        );
        assert_eq!(ty.normalize(), Type::int());
    }

    #[test]
    fn test_normalize_under_arrow() {
        // ((Λa. a) Int) → Bool  →  Int → Bool
        let ty = Type::Arrow(
            Box::new(Type::App(
                Box::new(Type::ty_abs("a", Kind::Star, Type::ty_var("a"))),
                Box::new(Type::int()),
            )),
            Box::new(Type::bool()),
        );
        assert_eq!(ty.normalize(), Type::arrow(Type::int(), Type::bool()));
    }

    #[test]
    fn test_normalize_idempotent() {
        // Already-normal type stays the same.
        let ty = Type::arrow(Type::int(), Type::bool());
        assert_eq!(ty.clone().normalize(), ty);
    }

    #[test]
    fn test_unify_normalizes_before_comparing() {
        // Two types that are beta-equivalent should unify.
        // (Λa. a) Int  ~  Int
        let mut state = InferState::new();
        let lhs = Type::App(
            Box::new(Type::ty_abs("a", Kind::Star, Type::ty_var("a"))),
            Box::new(Type::int()),
        );
        assert!(state.unify(&lhs, &Type::int()).is_ok());
    }
}

// ==============================================================================
// Tests — Statement / Interpreter
// ==============================================================================

#[cfg(test)]
mod statement_tests {
    use super::*;

    // ── Statement::Type ───────────────────────────────────────────────────────

    #[test]
    fn test_register_maybe() {
        let mut interp = Interpreter::new();
        interp
            .process(Statement::Type {
                name: "Maybe".to_string(),
                params: vec!["a".to_string()],
                constructors: vec![
                    ("Nothing".to_string(), vec![]),
                    // Parser would emit Con("a"); resolve_type converts it to TyVar("a").
                    ("Just".to_string(), vec![Type::Con("a".to_string())]),
                ],
            })
            .unwrap();

        // Registry should know Maybe.
        assert!(interp.checker.registry.get("Maybe").is_some());

        // Kind of Maybe: * → *
        assert_eq!(
            interp.kind_of(&Type::Con("Maybe".to_string())).unwrap(),
            Kind::arrow(Kind::Star, Kind::Star)
        );

        // Nothing : ∀a. Maybe a
        let nothing_ty = interp.type_of_name("Nothing").unwrap();
        let expected_nothing = Type::Forall(
            "a".to_string(),
            Box::new(Type::App(
                Box::new(Type::Con("Maybe".to_string())),
                Box::new(Type::TyVar("a".to_string())),
            )),
        );
        assert_eq!(nothing_ty, expected_nothing);

        // Just : ∀a. a → Maybe a
        let just_ty = interp.type_of_name("Just").unwrap();
        let expected_just = Type::Forall(
            "a".to_string(),
            Box::new(Type::Arrow(
                Box::new(Type::TyVar("a".to_string())),
                Box::new(Type::App(
                    Box::new(Type::Con("Maybe".to_string())),
                    Box::new(Type::TyVar("a".to_string())),
                )),
            )),
        );
        assert_eq!(just_ty, expected_just);
    }

    #[test]
    fn test_register_either() {
        let mut interp = Interpreter::new();
        interp
            .process(Statement::Type {
                name: "Either".to_string(),
                params: vec!["a".to_string(), "b".to_string()],
                constructors: vec![
                    ("Left".to_string(), vec![Type::Con("a".to_string())]),
                    ("Right".to_string(), vec![Type::Con("b".to_string())]),
                ],
            })
            .unwrap();

        // Either : * → * → *
        assert_eq!(
            interp.kind_of(&Type::Con("Either".to_string())).unwrap(),
            Kind::arrow(Kind::Star, Kind::arrow(Kind::Star, Kind::Star))
        );

        // Left : ∀a. ∀b. a → Either a b
        let left_ty = interp.type_of_name("Left").unwrap();
        let expected = Type::Forall(
            "a".to_string(),
            Box::new(Type::Forall(
                "b".to_string(),
                Box::new(Type::Arrow(
                    Box::new(Type::TyVar("a".to_string())),
                    Box::new(Type::App(
                        Box::new(Type::App(
                            Box::new(Type::Con("Either".to_string())),
                            Box::new(Type::TyVar("a".to_string())),
                        )),
                        Box::new(Type::TyVar("b".to_string())),
                    )),
                )),
            )),
        );
        assert_eq!(left_ty, expected);
    }

    #[test]
    fn test_invalid_field_type_fails() {
        let mut interp = Interpreter::new();
        // Field type Con("Banana") is not in registry → kind check fails.
        let result = interp.process(Statement::Type {
            name: "Bad".to_string(),
            params: vec![],
            constructors: vec![("MkBad".to_string(), vec![Type::Con("Banana".to_string())])],
        });
        assert!(result.is_err());
    }

    #[test]
    fn test_duplicate_param_fails() {
        let mut interp = Interpreter::new();
        let result = interp.process(Statement::Type {
            name: "Bad".to_string(),
            params: vec!["a".to_string(), "a".to_string()],
            constructors: vec![],
        });
        assert!(result.is_err());
    }

    // ── Statement::Let ────────────────────────────────────────────────────────

    #[test]
    fn test_let_statement_simple() {
        let mut interp = Interpreter::new();
        interp
            .process(Statement::Let {
                name: "answer".to_string(),
                ann: None,
                value: Expr::Literal(Literal::Int(42)),
            })
            .unwrap();

        // answer should have type Int.
        assert_eq!(interp.type_of_name("answer").unwrap(), Type::int());
    }

    #[test]
    fn test_let_statement_polymorphic() {
        let mut interp = Interpreter::new();
        // let id = λx. x  — should be generalised to ∀a. a → a
        interp
            .process(Statement::Let {
                name: "id".to_string(),
                ann: None,
                value: Expr::Lambda {
                    params: vec![("x".to_string(), None)],
                    body: Box::new(Expr::Ident("x".to_string())),
                    return_type: None,
                },
            })
            .unwrap();

        let id_ty = interp.type_of_name("id").unwrap();
        // Should be a Forall.
        assert!(
            matches!(id_ty, Type::Forall(_, _)),
            "Expected ∀α. α → α, got {:?}",
            id_ty
        );

        // Using id at two different types in a subsequent expression.
        let expr = Expr::If {
            condition: Box::new(Expr::Call {
                callee: Box::new(Expr::Ident("id".to_string())),
                params: vec![Expr::Literal(Literal::Bool(true))],
            }),
            then_branch: Box::new(Expr::Call {
                callee: Box::new(Expr::Ident("id".to_string())),
                params: vec![Expr::Literal(Literal::Int(1))],
            }),
            else_branch: Box::new(Expr::Literal(Literal::Int(0))),
        };
        assert_eq!(interp.type_of(&expr).unwrap(), Type::int());
    }

    #[test]
    fn test_let_statement_with_annotation() {
        let mut interp = Interpreter::new();
        interp
            .process(Statement::Let {
                name: "id".to_string(),
                ann: Some(Type::forall(
                    "a",
                    Type::arrow(Type::ty_var("a"), Type::ty_var("a")),
                )),
                value: Expr::Lambda {
                    params: vec![("x".to_string(), None)],
                    body: Box::new(Expr::Ident("x".to_string())),
                    return_type: None,
                },
            })
            .unwrap();

        // Applying id to an Int should give Int.
        let expr = Expr::Call {
            callee: Box::new(Expr::Ident("id".to_string())),
            params: vec![Expr::Literal(Literal::Int(7))],
        };
        assert_eq!(interp.type_of(&expr).unwrap(), Type::int());
    }

    #[test]
    fn test_let_statement_annotation_mismatch_fails() {
        let mut interp = Interpreter::new();
        let result = interp.process(Statement::Let {
            name: "x".to_string(),
            ann: Some(Type::int()),
            value: Expr::Literal(Literal::Bool(true)), // Bool ≠ Int
        });
        assert!(result.is_err());
    }

    // ── Interaction between statements ────────────────────────────────────────

    #[test]
    fn test_constructor_used_in_expression() {
        let mut interp = Interpreter::new();

        // Register Maybe.
        interp
            .process(Statement::Type {
                name: "Maybe".to_string(),
                params: vec!["a".to_string()],
                constructors: vec![
                    ("Nothing".to_string(), vec![]),
                    ("Just".to_string(), vec![Type::Con("a".to_string())]),
                ],
            })
            .unwrap();

        // Just 42  should give  Maybe Int
        let expr = Expr::Call {
            callee: Box::new(Expr::Ident("Just".to_string())),
            params: vec![Expr::Literal(Literal::Int(42))],
        };
        let ty = interp.type_of(&expr).unwrap();
        let expected = Type::App(
            Box::new(Type::Con("Maybe".to_string())),
            Box::new(Type::int()),
        );
        assert_eq!(ty, expected);
    }

    #[test]
    fn test_multiple_statements_in_sequence() {
        let mut interp = Interpreter::new();

        interp
            .process(Statement::Let {
                name: "forty_two".to_string(),
                ann: None,
                value: Expr::Literal(Literal::Int(42)),
            })
            .unwrap();

        interp
            .process(Statement::Let {
                name: "is_answer".to_string(),
                ann: None,
                // Use forty_two — it should be in scope.
                value: Expr::Annot {
                    expr: Box::new(Expr::Ident("forty_two".to_string())),
                    ty: Type::int(),
                },
            })
            .unwrap();

        assert_eq!(interp.type_of_name("is_answer").unwrap(), Type::int());
    }
}

// ==============================================================================
// Tests — resolve_type
// ==============================================================================

#[cfg(test)]
mod resolve_tests {
    use super::*;

    #[test]
    fn test_resolve_forall_body() {
        // ∀a. a → Int  — the parser emits Con("a") for the bound variable.
        let ty = Type::Forall(
            "a".to_string(),
            Box::new(Type::Arrow(
                Box::new(Type::Con("a".to_string())), // Con, not TyVar
                Box::new(Type::Con("Int".to_string())),
            )),
        );
        let resolved = resolve_type(ty, &HashSet::new());
        assert_eq!(
            resolved,
            Type::Forall(
                "a".to_string(),
                Box::new(Type::Arrow(
                    Box::new(Type::TyVar("a".to_string())), // now TyVar ✓
                    Box::new(Type::Con("Int".to_string())),
                )),
            )
        );
    }

    #[test]
    fn test_resolve_does_not_affect_outer_con() {
        // Con("Int") outside any binder stays Con("Int").
        let ty = Type::Con("Int".to_string());
        assert_eq!(resolve_type(ty.clone(), &HashSet::new()), ty);
    }

    #[test]
    fn test_resolve_shadowing() {
        // ∀a. (∀a. a) → a  — inner `a` and outer `a` are distinct binders.
        let ty = Type::Forall(
            "a".to_string(),
            Box::new(Type::Arrow(
                Box::new(Type::Forall(
                    "a".to_string(),
                    Box::new(Type::Con("a".to_string())),
                )),
                Box::new(Type::Con("a".to_string())),
            )),
        );
        let resolved = resolve_type(ty, &HashSet::new());
        // Both inner and outer `a` should become TyVar("a").
        if let Type::Forall(_, outer_body) = resolved {
            if let Type::Arrow(inner_forall, outer_a) = *outer_body {
                assert_eq!(*outer_a, Type::TyVar("a".to_string()));
                if let Type::Forall(_, inner_body) = *inner_forall {
                    assert_eq!(*inner_body, Type::TyVar("a".to_string()));
                } else {
                    panic!("Expected inner Forall");
                }
            } else {
                panic!("Expected Arrow");
            }
        } else {
            panic!("Expected outer Forall");
        }
    }

    #[test]
    fn test_resolve_with_explicit_bound_set() {
        // Simulates a parser that collected explicit ∀[a, b] params before body.
        let body = Type::Arrow(
            Box::new(Type::Con("a".to_string()).app(Type::Con("b".to_string()))),
            Box::new(Type::Con("Int".to_string())),
        );
        let bound: HashSet<String> = vec!["a".to_string(), "b".to_string()].into_iter().collect();
        let resolved = resolve_type(body, &bound);
        if let Type::Arrow(app, _) = resolved {
            if let Type::App(f, a) = *app {
                assert_eq!(*f, Type::TyVar("a".to_string()));
                assert_eq!(*a, Type::TyVar("b".to_string()));
            } else {
                panic!("Expected App");
            }
        } else {
            panic!("Expected Arrow");
        }
    }
}
