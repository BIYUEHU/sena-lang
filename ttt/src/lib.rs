use std::{
    collections::{HashMap, HashSet},
    fmt,
    mem::take,
};

// ==================== Literals ====================

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    String(String),
    Char(char),
    Int(i64),
    Float(f64),
    Bool(bool),
    Unit,
}

// ==================== Types ====================

/// System F types.
///
/// Two distinct notions of "type variable" exist:
/// - `Var(usize)`: unification/inference variable (internal, assigned fresh during inference,
///   can be substituted via the substitution map).
/// - `TyVar(String)`: named, *rigid* type variable — bound by a `∀` binder or introduced by
///   a type abstraction `Λ`. These are *never* unified away; they can only unify with
///   themselves.
#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    /// Monomorphic type constant: `Int`, `Bool`, `String`, …
    Con(String),
    /// Unification/inference variable — internal, created fresh during inference.
    Var(usize),
    /// Named rigid type variable — bound by `∀` or introduced by `Λ`.
    TyVar(String),
    /// Type application: e.g. `Maybe Int` ≡ `App(Con("Maybe"), Con("Int"))`.
    App(Box<Type>, Box<Type>),
    /// Function type: `τ₁ → τ₂`.
    Arrow(Box<Type>, Box<Type>),
    /// Universal quantification: `∀α. τ`.
    Forall(String, Box<Type>),
}

// ---- Smart constructors ----

macro_rules! gen_type_constructors {
    ($($name:ident),* $(,)?) => {
        impl Type {
            $(
                pub fn $name() -> Self {
                    let mut chars = stringify!($name).chars();
                    Type::Con(format!("{}{}", chars.next().unwrap().to_uppercase(), chars.collect::<String>()))
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

    // ---- Substitution ----

    /// Substitute all free occurrences of the *named* type variable `var` with `replacement`.
    /// Respects shadowing: stops descending into `∀var. …`.
    pub fn subst_ty(&self, var: &str, replacement: &Type) -> Type {
        match self {
            Type::TyVar(name) if name == var => replacement.clone(),
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
                    self.clone() // shadowed — do not substitute
                } else {
                    Type::Forall(bound.clone(), Box::new(body.subst_ty(var, replacement)))
                }
            }
        }
    }

    // ---- Free variable analysis ----

    /// Free *named* type variables (rigid `TyVar`s not bound by any enclosing `∀`).
    pub fn free_ty_vars(&self) -> HashSet<String> {
        match self {
            Type::TyVar(name) => {
                let mut s = HashSet::new();
                s.insert(name.clone());
                s
            }
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
            Type::Forall(bound, body) => {
                let mut s = body.free_ty_vars();
                s.remove(bound);
                s
            }
        }
    }

    /// Free *unification* variables (`Var(usize)`).
    /// Traverses into `∀` binders — a `Var` is never bound by a `∀`.
    pub fn free_unif_vars(&self) -> HashSet<usize> {
        match self {
            Type::Var(id) => {
                let mut s = HashSet::new();
                s.insert(*id);
                s
            }
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
            Type::Forall(_, body) => body.free_unif_vars(),
        }
    }
}

// ---- Display ----

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Con(n) => write!(f, "{}", n),
            Type::Var(id) => write!(f, "?{}", id),
            Type::TyVar(n) => write!(f, "{}", n),
            Type::App(func, arg) => write!(f, "({} {})", func, arg),
            Type::Arrow(p, r) => match p.as_ref() {
                Type::Arrow(_, _) | Type::Forall(_, _) => write!(f, "({}) → {}", p, r),
                _ => write!(f, "{} → {}", p, r),
            },
            Type::Forall(v, body) => write!(f, "∀{}. {}", v, body),
        }
    }
}

// ==================== Expressions ====================

pub enum Expr {
    /// Variable reference.
    Ident(String),
    /// Literal value.
    Literal(Literal),
    /// Lambda abstraction: `λ(x₁ [: τ₁], …) [: τ_ret] → body`.
    Lambda {
        params: Vec<(String, Option<Type>)>,
        body: Box<Expr>,
        return_type: Option<Type>,
    },
    /// Function application: `f(a₁, a₂, …)` (curried internally).
    Call {
        callee: Box<Expr>,
        params: Vec<Expr>,
    },
    /// Conditional expression.
    If {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    /// **System F** — Type abstraction: `Λα. e`.
    /// Introduces a rigid type variable `α`; the whole expression has type `∀α. T(e)`.
    TyLam { ty_param: String, body: Box<Expr> },
    /// **System F** — Type application: `e [τ]`.
    /// Requires `e : ∀α. σ`; result type is `σ[τ/α]`.
    TyApp { expr: Box<Expr>, ty: Box<Type> },
    /// Let binding with generalization: `let name [: ann] = value in body`.
    /// The inferred (or annotated) type of `value` is *generalized* over free unification
    /// variables not present in the surrounding environment, yielding a polytype.
    Let {
        name: String,
        ann: Option<Type>,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    /// Type ascription / annotation: `(e : τ)`.
    /// Checks that `e` has type `τ`.
    Annot { expr: Box<Expr>, ty: Type },
}

// ==================== Type Environment ====================

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

    /// Extend the environment with a new child scope.
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

    /// All types bound in any scope (used to compute free unification vars in env).
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

// ==================== Inference State ====================

/// Mutable state threaded through type inference.
struct InferState {
    /// Counter for fresh unification variables.
    next_var: usize,
    /// Counter for fresh Skolem names (used when unifying `∀` types).
    next_skolem: usize,
    /// Counter for fresh generalization names (a, b, …, a1, b1, …).
    next_gen: usize,
    /// Substitution map: `Var(id)` ↦ `Type`.
    subs: HashMap<usize, Type>,
    /// Deferred unification constraints.
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

    // ---- Fresh variable factories ----

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

    // ---- Substitution application ----

    /// Walk a type through the current substitution map (follows chains).
    fn apply_subs(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) => {
                if let Some(sub) = self.subs.get(id) {
                    self.apply_subs(sub) // follow chains
                } else {
                    ty.clone()
                }
            }
            Type::Arrow(p, r) => {
                Type::Arrow(Box::new(self.apply_subs(p)), Box::new(self.apply_subs(r)))
            }
            Type::App(t1, t2) => {
                Type::App(Box::new(self.apply_subs(t1)), Box::new(self.apply_subs(t2)))
            }
            Type::Forall(v, body) => Type::Forall(v.clone(), Box::new(self.apply_subs(body))),
            _ => ty.clone(),
        }
    }

    // ---- Instantiation ----

    /// Replace all leading `∀`-bound variables with fresh unification vars.
    ///
    /// `∀a. ∀b. a → b`  →  `?n → ?m`
    fn instantiate(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Forall(var, body) => {
                let fresh = self.fresh_var();
                let inner = body.subst_ty(var, &fresh);
                self.instantiate(&inner)
            }
            _ => ty.clone(),
        }
    }

    // ---- Generalization ----

    /// Close a type over all free unification variables *not* present in the environment.
    ///
    /// Concretely: replaces each `Var(id)` not in `env_free` with a fresh named `TyVar`,
    /// then wraps the type in `∀` binders for each such variable.
    fn generalize(&mut self, ty: &Type, env_free: &HashSet<usize>) -> Type {
        let ty = self.apply_subs(ty);

        // Free unification variables in `ty` that are not free in the env.
        let mut to_gen: Vec<usize> = ty.free_unif_vars().difference(env_free).copied().collect();
        to_gen.sort(); // deterministic ordering

        if to_gen.is_empty() {
            return ty;
        }

        // Assign a fresh name to each variable being generalized.
        let assignments: Vec<(usize, String)> = to_gen
            .iter()
            .map(|&id| (id, self.fresh_gen_name()))
            .collect();

        // 1. Replace all Var(id) → TyVar(name) in the type.
        let mut result = ty;
        for (var_id, name) in &assignments {
            result = Self::replace_unif_var_with(result, *var_id, &Type::TyVar(name.clone()));
        }

        // 2. Wrap in ∀ binders, outermost = lowest id (reversed iteration).
        for (_, name) in assignments.iter().rev() {
            result = Type::Forall(name.clone(), Box::new(result));
        }

        result
    }

    fn replace_unif_var_with(ty: Type, id: usize, replacement: &Type) -> Type {
        match ty {
            Type::Var(vid) if vid == id => replacement.clone(),
            Type::Var(_) | Type::Con(_) | Type::TyVar(_) => ty,
            Type::Arrow(p, r) => Type::Arrow(
                Box::new(Self::replace_unif_var_with(*p, id, replacement)),
                Box::new(Self::replace_unif_var_with(*r, id, replacement)),
            ),
            Type::App(t1, t2) => Type::App(
                Box::new(Self::replace_unif_var_with(*t1, id, replacement)),
                Box::new(Self::replace_unif_var_with(*t2, id, replacement)),
            ),
            Type::Forall(v, body) => Type::Forall(
                v,
                Box::new(Self::replace_unif_var_with(*body, id, replacement)),
            ),
        }
    }

    /// Free unification variables across the entire type environment.
    fn env_free_unif_vars(&self, env: &TypeEnv) -> HashSet<usize> {
        env.all_types()
            .iter()
            .flat_map(|ty| self.apply_subs(ty).free_unif_vars())
            .collect()
    }

    // ==================== Unification ====================

    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), String> {
        let t1 = self.apply_subs(t1);
        let t2 = self.apply_subs(t2);

        match (&t1, &t2) {
            // ── Identical type constants ──────────────────────────────────────
            (Type::Con(a), Type::Con(b)) if a == b => Ok(()),

            // ── Identical rigid type variables ────────────────────────────────
            (Type::TyVar(a), Type::TyVar(b)) if a == b => Ok(()),

            // ── Two *different* rigid type variables cannot be unified ─────────
            // (skolem variables introduced during Forall-unification also live here)
            (Type::TyVar(a), Type::TyVar(b)) => Err(format!(
                "Cannot unify rigid type variables '{}' and '{}'",
                a, b
            )),

            // ── Unification variable on left ──────────────────────────────────
            (Type::Var(id), _) => {
                let id = *id;
                let rhs = t2.clone();
                self.bind_var(id, &rhs)
            }

            // ── Unification variable on right ─────────────────────────────────
            (_, Type::Var(id)) => {
                let id = *id;
                let lhs = t1.clone();
                self.bind_var(id, &lhs)
            }

            // ── Arrow: unify component-wise ───────────────────────────────────
            (Type::Arrow(p1, r1), Type::Arrow(p2, r2)) => {
                let (p1, r1, p2, r2) = (p1.clone(), r1.clone(), p2.clone(), r2.clone());
                self.unify(&p1, &p2)?;
                self.unify(&r1, &r2)
            }

            // ── Type application: unify component-wise ────────────────────────
            (Type::App(f1, a1), Type::App(f2, a2)) => {
                let (f1, a1, f2, a2) = (f1.clone(), a1.clone(), f2.clone(), a2.clone());
                self.unify(&f1, &f2)?;
                self.unify(&a1, &a2)
            }

            // ── Forall: unify under a fresh Skolem variable ───────────────────
            //
            // `∀α. τ₁ ≡ ∀β. τ₂`  iff  `τ₁[$s/α] ≡ τ₂[$s/β]`  for fresh Skolem `$s`.
            //
            // The Skolem is rigid (TyVar starting with '$s'), so if any unification
            // variable attempts to escape its scope by unifying with the Skolem,
            // the rigid-variable mismatch rule above will catch it.
            (Type::Forall(a, b1), Type::Forall(b, b2)) => {
                let skolem = self.fresh_skolem();
                let body1 = b1.subst_ty(a, &skolem);
                let body2 = b2.subst_ty(b, &skolem);
                self.unify(&body1, &body2)
            }

            // ── Everything else is a type mismatch ───────────────────────────
            _ => Err(format!(
                "Type mismatch: cannot unify\n    {}\nwith\n    {}",
                t1, t2
            )),
        }
    }

    fn bind_var(&mut self, id: usize, ty: &Type) -> Result<(), String> {
        // Trivial: binding a var to itself.
        if let Type::Var(other) = ty {
            if *other == id {
                return Ok(());
            }
        }

        // If we already have a substitution, unify against it.
        if let Some(existing) = self.subs.get(&id).cloned() {
            return self.unify(&existing, ty);
        }

        // Occurs check: prevents infinite/recursive types.
        if self.occurs_in(id, ty) {
            return Err(format!(
                "Occurs check failed: type variable ?{} occurs in {}",
                id, ty
            ));
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
                // Follow substitution chains.
                if let Some(sub) = self.subs.get(other) {
                    self.occurs_in(id, &sub.clone())
                } else {
                    false
                }
            }
            Type::Arrow(p, r) => self.occurs_in(id, p) || self.occurs_in(id, r),
            Type::App(t1, t2) => self.occurs_in(id, t1) || self.occurs_in(id, t2),
            Type::Forall(_, body) => self.occurs_in(id, body),
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

// ==================== Type Checker ====================

pub struct TypeChecker {
    pub env: TypeEnv,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            env: TypeEnv::new(),
        }
    }

    pub fn with_env(env: TypeEnv) -> Self {
        Self { env }
    }

    /// Register a global binding (builtins, primitives, etc.).
    pub fn define(&mut self, name: impl Into<String>, ty: Type) {
        self.env.insert(name.into(), ty);
    }

    // ==================== Core Inference ====================

    fn infer(expr: &Expr, env: &TypeEnv, state: &mut InferState) -> Result<Type, String> {
        match expr {
            Expr::Literal(lit) => Self::infer_literal(lit),

            // Variable: look up and *instantiate* any ∀ binders.
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
            } => Self::infer_lambda(params, body, return_type, env, state),

            Expr::Call { callee, params } => Self::infer_call(callee, params, env, state),

            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => Self::infer_if(condition, then_branch, else_branch, env, state),

            // ── System F: Type abstraction  Λα. e ─────────────────────────────
            //
            // Inference rule:
            //   Γ ⊢ e : τ   (α rigid in τ, not free in Γ)
            //   ───────────────────────────────────────────
            //   Γ ⊢ Λα. e : ∀α. τ
            //
            // Because `α` is represented as `Type::TyVar("α")` everywhere inside `e`'s
            // annotations, and TyVars are rigid (not unifiable), the inference naturally
            // treats it as a fixed, abstract type.
            Expr::TyLam { ty_param, body } => {
                let body_ty = Self::infer(body, env, state)?;
                let body_ty = state.apply_subs(&body_ty);
                Ok(Type::Forall(ty_param.clone(), Box::new(body_ty)))
            }

            // ── System F: Type application  e [τ] ────────────────────────────
            //
            // Inference rule:
            //   Γ ⊢ e : ∀α. σ
            //   ────────────────────────────
            //   Γ ⊢ e [τ] : σ[τ/α]
            //
            // Requires the expression to have a *syntactic* Forall type after substitution.
            // If the type is still an unresolved unification variable, that's a type error —
            // in System F, type applications must be explicit and unambiguous.
            Expr::TyApp { expr, ty } => {
                let expr_ty = Self::infer(expr, env, state)?;
                let expr_ty = state.apply_subs(&expr_ty);
                match expr_ty {
                    Type::Forall(var, body) => Ok(body.subst_ty(&var, ty)),
                    other => Err(format!(
                        "Type application requires a polymorphic type (∀α. τ),\n\
                         but the expression has type: {}",
                        other
                    )),
                }
            }

            // ── Let binding with generalization ───────────────────────────────
            //
            // let x = e₁ in e₂
            //
            // 1. Infer (or check against annotation) e₁ : τ
            // 2. Solve constraints generated for e₁ in isolation
            // 3. Generalize τ over vars not free in Γ → σ
            // 4. Type e₂ in Γ, x : σ
            Expr::Let {
                name,
                ann,
                value,
                body,
            } => Self::infer_let(name, ann.as_ref(), value, body, env, state),

            // ── Type ascription  (e : τ) ──────────────────────────────────────
            Expr::Annot { expr, ty } => {
                let inferred = Self::infer(expr, env, state)?;
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
    ) -> Result<Type, String> {
        let mut new_env = TypeEnv::extend(env.clone());

        let param_types: Vec<Type> = params
            .iter()
            .map(|(name, ann)| {
                let ty = ann.clone().unwrap_or_else(|| state.fresh_var());
                new_env.insert(name.clone(), ty.clone());
                ty
            })
            .collect();

        let body_ty = Self::infer(body, &new_env, state)?;

        if let Some(ret_ann) = return_type {
            state.push_constraint(body_ty.clone(), ret_ann.clone());
        }

        // Build curried arrow: p₁ → p₂ → … → body
        let func_ty = param_types.into_iter().rfold(body_ty, |ret, param| {
            Type::Arrow(Box::new(param), Box::new(ret))
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
            let arrow = Type::Arrow(Box::new(param_ty), Box::new(ret_ty.clone()));
            state.push_constraint(current_ty, arrow);
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
        let cond_ty = Self::infer(condition, env, state)?;
        state.push_constraint(cond_ty, Type::bool());

        let then_ty = Self::infer(then_branch, env, state)?;
        let else_ty = Self::infer(else_branch, env, state)?;
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
    ) -> Result<Type, String> {
        // Isolate value inference: save outer constraints so we only solve
        // the constraints that belong to the `value` expression, enabling
        // clean generalization.
        let outer_constraints = take(&mut state.constraints);

        let poly_ty = if let Some(ann_ty) = ann {
            // Annotation provided (possibly a polytype like ∀a. a → a).
            //
            // We must NOT push `inferred ~ ann_ty` directly, because that would
            // try to unify a monotype (e.g. `?0 → ?0`) with a `∀`-type, which
            // the unifier rightly rejects.
            //
            // Correct approach:
            //   1. *Instantiate* the annotation — replace leading ∀ binders with
            //      fresh unification variables — producing a checkable monotype.
            //   2. Constrain the inferred type against that instantiated monotype.
            //   3. Solve and verify the constraint.
            //   4. Store the *original annotation polytype* in the env unchanged —
            //      it is already the most general type for this binding.
            let inst = state.instantiate(ann_ty);
            let inferred = Self::infer(value, env, state)?;
            state.push_constraint(inferred, inst);
            state.solve_constraints()?;
            // The annotation is already a polytype; no need to generalize again.
            ann_ty.clone()
        } else {
            // No annotation: infer, solve, then generalize over unconstrained vars.
            let inferred = Self::infer(value, env, state)?;
            state.solve_constraints()?;

            // Compute free unification vars in the surrounding env.
            // Those must NOT be generalized — they are monomorphic in context.
            let env_free = state.env_free_unif_vars(env);
            state.generalize(&inferred, &env_free)
        };

        // Restore outer constraints before continuing into the body.
        state.constraints = outer_constraints;

        let mut new_env = TypeEnv::extend(env.clone());
        new_env.insert(name.to_string(), poly_ty);

        Self::infer(body, &new_env, state)
    }

    // ==================== Public API ====================

    /// Infer the type of an expression from scratch.
    pub fn type_of(&self, expr: &Expr) -> Result<Type, String> {
        let mut state = InferState::new();
        let ty = Self::infer(expr, &self.env, &mut state)?;
        state.solve_constraints()?;
        Ok(state.apply_subs(&ty))
    }

    /// Check that an expression has the given type.
    pub fn check(&self, expr: &Expr, expected: &Type) -> Result<(), String> {
        let mut state = InferState::new();
        let actual = Self::infer(expr, &self.env, &mut state)?;
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
//  Tests — original suite (should all still pass)
// ==============================================================================

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
        if let Type::Arrow(left, right) = &ty {
            assert_eq!(left, right, "identity function should have type α → α");
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
        // λa. λb. a  (K combinator)
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
        assert!(result.is_ok(), "K combinator should type-check");
        // Should be α → β → α
        if let Type::Arrow(_, ret) = result.unwrap() {
            if let Type::Arrow(_, _) = *ret {
                // correct shape
            } else {
                panic!("Expected α → β → α");
            }
        } else {
            panic!("Expected arrow type");
        }
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
        // ((λf. λx. f (f x)) (λy: Int. y)) 0
        // ≡ apply twice (identity on Int) to 0
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
        for (lit, expected) in tests {
            assert_eq!(checker.type_of(&Expr::Literal(lit)).unwrap(), expected);
        }
    }

    #[test]
    fn test_function_with_annotation() {
        let checker = TypeChecker::new();
        let expr = Expr::Lambda {
            params: vec![("x".to_string(), Some(Type::int()))],
            body: Box::new(Expr::Ident("x".to_string())),
            return_type: Some(Type::int()),
        };
        let result = checker.type_of(&expr).unwrap();
        assert_eq!(
            result,
            Type::Arrow(Box::new(Type::int()), Box::new(Type::int()))
        );
    }
}

// ==============================================================================
//  Tests — System F specific
// ==============================================================================

#[cfg(test)]
mod system_f_tests {
    use super::*;

    // ── Helpers ────────────────────────────────────────────────────────────────

    /// Λα. λx: α. x  (canonical polymorphic identity)
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

    // ── TyLam ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_ty_lam_yields_forall() {
        let checker = TypeChecker::new();
        // Λα. λx: α. x  should have type  ∀α. α → α
        let ty = checker.type_of(&poly_id()).unwrap();
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
    fn test_ty_lam_body_uses_rigid_var() {
        // The body λx: α. x  has type  α → α (not unification variables)
        // so the bound variable stays rigid inside.
        let checker = TypeChecker::new();
        let inner = Expr::Lambda {
            params: vec![("x".to_string(), Some(Type::ty_var("α")))],
            body: Box::new(Expr::Ident("x".to_string())),
            return_type: None,
        };
        // Without wrapping in TyLam the α is still a free rigid TyVar.
        let ty = checker.type_of(&inner).unwrap();
        assert_eq!(
            ty,
            Type::Arrow(
                Box::new(Type::TyVar("α".to_string())),
                Box::new(Type::TyVar("α".to_string())),
            )
        );
    }

    // ── TyApp ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_ty_app_int() {
        let checker = TypeChecker::new();
        // (Λα. λx: α. x) [Int]  should have type  Int → Int
        let expr = Expr::TyApp {
            expr: Box::new(poly_id()),
            ty: Box::new(Type::int()),
        };
        let ty = checker.type_of(&expr).unwrap();
        assert_eq!(
            ty,
            Type::Arrow(Box::new(Type::int()), Box::new(Type::int()))
        );
    }

    #[test]
    fn test_ty_app_bool() {
        let checker = TypeChecker::new();
        // (Λα. λx: α. x) [Bool]  should have type  Bool → Bool
        let expr = Expr::TyApp {
            expr: Box::new(poly_id()),
            ty: Box::new(Type::bool()),
        };
        let ty = checker.type_of(&expr).unwrap();
        assert_eq!(
            ty,
            Type::Arrow(Box::new(Type::bool()), Box::new(Type::bool()))
        );
    }

    #[test]
    fn test_ty_app_on_monotype_fails() {
        let checker = TypeChecker::new();
        // (λx: Int. x) [Int]  should fail — not a ∀ type
        let expr = Expr::TyApp {
            expr: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), Some(Type::int()))],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            }),
            ty: Box::new(Type::int()),
        };
        assert!(checker.type_of(&expr).is_err());
    }

    // ── Forall unification ─────────────────────────────────────────────────────

    #[test]
    fn test_forall_unification_alpha_equivalent() {
        let checker = TypeChecker::new();
        // Check  (Λα. λx:α. x) : ∀β. β → β
        // These are α-equivalent so this must succeed.
        let expected = Type::Forall(
            "β".to_string(),
            Box::new(Type::Arrow(
                Box::new(Type::TyVar("β".to_string())),
                Box::new(Type::TyVar("β".to_string())),
            )),
        );
        assert!(checker.check(&poly_id(), &expected).is_ok());
    }

    #[test]
    fn test_forall_unification_mismatch_fails() {
        let checker = TypeChecker::new();
        // ∀α. α → α  cannot unify with  ∀α. α → Bool
        let expr = poly_id();
        let bad_type = Type::Forall(
            "α".to_string(),
            Box::new(Type::Arrow(
                Box::new(Type::TyVar("α".to_string())),
                Box::new(Type::bool()),
            )),
        );
        assert!(checker.check(&expr, &bad_type).is_err());
    }

    // ── Let-polymorphism ───────────────────────────────────────────────────────

    #[test]
    fn test_let_monomorphic_application() {
        let checker = TypeChecker::new();
        // let id = λx. x in id 42
        let expr = Expr::Let {
            name: "id".to_string(),
            ann: None,
            value: Box::new(Expr::Lambda {
                params: vec![("x".to_string(), None)],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            }),
            body: Box::new(Expr::Call {
                callee: Box::new(Expr::Ident("id".to_string())),
                params: vec![Expr::Literal(Literal::Int(42))],
            }),
        };
        let ty = checker.type_of(&expr).unwrap();
        assert_eq!(ty, Type::int());
    }

    #[test]
    fn test_let_polymorphic_self_application() {
        let checker = TypeChecker::new();
        // let id = λx. x in id id
        // Requires let-polymorphism: id must be instantiated at two different types.
        let expr = Expr::Let {
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
        // Without let-polymorphism this would fail (trying to unify α with α→α).
        assert!(
            checker.type_of(&expr).is_ok(),
            "id id should type-check with let-polymorphism"
        );
    }

    #[test]
    fn test_let_polymorphic_two_types() {
        let checker = TypeChecker::new();
        // let id = λx. x in
        //   if id true then id 1 else id 2
        // id used at Bool and Int — only possible with let-polymorphism.
        let expr = Expr::Let {
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
        let ty = checker.type_of(&expr).unwrap();
        assert_eq!(ty, Type::int());
    }

    #[test]
    fn test_let_annotated() {
        let checker = TypeChecker::new();
        // let id : ∀a. a → a = λx. x in id 42
        let expr = Expr::Let {
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
        let ty = checker.type_of(&expr).unwrap();
        assert_eq!(ty, Type::int());
    }

    #[test]
    fn test_let_annotation_mismatch_fails() {
        let checker = TypeChecker::new();
        // let id : Int → Int = λx. x in ...
        // Annotation is too specific — but λx. x does unify with Int → Int, so this
        // succeeds. Test that a *wrong* annotation fails.
        let expr = Expr::Let {
            name: "const_int".to_string(),
            ann: Some(Type::arrow(Type::int(), Type::int())),
            value: Box::new(Expr::Literal(Literal::Bool(true))), // Bool ≠ Int → Int
            body: Box::new(Expr::Ident("const_int".to_string())),
        };
        assert!(checker.type_of(&expr).is_err());
    }

    // ── Annot (type ascription) ────────────────────────────────────────────────

    #[test]
    fn test_annot_ok() {
        let checker = TypeChecker::new();
        let expr = Expr::Annot {
            expr: Box::new(Expr::Literal(Literal::Int(0))),
            ty: Type::int(),
        };
        assert_eq!(checker.type_of(&expr).unwrap(), Type::int());
    }

    #[test]
    fn test_annot_mismatch_fails() {
        let checker = TypeChecker::new();
        let expr = Expr::Annot {
            expr: Box::new(Expr::Literal(Literal::Int(0))),
            ty: Type::string(),
        };
        assert!(checker.type_of(&expr).is_err());
    }

    // ── Church encodings ───────────────────────────────────────────────────────

    #[test]
    fn test_church_true() {
        let checker = TypeChecker::new();
        // Λα. λt: α. λf: α. t   ≡ Church true : ∀α. α → α → α
        let church_true = Expr::TyLam {
            ty_param: "α".to_string(),
            body: Box::new(Expr::Lambda {
                params: vec![
                    ("t".to_string(), Some(Type::ty_var("α"))),
                    ("f".to_string(), Some(Type::ty_var("α"))),
                ],
                body: Box::new(Expr::Ident("t".to_string())),
                return_type: None,
            }),
        };
        let ty = checker.type_of(&church_true).unwrap();
        assert_eq!(
            ty,
            Type::Forall(
                "α".to_string(),
                Box::new(Type::Arrow(
                    Box::new(Type::TyVar("α".to_string())),
                    Box::new(Type::Arrow(
                        Box::new(Type::TyVar("α".to_string())),
                        Box::new(Type::TyVar("α".to_string())),
                    )),
                ))
            )
        );
    }

    #[test]
    fn test_church_false() {
        let checker = TypeChecker::new();
        // Λα. λt: α. λf: α. f   ≡ Church false : ∀α. α → α → α  (same type!)
        let church_false = Expr::TyLam {
            ty_param: "α".to_string(),
            body: Box::new(Expr::Lambda {
                params: vec![
                    ("t".to_string(), Some(Type::ty_var("α"))),
                    ("f".to_string(), Some(Type::ty_var("α"))),
                ],
                body: Box::new(Expr::Ident("f".to_string())),
                return_type: None,
            }),
        };
        let expected = Type::Forall(
            "α".to_string(),
            Box::new(Type::Arrow(
                Box::new(Type::TyVar("α".to_string())),
                Box::new(Type::Arrow(
                    Box::new(Type::TyVar("α".to_string())),
                    Box::new(Type::TyVar("α".to_string())),
                )),
            )),
        );
        assert_eq!(checker.type_of(&church_false).unwrap(), expected);
    }

    #[test]
    fn test_church_numeral_zero() {
        let checker = TypeChecker::new();
        // Church zero: Λα. λf: α→α. λx: α. x  : ∀α. (α→α) → α → α
        let church_zero = Expr::TyLam {
            ty_param: "α".to_string(),
            body: Box::new(Expr::Lambda {
                params: vec![
                    (
                        "f".to_string(),
                        Some(Type::arrow(Type::ty_var("α"), Type::ty_var("α"))),
                    ),
                    ("x".to_string(), Some(Type::ty_var("α"))),
                ],
                body: Box::new(Expr::Ident("x".to_string())),
                return_type: None,
            }),
        };
        let ty = checker.type_of(&church_zero).unwrap();
        let expected = Type::Forall(
            "α".to_string(),
            Box::new(Type::Arrow(
                Box::new(Type::Arrow(
                    Box::new(Type::TyVar("α".to_string())),
                    Box::new(Type::TyVar("α".to_string())),
                )),
                Box::new(Type::Arrow(
                    Box::new(Type::TyVar("α".to_string())),
                    Box::new(Type::TyVar("α".to_string())),
                )),
            )),
        );
        assert_eq!(ty, expected);
    }

    // ── Occurs check ──────────────────────────────────────────────────────────

    #[test]
    fn test_occurs_check_prevents_infinite_type() {
        let checker = TypeChecker::new();
        // λx. x x  would require  x : α  and  α ≡ α → β  → infinite type
        let omega = Expr::Lambda {
            params: vec![("x".to_string(), None)],
            body: Box::new(Expr::Call {
                callee: Box::new(Expr::Ident("x".to_string())),
                params: vec![Expr::Ident("x".to_string())],
            }),
            return_type: None,
        };
        assert!(
            checker.type_of(&omega).is_err(),
            "self-application should fail the occurs check"
        );
    }

    // ── Polymorphic application ────────────────────────────────────────────────

    #[test]
    fn test_poly_id_applied_then_called() {
        let checker = TypeChecker::new();
        // ((Λα. λx: α. x) [Int → Int]) (λy: Int. y)
        // = (λx: Int→Int. x) (λy: Int. y)  : Int → Int
        let expr = Expr::Call {
            callee: Box::new(Expr::TyApp {
                expr: Box::new(poly_id()),
                ty: Box::new(Type::arrow(Type::int(), Type::int())),
            }),
            params: vec![Expr::Lambda {
                params: vec![("y".to_string(), Some(Type::int()))],
                body: Box::new(Expr::Ident("y".to_string())),
                return_type: None,
            }],
        };
        let ty = checker.type_of(&expr).unwrap();
        assert_eq!(
            ty,
            Type::Arrow(Box::new(Type::int()), Box::new(Type::int()))
        );
    }

    // ── define() — global environment ─────────────────────────────────────────

    #[test]
    fn test_define_and_use_polymorphic_builtin() {
        let mut checker = TypeChecker::new();
        // Register a polymorphic `const`: ∀a. ∀b. a → b → a
        checker.define(
            "const",
            Type::forall(
                "a",
                Type::forall(
                    "b",
                    Type::arrow(
                        Type::ty_var("a"),
                        Type::arrow(Type::ty_var("b"), Type::ty_var("a")),
                    ),
                ),
            ),
        );

        // const 42 true  should give  Int
        let expr = Expr::Call {
            callee: Box::new(Expr::Call {
                callee: Box::new(Expr::Ident("const".to_string())),
                params: vec![Expr::Literal(Literal::Int(42))],
            }),
            params: vec![Expr::Literal(Literal::Bool(true))],
        };
        let ty = checker.type_of(&expr).unwrap();
        assert_eq!(ty, Type::int());
    }
}
