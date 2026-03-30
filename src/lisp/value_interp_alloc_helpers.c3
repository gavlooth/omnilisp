module lisp;

import main;

fn Value* Interp.alloc_value(Interp* self) @inline {
    Value* v = (Value*)self.current_scope.alloc(Value.sizeof);
    assert(v != null, "alloc_value: scope allocation failed");
    v.tag = NIL;  // Safe default
    v.scope_gen = self.current_scope.generation;  // Stamp for O(1) scope checks
    // Dtor registration moved to heap-backed constructors (make_string, make_closure, etc.)
    // Flat types (NIL, INT, DOUBLE, SYMBOL, CONS, BOOL) skip dtor — no heap backing.
    return v;
}

// Allocate a Value in current scope's ESCAPE lane.
// Not wired into constructors yet; used by dual-lane rollout phases.
fn Value* Interp.alloc_value_escape(Interp* self) @inline {
    Value* v = (Value*)self.current_scope.alloc_escape(Value.sizeof);
    assert(v != null, "alloc_value_escape: scope allocation failed");
    v.tag = NIL;  // Safe default
    v.scope_gen = self.current_scope.escape_generation;
    return v;
}

// Allocate a Value in root_scope (for Expr literals and other permanent values).
// Root-scope dtors only run at program exit, so unconditional registration is fine here.
// The perf win is from removing dtors in alloc_value() (child scopes, per-call teardown).
fn Value* Interp.alloc_value_root(Interp* self) @inline {
    Value* v = (Value*)self.root_scope.alloc(Value.sizeof);
    assert(v != null, "alloc_value_root: scope allocation failed");
    v.tag = NIL;
    v.scope_gen = self.root_scope.generation;  // Stamp for O(1) scope checks
    main::scope_register_dtor(self.root_scope, (void*)v, &scope_dtor_value);
    return v;
}

fn Env* Interp.alloc_env(Interp* self) @inline {
    Env* e = (Env*)self.current_scope.alloc(Env.sizeof);
    assert(e != null, "alloc_env: scope allocation failed");
    e.bindings = null;
    e.binding_count = 0;
    e.capacity = 0;
    e.parent = null;
    e.hash_table = null;
    e.hash_capacity = 0;
    e.lifetime_kind = ENV_LIFETIME_LOCAL_SCOPE_FRAME;
    e.scope_gen = self.current_scope.generation;
    return e;
}

fn Env* Interp.alloc_env_escape(Interp* self) @inline {
    Env* e = (Env*)self.current_scope.alloc_escape(Env.sizeof);
    assert(e != null, "alloc_env_escape: scope allocation failed");
    e.bindings = null;
    e.binding_count = 0;
    e.capacity = 0;
    e.parent = null;
    e.hash_table = null;
    e.hash_capacity = 0;
    e.lifetime_kind = ENV_LIFETIME_LOCAL_SCOPE_FRAME;
    e.scope_gen = self.current_scope.escape_generation;
    return e;
}

fn Expr* Interp.alloc_expr(Interp* self) {
    Expr* expr = (Expr*)ast_arena_alloc(&self.ast_arena, Expr.sizeof);
    assert(expr != null, "alloc_expr: ast arena allocation failed");
    return expr;
}

fn Pattern* Interp.alloc_pattern(Interp* self) {
    Pattern* pattern = (Pattern*)ast_arena_alloc(&self.ast_arena, Pattern.sizeof);
    assert(pattern != null, "alloc_pattern: ast arena allocation failed");
    return pattern;
}
