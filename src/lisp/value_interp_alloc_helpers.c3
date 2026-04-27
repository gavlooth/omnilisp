module lisp;

import main;

int g_interp_force_alloc_value_root_fail_nth = 0;

fn bool interp_should_force_alloc_value_root_fail() @inline {
    if (g_interp_force_alloc_value_root_fail_nth <= 0) return false;
    g_interp_force_alloc_value_root_fail_nth--;
    return g_interp_force_alloc_value_root_fail_nth == 0;
}

fn Value* Interp.alloc_value(Interp* self) @inline {
    Value* v = (Value*)self.current_scope.alloc(Value.sizeof);
    if (v == null) return null;
    v.tag = NIL;  // Safe default
    v.scope_gen = self.current_scope.generation;  // Stamp for O(1) scope checks
    v.continuation_boundary_owned = false;
    v.string_heap_owned = false;
    // Dtor registration moved to heap-backed constructors (make_string, make_closure, etc.)
    // Flat types (NIL, INT, DOUBLE, SYMBOL, CONS, BOOL) skip dtor — no heap backing.
    return v;
}

// Allocate a Value in current scope's ESCAPE lane.
// Not wired into constructors yet; used by dual-lane rollout phases.
fn Value* Interp.alloc_value_escape(Interp* self) @inline {
    Value* v = (Value*)self.current_scope.alloc_escape_attributed(
        Value.sizeof,
        main::ScopeEscapeAllocSource.SCOPE_ESCAPE_ALLOC_INTERP_VALUE
    );
    if (v == null) return null;
    v.tag = NIL;  // Safe default
    v.scope_gen = self.current_scope.escape_generation;
    v.continuation_boundary_owned = false;
    v.string_heap_owned = false;
    return v;
}

// Allocate a Value in root_scope (for Expr literals and other permanent values).
// Root-scope dtors only run at program exit, so unconditional registration is fine here.
// The perf win is from removing dtors in alloc_value() (child scopes, per-call teardown).
fn Value* Interp.alloc_value_root(Interp* self) @inline {
    if (interp_should_force_alloc_value_root_fail()) return null;
    Value* v = (Value*)self.root_scope.alloc(Value.sizeof);
    if (v == null) return null;
    v.tag = NIL;
    v.scope_gen = self.root_scope.generation;  // Stamp for O(1) scope checks
    v.continuation_boundary_owned = false;
    v.string_heap_owned = false;
    if (!main::scope_register_dtor(self.root_scope, (void*)v, &scope_dtor_value)) return null;
    return v;
}

fn Env* Interp.alloc_env(Interp* self) @inline {
    Env* e = (Env*)self.current_scope.alloc(Env.sizeof);
    if (e == null) return null;
    e.bindings = null;
    e.binding_count = 0;
    e.capacity = 0;
    e.parent = null;
    e.hash_table = null;
    e.hash_capacity = 0;
    e.is_inline = false;
    e.lifetime_kind = ENV_LIFETIME_LOCAL_SCOPE_FRAME;
    e.scope_gen = self.current_scope.generation;
    e.owner_scope = self.current_scope;
    e.owner_escape_lane = false;
    return e;
}

fn Env* Interp.alloc_env_escape(Interp* self) @inline {
    Env* e = (Env*)self.current_scope.alloc_escape_attributed(
        Env.sizeof,
        main::ScopeEscapeAllocSource.SCOPE_ESCAPE_ALLOC_INTERP_ENV
    );
    if (e == null) return null;
    e.bindings = null;
    e.binding_count = 0;
    e.capacity = 0;
    e.parent = null;
    e.hash_table = null;
    e.hash_capacity = 0;
    e.is_inline = false;
    e.lifetime_kind = ENV_LIFETIME_LOCAL_SCOPE_FRAME;
    e.scope_gen = self.current_scope.escape_generation;
    e.owner_scope = self.current_scope;
    e.owner_escape_lane = true;
    return e;
}

fn Expr* Interp.alloc_expr(Interp* self) {
    Expr* expr = (Expr*)ast_arena_alloc(&self.ast_arena, Expr.sizeof);
    assert(expr != null, "alloc_expr: ast arena allocation failed");
    return expr;
}

fn Pattern* Interp.alloc_pattern(Interp* self) {
    Pattern* pattern = (Pattern*)ast_arena_alloc(&self.ast_arena, Pattern.sizeof);
    return pattern;
}
