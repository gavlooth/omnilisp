module lisp;

import main;

// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (let/define/quasiquote)
// =============================================================================
// Recursive let helper — called from JIT-compiled code.
// Mirrors the is_recursive branch of eval_let from eval.c3.
// Creates placeholder, extends env, evaluates init, patches closure, evaluates body.
// Destructuring let: evaluate init, match against pattern, bind results in env.
// Called from JIT-compiled code when let_expr.pattern != null.
fn Value* jit_eval_let_destruct(Interp* interp, Expr* expr, Env* env) {
    Value* init_val = jit_eval(expr.let_expr.init, env, interp);
    if (init_val != null && init_val.tag == ERROR) return init_val;

    MatchResult mr = match_pattern(expr.let_expr.pattern, init_val, interp);
    if (!mr.matched) {
        mr.cleanup();
        return raise_error(interp, "let destructuring: pattern did not match value");
    }

    Env* dest_env = env;
    for (usz i = 0; i < mr.binding_count; i++) {
        dest_env = make_env(interp, dest_env);
        env_define_with_barrier(dest_env, mr.bindings[i].name, mr.bindings[i].value, interp);
    }
    mr.cleanup();

    return jit_eval(expr.let_expr.body, dest_env, interp);
}

fn void jit_patch_rec_closure_in_env_scope(Interp* interp, Value* actual_value, SymbolId name) {
    main::ScopeRegion* env_scope = actual_value.closure_val.env_scope;

    Value* self_ref = boundary_alloc_value_in_scope(interp, env_scope);
    self_ref.tag = CLOSURE;

    Closure* copied_closure = (Closure*)env_scope.alloc(Closure.sizeof);
    *copied_closure = *actual_value.closure_val;
    copied_closure.scope_gen = env_scope.generation;

    if (copied_closure.param_count > 0) {
        copied_closure.params = (SymbolId*)env_scope.alloc(SymbolId.sizeof * copied_closure.param_count);
        for (usz i = 0; i < copied_closure.param_count; i++) {
            copied_closure.params[i] = actual_value.closure_val.params[i];
        }
    } else {
        copied_closure.params = null;
    }

    if (copied_closure.type_sig != null) {
        copied_closure.type_sig = (MethodSignature*)env_scope.alloc(MethodSignature.sizeof);
        if (copied_closure.type_sig == null ||
            !method_signature_copy_to_scope(copied_closure.type_sig, actual_value.closure_val.type_sig, env_scope)) {
            copied_closure.type_sig = null;
        }
    }

    self_ref.closure_val = copied_closure;

    Env* closure_env = actual_value.closure_val.env;
    for (usz i = 0; i < closure_env.binding_count; i++) {
        if ((uint)closure_env.bindings[i].name == (uint)name) {
            closure_env.bindings[i].value = self_ref;
            break;
        }
    }
}

fn void jit_patch_rec_closure(Value* actual_value, Env* rec_env, SymbolId name, Interp* interp) {
    if (!is_closure(actual_value)) return;
    if (actual_value.closure_val.env_scope != null) {
        jit_patch_rec_closure_in_env_scope(interp, actual_value, name);
    } else {
        actual_value.closure_val.env = rec_env;
    }
}

fn void jit_patch_rec_binding(Env* rec_env, SymbolId name, Value* actual_value) {
    for (usz i = 0; i < rec_env.binding_count; i++) {
        if ((uint)rec_env.bindings[i].name == (uint)name) {
            rec_env.bindings[i].value = actual_value;
            break;
        }
    }
}

fn bool jit_letrec_body_has_shift(Value* actual_value, Expr* init_expr) {
    if (!is_closure(actual_value)) return false;
    Expr* lambda_body = init_expr;
    if (lambda_body == null || lambda_body.tag != E_LAMBDA) return false;
    return expr_contains_shift(lambda_body.lambda.body);
}

fn Value* jit_eval_let_rec(Interp* interp, Expr* expr, Env* env) {
    Value* placeholder = make_nil(interp);
    Env* rec_env = env.extend(interp, expr.let_expr.name, placeholder);
    Value* actual_value = jit_eval(expr.let_expr.init, rec_env, interp);
    if (actual_value != null && actual_value.tag == ERROR) return actual_value;

    jit_patch_rec_closure(actual_value, rec_env, expr.let_expr.name, interp);
    jit_patch_rec_binding(rec_env, expr.let_expr.name, actual_value);

    bool body_has_shift = jit_letrec_body_has_shift(actual_value, expr.let_expr.init);
    Value* result = jit_eval_in_call_scope(expr.let_expr.body, rec_env, interp, !body_has_shift);
    return result;
}

// set! helper — called from JIT-compiled code.
// Command semantics: returns Void on successful mutation.
fn Value* jit_eval_set(Interp* interp, SymbolId name, Value* value, Env* env) {
    // Env bindings (closures, globals) live in root scope; promote child-lifetime
    // values before storing to avoid dangling pointers.
    Value* stored = boundary_promote_to_root_site(
        value,
        interp,
        COPY_SITE_JIT_EVAL_SET
    );
    if (stored != null && stored.tag == ERROR) return stored;
    // Try local env first, then global
    if (catch err = env_set_with_barrier(env, name, stored, interp)) {
        if (interp.global_env == null) return raise_error(interp, "set!: unbound variable");
        if (catch err2 = env_set_with_barrier(interp.global_env, name, stored, interp)) {
            return raise_error(interp, "set!: unbound variable");
        }
    }
    return make_void(interp);
}

fn Value* jit_resolve_set_path_root(Interp* interp, Expr* expr, Env* env) {
    Value* current = env.lookup(expr.set_expr.path_segments[0]);
    if (current == null) current = interp.global_env.lookup(expr.set_expr.path_segments[0]);
    if (current == null) return raise_error(interp, "set!: unbound path root");
    return current;
}

fn Value* jit_set_path_step_instance(Value* current, SymbolId seg, Interp* interp) {
    TypeInfo* ti = interp.types.get(current.instance_val.type_id);
    if (ti != null) {
        for (usz fi = 0; fi < ti.field_count; fi++) {
            if (ti.fields[fi].name == seg && fi < current.instance_val.field_count) {
                return &current.instance_val.fields[fi];
            }
        }
    }
    return raise_error(interp, "set!: field not found in path");
}

fn Value* jit_set_path_step_cons(Value* current, SymbolId seg, Interp* interp) {
    if ((uint)seg == (uint)interp.sym_car) return current.cons_val.car;
    if ((uint)seg == (uint)interp.sym_cdr) return current.cons_val.cdr;
    return raise_error(interp, "set!: cons only supports .car and .cdr");
}

fn Value* jit_set_path_step(Value* current, SymbolId seg, Interp* interp) {
    if (current != null && current.tag == INSTANCE && current.instance_val != null) {
        return jit_set_path_step_instance(current, seg, interp);
    }
    if (current != null && current.tag == CONS) {
        return jit_set_path_step_cons(current, seg, interp);
    }
    return raise_error(interp, "set!: path segment is not an instance or cons");
}

fn Value* jit_set_path_mutate_cons(Value* current, SymbolId last_seg, Value* value, Interp* interp) {
    Value* stored = boundary_promote_to_root(value, interp);
    if (stored != null && stored.tag == ERROR) return stored;
    if ((uint)last_seg == (uint)interp.sym_car) {
        current.cons_val.car = stored;
        return make_void(interp);
    }
    if ((uint)last_seg == (uint)interp.sym_cdr) {
        current.cons_val.cdr = stored;
        return make_void(interp);
    }
    return raise_error(interp, "set!: cons only supports .car and .cdr");
}

<*
@require interp != null : "jit_copy_value_into_scope_site: null interp"
@require target_scope != null : "jit_copy_value_into_scope_site: null target_scope"
@require releasing_scope != null : "jit_copy_value_into_scope_site: null releasing_scope"
*>
fn Value* jit_copy_value_into_scope_site(
    Value* value,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    uint site
) {
    if (value == null || target_scope == null) return value;
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);
    return boundary_copy_to_parent_site_ctx(value, interp, site, &copy_ctx);
}

fn Value* jit_set_path_mutate_instance(Value* current, SymbolId last_seg, Value* value, Interp* interp) {
    TypeInfo* ti = interp.types.get(current.instance_val.type_id);
    if (ti != null) {
        for (usz fi = 0; fi < ti.field_count; fi++) {
            if (ti.fields[fi].name == last_seg && fi < current.instance_val.field_count) {
                main::ScopeRegion* target = current.instance_val.owner_scope;
                if (target == null) target = interp.root_scope;
                Value* stored = null;
                if (target == interp.root_scope) {
                    stored = boundary_promote_to_root_site(
                        value,
                        interp,
                        COPY_SITE_ENV_PROMOTE_FOR_TARGET
                    );
                } else {
                    stored = jit_copy_value_into_scope_site(
                        value,
                        interp,
                        target,
                        interp.current_scope,
                        COPY_SITE_ENV_PROMOTE_FOR_TARGET
                    );
                }
                if (stored != null && stored.tag == ERROR) return stored;
                if (stored != null) current.instance_val.fields[fi] = *stored;
                return make_void(interp);
            }
        }
    }
    return raise_error(interp, "set!: field not found");
}

// set! dot-path helper — handles (set! obj.field value) mutations.
// Mirrors the dot-path E_SET logic from eval.c3 lines 496-561.
fn Value* jit_eval_set_path(Interp* interp, Expr* expr, Value* value, Env* env) {
    Value* current = jit_resolve_set_path_root(interp, expr, env);
    if (current != null && current.tag == ERROR) return current;

    // Traverse intermediate segments
    for (usz si = 1; si < expr.set_expr.path_segment_count - 1; si++) {
        current = jit_set_path_step(current, expr.set_expr.path_segments[si], interp);
        if (current != null && current.tag == ERROR) return current;
    }

    // Mutate the last field
    SymbolId last_seg = expr.set_expr.path_segments[expr.set_expr.path_segment_count - 1];

    // Cons cell mutation
    if (current != null && current.tag == CONS) {
        return jit_set_path_mutate_cons(current, last_seg, value, interp);
    }

    // Instance field mutation
    if (current != null && current.tag == INSTANCE && current.instance_val != null) {
        return jit_set_path_mutate_instance(current, last_seg, value, interp);
    }
    return raise_error(interp, "set!: target is not an instance or cons");
}

// Environment extension helper — called from JIT-compiled code.
// Extends an env with a new binding for capturing let-locals in lambdas.
fn Env* jit_env_extend(Interp* interp, Env* env, SymbolId name, Value* value) {
    return env.extend(interp, name, value);
}

// Mutable box allocation in root-scope lifetime — called from JIT-compiled code.
// Creates a root-persistent mutable-box Env node that survives child region cleanup and
// is preserved (not copied) by env_copy. This ensures closures and
// the enclosing JIT scope share the same mutable box.
<* @require interp != null : "jit_env_extend_root: null interp" *>
fn Env* jit_env_extend_root(Interp* interp, Env* env, SymbolId name, Value* value) {
    // Promote initial payload before publishing into persistent root env box.
    Value* promoted = boundary_promote_to_root_site(
        value,
        interp,
        COPY_SITE_JIT_ENV_EXTEND_ROOT
    );
    Env* box = boundary_env_extend_in_root(interp, env, name, promoted);
    return box;
}

// JIT-native quasiquote — no eval() dependency.
