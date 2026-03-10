module lisp;

import std::io;
import std::collections::list;
import main;
// =============================================================================
// SECTION 3: JIT HELPER FUNCTIONS (closure/define/quasiquote)
// =============================================================================
fn Value* jit_tco_sentinel() {
    return &g_jit_tco_sentinel;
}

// Execute a JitFn with trampoline TCO handling.
// Must be used whenever calling a JitFn from outside jit_eval (e.g., run(), test harness).
fn Value* jit_exec(JitFn f, Interp* interp) {
    jit_exec_enter();
    Value* result = f(interp);
    jit_exec_leave();
    while (interp.flags.jit_tco_bounce) {
        interp.flags.jit_tco_bounce = false;
        result = jit_eval(interp.jit_tco_expr, interp.jit_tco_env, interp);
    }
    return result;
}

// Returns jit_env if set (sub-expression context), else global_env
fn Env* jit_get_env(Interp* interp) {
    if (interp.jit_env != null) return interp.jit_env;
    return interp.global_env;
}

fn bool jit_is_falsy(Value* v, Interp* interp) {
    return is_falsy(v, interp);
}

fn Value* jit_make_true(Interp* interp) {
    return make_symbol(interp, interp.sym_true);
}

fn Value* jit_make_false(Interp* interp) {
    return make_symbol(interp, interp.sym_false);
}

fn char[] jit_env_copy_fault_message(BoundaryEnvCopyFault fault_code) @inline {
    switch (fault_code) {
        case BOUNDARY_ENV_COPY_FAULT_NONE:
            return "jit: closure env-copy failed";
        case BOUNDARY_ENV_COPY_FAULT_PROMOTION_CTX_ALLOC:
            return "jit: closure env-copy failed (promotion context allocation)";
        case BOUNDARY_ENV_COPY_FAULT_DEPTH_LIMIT:
            return "jit: closure env-copy failed (env depth limit)";
        case BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC:
            return "jit: closure env-copy failed (env frame allocation)";
        case BOUNDARY_ENV_COPY_FAULT_BINDING_BUFFER_ALLOC:
            return "jit: closure env-copy failed (binding buffer allocation)";
        case BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY:
            return "jit: closure env-copy failed (binding value copy)";
        case BOUNDARY_ENV_COPY_FAULT_PARENT_COPY:
            return "jit: closure env-copy failed (parent chain copy)";
    }
}

fn bool jit_copy_closure_env_if_needed(
    Interp* interp,
    Value* closure,
    Env* env,
    bool need_env_copy,
    BoundaryEnvCopyFault* fault_out = null
) {
    if (fault_out != null) *fault_out = BOUNDARY_ENV_COPY_FAULT_NONE;
    if (!need_env_copy) return true;
    main::ScopeRegion* env_scope = main::scope_create(null);
    if (env_scope == null) return false;
    BoundaryEnvCopyResult copied = boundary_copy_env_to_target_scope_checked(env, interp, env_scope);
    if (!boundary_env_copy_result_ok(copied)) {
        if (fault_out != null) *fault_out = copied.fault_code;
        main::scope_release(env_scope);
        return false;
    }
    Env* copied_env = copied.env;
    if (copied_env == null) {
        if (fault_out != null) *fault_out = BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC;
        main::scope_release(env_scope);
        return false;
    }
    closure.closure_val.env = copied_env;
    closure.closure_val.env_scope = env_scope;
    return true;
}

fn void jit_copy_lambda_variadic_info(Interp* interp, Value* closure, Expr* expr) {
    closure.closure_val.param_count = expr.lambda.param_count;
    if (expr.lambda.param_count > 0) {
        closure.closure_val.params = (SymbolId*)interp.current_scope.alloc(SymbolId.sizeof * expr.lambda.param_count);
        for (usz i = 0; i < expr.lambda.param_count; i++) {
            closure.closure_val.params[i] = expr.lambda.params[i];
        }
    }
    closure.closure_val.has_rest = expr.lambda.has_rest;
    closure.closure_val.rest_param = expr.lambda.rest_param;

    if (expr.lambda.has_rest && expr.lambda.param_count == 0) {
        closure.closure_val.has_param = false;
        closure.closure_val.param = 0;
    }
}

fn void method_signature_init_empty(MethodSignature* sig) {
    sig.param_types = null;
    sig.param_type_vars = null;
    sig.val_literals = null;
    sig.has_val_literal = null;
    sig.param_count = 0;
    sig.constraints = null;
    sig.constraint_count = 0;
}

fn bool method_signature_alloc_arrays_in_scope(MethodSignature* sig, main::ScopeRegion* scope, usz param_count, usz constraint_count) {
    method_signature_init_empty(sig);
    sig.param_count = param_count;
    sig.constraint_count = constraint_count;
    if (param_count > 0) {
        sig.param_types = (TypeId*)scope.alloc(TypeId.sizeof * param_count);
        sig.param_type_vars = (SymbolId*)scope.alloc(SymbolId.sizeof * param_count);
        sig.val_literals = (ValueLiteralKey*)scope.alloc(ValueLiteralKey.sizeof * param_count);
        sig.has_val_literal = (bool*)scope.alloc(bool.sizeof * param_count);
        if (sig.param_types == null || sig.param_type_vars == null || sig.val_literals == null || sig.has_val_literal == null) return false;
        for (usz i = 0; i < param_count; i++) {
            sig.param_types[i] = INVALID_TYPE_ID;
            sig.param_type_vars[i] = (SymbolId)0;
            sig.val_literals[i].tag = NIL;
            sig.val_literals[i].int_value = 0;
            sig.val_literals[i].sym_value = 0;
            sig.has_val_literal[i] = false;
        }
    }
    if (constraint_count > 0) {
        sig.constraints = (MethodConstraint*)scope.alloc(MethodConstraint.sizeof * constraint_count);
        if (sig.constraints == null) return false;
    }
    return true;
}

fn bool method_signature_copy_to_scope(MethodSignature* dst, MethodSignature* src, main::ScopeRegion* scope) {
    if (!method_signature_alloc_arrays_in_scope(dst, scope, src.param_count, src.constraint_count)) return false;
    for (usz i = 0; i < src.param_count; i++) {
        dst.param_types[i] = src.param_types[i];
        dst.param_type_vars[i] = src.param_type_vars[i];
        dst.val_literals[i] = src.val_literals[i];
        dst.has_val_literal[i] = src.has_val_literal[i];
    }
    for (usz i = 0; i < src.constraint_count; i++) dst.constraints[i] = src.constraints[i];
    return true;
}

fn void method_signature_destroy_heap(MethodSignature* sig) {
    if (sig == null) return;
    if (sig.param_types != null) mem::free(sig.param_types);
    if (sig.param_type_vars != null) mem::free(sig.param_type_vars);
    if (sig.val_literals != null) mem::free(sig.val_literals);
    if (sig.has_val_literal != null) mem::free(sig.has_val_literal);
    if (sig.constraints != null) mem::free(sig.constraints);
    method_signature_init_empty(sig);
}

fn bool method_signature_copy_to_heap(MethodSignature* dst, MethodSignature* src) {
    method_signature_init_empty(dst);
    dst.param_count = src.param_count;
    dst.constraint_count = src.constraint_count;
    if (src.param_count > 0) {
        dst.param_types = (TypeId*)mem::malloc(TypeId.sizeof * src.param_count);
        dst.param_type_vars = (SymbolId*)mem::malloc(SymbolId.sizeof * src.param_count);
        dst.val_literals = (ValueLiteralKey*)mem::malloc(ValueLiteralKey.sizeof * src.param_count);
        dst.has_val_literal = (bool*)mem::malloc(bool.sizeof * src.param_count);
        if (dst.param_types == null || dst.param_type_vars == null || dst.val_literals == null || dst.has_val_literal == null) {
            method_signature_destroy_heap(dst);
            return false;
        }
        for (usz i = 0; i < src.param_count; i++) {
            dst.param_types[i] = src.param_types[i];
            dst.param_type_vars[i] = src.param_type_vars[i];
            dst.val_literals[i] = src.val_literals[i];
            dst.has_val_literal[i] = src.has_val_literal[i];
        }
    }
    if (src.constraint_count > 0) {
        dst.constraints = (MethodConstraint*)mem::malloc(MethodConstraint.sizeof * src.constraint_count);
        if (dst.constraints == null) {
            method_signature_destroy_heap(dst);
            return false;
        }
        for (usz i = 0; i < src.constraint_count; i++) dst.constraints[i] = src.constraints[i];
    }
    return true;
}

fn MethodSignature* jit_build_lambda_type_sig(Interp* interp, Expr* expr) {
    MethodSignature* sig = (MethodSignature*)interp.current_scope.alloc(MethodSignature.sizeof);
    if (sig == null) return null;
    SymbolId sym_lambda_type = interp.symbols.intern("Lambda");

    usz constraint_count = 0;
    for (usz i = 0; i < expr.lambda.param_count; i++) {
        TypeAnnotation* ann = &expr.lambda.param_annotations[i];
        if (ann.has_annotation && ann.is_dict) {
            for (usz m = 0; m < ann.meta_count; m++) {
                if (!ann.meta[m].is_int) constraint_count++;
            }
        }
    }
    if (!method_signature_alloc_arrays_in_scope(sig, interp.current_scope, expr.lambda.param_count, constraint_count)) {
        return null;
    }
    sig.constraint_count = 0;

    for (usz i = 0; i < sig.param_count; i++) {
        TypeAnnotation* ann = &expr.lambda.param_annotations[i];
        if (ann.has_annotation) {
            if (ann.has_val_literal) {
                sig.param_types[i] = INVALID_TYPE_ID;
                sig.param_type_vars[i] = (SymbolId)0;
                sig.val_literals[i] = ann.val_literal;
                sig.has_val_literal[i] = true;
            } else if (ann.is_dict) {
                sig.param_types[i] = INVALID_TYPE_ID;
                sig.param_type_vars[i] = (SymbolId)0;
                sig.val_literals[i].tag = NIL;
                sig.val_literals[i].int_value = 0;
                sig.val_literals[i].sym_value = 0;
                sig.has_val_literal[i] = false;
                bool saw_type_var = false;
                for (usz m = 0; m < ann.meta_count; m++) {
                    if (!ann.meta[m].is_int) {
                        if (!saw_type_var) {
                            sig.param_type_vars[i] = ann.meta[m].key;
                            saw_type_var = true;
                        } else if ((uint)sig.param_type_vars[i] != (uint)ann.meta[m].key) {
                            // Multi-key metadata does not have an unambiguous single type-var binding for this param.
                            sig.param_type_vars[i] = (SymbolId)0;
                        }
                        sig.constraints[sig.constraint_count].param_sym = ann.meta[m].key;
                        sig.constraints[sig.constraint_count].bound_type =
                            interp.types.lookup(ann.meta[m].sym_value, &interp.symbols);
                        sig.constraint_count++;
                    }
                }
            } else {
                TypeId resolved = interp.types.lookup(ann.base_type, &interp.symbols);
                if ((uint)ann.base_type == (uint)sym_lambda_type) {
                    // `^Lambda` / `^(Lambda ...)` gates function-valued args at call boundaries.
                    sig.param_types[i] = interp.tid_Closure;
                    sig.param_type_vars[i] = (SymbolId)0;
                } else {
                    sig.param_types[i] = resolved;
                    sig.param_type_vars[i] = resolved == INVALID_TYPE_ID ? ann.base_type : (SymbolId)0;
                }
                sig.val_literals[i].tag = NIL;
                sig.val_literals[i].int_value = 0;
                sig.val_literals[i].sym_value = 0;
                sig.has_val_literal[i] = false;
            }
        } else {
            sig.param_types[i] = INVALID_TYPE_ID;
            sig.param_type_vars[i] = (SymbolId)0;
            sig.val_literals[i].tag = NIL;
            sig.val_literals[i].int_value = 0;
            sig.val_literals[i].sym_value = 0;
            sig.has_val_literal[i] = false;
        }
    }
    return sig;
}

// Lambda/closure creation helper — called from JIT-compiled code.
// Mirrors eval_lambda from eval.c3: creates a closure value from an E_LAMBDA Expr*.
// Phase 4b: Closures allocated in current_scope (not root_scope). Env copied to a
// standalone env_scope (refcounted). Temporary closures freed when scope releases.
fn Value* jit_make_closure_from_expr(Interp* interp, Expr* expr, Env* env) {
    bool need_env_copy = (interp.current_scope != interp.root_scope);
    BoundaryEnvCopyFault copy_fault = BOUNDARY_ENV_COPY_FAULT_NONE;

    // Check for zero-arg lambda (sentinel param)
    if ((uint)expr.lambda.param == 0xFFFFFFFF && !expr.lambda.has_rest) {
        Value* closure = make_closure_no_param(interp, expr.lambda.body, env);
        if (!jit_copy_closure_env_if_needed(interp, closure, env, need_env_copy, &copy_fault)) {
            return raise_error(interp, jit_env_copy_fault_message(copy_fault));
        }
        return closure;
    }

    // Create closure in current_scope
    Value* closure = make_closure(interp, expr.lambda.param, expr.lambda.body, env);

    jit_copy_lambda_variadic_info(interp, closure, expr);

    closure.closure_val.has_typed_params = expr.lambda.has_typed_params;
    closure.closure_val.type_sig = null;
    if (expr.lambda.has_typed_params) {
        closure.closure_val.type_sig = jit_build_lambda_type_sig(interp, expr);
        if (closure.closure_val.type_sig == null) {
            return raise_error(interp, "jit: failed to allocate method signature");
        }
    }

    if (!jit_copy_closure_env_if_needed(interp, closure, env, need_env_copy, &copy_fault)) {
        return raise_error(interp, jit_env_copy_fault_message(copy_fault));
    }

    return closure;
}

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
// Evaluates to the set value, mirrors E_SET in eval().
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
    return stored;
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
    if ((uint)last_seg == (uint)interp.sym_car) {
        current.cons_val.car = stored;
        return stored;
    }
    if ((uint)last_seg == (uint)interp.sym_cdr) {
        current.cons_val.cdr = stored;
        return stored;
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
                if (stored != null) current.instance_val.fields[fi] = *stored;
                return stored;
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
