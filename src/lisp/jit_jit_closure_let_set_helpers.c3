module lisp;

import main;

fn Value* jit_eval_let_destruct(Interp* interp, Expr* expr, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    Value* init_val = jit_eval(expr.let_expr.init, env, interp);
    if (init_val != null && init_val.tag == ERROR) return init_val;

    MatchResult mr = match_pattern(expr.let_expr.pattern, init_val, interp);
    if (mr.has_error()) {
        Value* err = mr.error;
        mr.cleanup();
        return err;
    }
    if (!mr.matched) {
        mr.cleanup();
        return raise_error(interp, "let destructuring: pattern did not match value");
    }

    Env* dest_env = env;
    for (usz i = 0; i < mr.binding_count; i++) {
        dest_env = make_env(interp, dest_env);
        if (dest_env == null ||
            env_define_with_barrier_result(dest_env, mr.bindings[i].name, mr.bindings[i].value, interp) != null) {
            mr.cleanup();
            return raise_error(interp, "let destructuring: out of memory while binding pattern");
        }
    }
    mr.cleanup();

    return jit_eval(expr.let_expr.body, dest_env, interp);
}

fn Value* jit_patch_rec_closure_in_env_scope(Interp* interp, Value* actual_value, SymbolId name) {
    main::ScopeRegion* env_scope = actual_value.closure_val.env_scope;
    MethodSignature* src_sig = actual_value.closure_val.type_sig;

    Value* self_ref = boundary_alloc_value_in_scope(interp, env_scope);
    if (self_ref == null) return null;
    self_ref.tag = CLOSURE;

    Closure* copied_closure = (Closure*)env_scope.alloc(Closure.sizeof);
    if (copied_closure == null) return null;
    *copied_closure = *actual_value.closure_val;
    copied_closure.scope_gen = env_scope.generation;

    if (copied_closure.param_count > 0) {
        copied_closure.params = (SymbolId*)env_scope.alloc(SymbolId.sizeof * copied_closure.param_count);
        if (copied_closure.params == null) return null;
        for (usz i = 0; i < copied_closure.param_count; i++) {
            copied_closure.params[i] = actual_value.closure_val.params[i];
        }
    } else {
        copied_closure.params = null;
    }

    copied_closure.type_sig = null;
    if (src_sig != null) {
        copied_closure.type_sig = (MethodSignature*)env_scope.alloc(MethodSignature.sizeof);
        if (copied_closure.type_sig == null ||
            !method_signature_copy_to_scope(copied_closure.type_sig, src_sig, env_scope)) {
            return null;
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
    return self_ref;
}

fn Value* jit_patch_rec_closure(Value* actual_value, Env* rec_env, SymbolId name, Interp* interp) {
    if (!is_closure(actual_value)) return actual_value;
    if (actual_value.closure_val.env_scope != null) {
        return jit_patch_rec_closure_in_env_scope(interp, actual_value, name);
    }
    actual_value.closure_val.env = rec_env;
    return actual_value;
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
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    Value* placeholder = make_nil(interp);
    Env* rec_env = jit_env_extend(interp, env, expr.let_expr.name, placeholder);
    if (rec_env == null) return raise_error(interp, "let: out of memory while binding recursive name");
    Value* actual_value = jit_eval(expr.let_expr.init, rec_env, interp);
    if (actual_value != null && actual_value.tag == ERROR) return actual_value;

    Value* published_value = jit_patch_rec_closure(actual_value, rec_env, expr.let_expr.name, interp);
    if (actual_value != null && actual_value.tag == CLOSURE && published_value == null) {
        return raise_error(interp, "let: out of memory while patching recursive closure");
    }
    jit_patch_rec_binding(rec_env, expr.let_expr.name, published_value);

    bool body_has_shift = jit_letrec_body_has_shift(published_value, expr.let_expr.init);
    Value* result = jit_eval_in_call_scope(expr.let_expr.body, rec_env, interp, !body_has_shift);
    return result;
}

fn Value* jit_eval_set(Interp* interp, SymbolId name, Value* value, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    bool found = false;
    Value* write_err = env_set_with_barrier_result(env, name, value, interp, &found);
    if (write_err != null) return write_err;
    if (!found) {
        if (interp.global_env == null) return raise_error(interp, "set!: unbound variable");
        write_err = env_set_with_barrier_result(interp.global_env, name, value, interp, &found);
        if (write_err != null) return write_err;
        if (!found) {
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
                Value* stored = jit_copy_value_into_scope_site(
                    value,
                    interp,
                    target,
                    interp.current_scope,
                    COPY_SITE_ENV_PROMOTE_FOR_TARGET
                );
                if (stored != null && stored.tag == ERROR) return stored;
                if (stored != null) current.instance_val.fields[fi] = *stored;
                return make_void(interp);
            }
        }
    }
    return raise_error(interp, "set!: field not found");
}

fn Value* jit_eval_set_path(Interp* interp, Expr* expr, Value* value, Env* env) {
    interp = jit_resolve_interp(interp);
    if (interp == null) return null;
    Value* current = jit_resolve_set_path_root(interp, expr, env);
    if (current != null && current.tag == ERROR) return current;

    for (usz si = 1; si < expr.set_expr.path_segment_count - 1; si++) {
        current = jit_set_path_step(current, expr.set_expr.path_segments[si], interp);
        if (current != null && current.tag == ERROR) return current;
    }

    SymbolId last_seg = expr.set_expr.path_segments[expr.set_expr.path_segment_count - 1];
    if (current != null && current.tag == CONS) {
        return jit_set_path_mutate_cons(current, last_seg, value, interp);
    }
    if (current != null && current.tag == INSTANCE && current.instance_val != null) {
        return jit_set_path_mutate_instance(current, last_seg, value, interp);
    }
    return raise_error(interp, "set!: target is not an instance or cons");
}
