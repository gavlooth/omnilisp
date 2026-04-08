module lisp;

import main;

fn bool method_signature_copy_to_escape(MethodSignature* dst, MethodSignature* src, main::ScopeRegion* scope) {
    if (dst == null || src == null || scope == null) return false;
    method_signature_init_empty(dst);
    dst.param_count = src.param_count;
    dst.constraint_count = src.constraint_count;

    if (src.param_count > 0) {
        dst.param_types = (TypeId*)scope.alloc_escape(TypeId.sizeof * src.param_count);
        dst.param_type_vars = (SymbolId*)scope.alloc_escape(SymbolId.sizeof * src.param_count);
        dst.val_literals = (ValueLiteralKey*)scope.alloc_escape(ValueLiteralKey.sizeof * src.param_count);
        dst.has_val_literal = (bool*)scope.alloc_escape(bool.sizeof * src.param_count);
        if (dst.param_types == null || dst.param_type_vars == null || dst.val_literals == null || dst.has_val_literal == null) {
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
        dst.constraints = (MethodConstraint*)scope.alloc_escape(MethodConstraint.sizeof * src.constraint_count);
        if (dst.constraints == null) return false;
        for (usz i = 0; i < src.constraint_count; i++) {
            dst.constraints[i] = src.constraints[i];
        }
    }

    return true;
}

fn bool promote_escape_closure_env_needs_detach(Value* v, Interp* interp) @inline {
    if (v == null || interp == null || v.tag != CLOSURE || v.closure_val == null) return false;
    if (v.closure_val.env == null) return false;
    if (v.closure_val.env_scope != null) {
        return !boundary_ptr_in_scope((void*)v.closure_val.env, v.closure_val.env_scope);
    }
    if (interp.current_scope == null) return false;
    return env_chain_has_frame_in_scope(v.closure_val.env, interp.current_scope, interp.global_env);
}

fn bool promote_escape_should_iterate_cons_tail(Value* old_cdr, Interp* interp) @inline {
    if (old_cdr == null || old_cdr.tag != CONS) return false;
    // Keep long disjoint list promotion iterative to avoid recursive depth blowups
    // under ASAN stack pressure. Reusable target-chain tails still short-circuit.
    if (old_cdr.scope_gen == interp.current_scope.generation) return true;
    if (boundary_ptr_in_target_scope_chain_with_hint((void*)old_cdr, interp, old_cdr.scope_gen)) return false;
    return true;
}

fn Value* promote_escape_cons(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    // Iterative spine copy to avoid recursion blowups on long lists.
    Value* head = interp.alloc_value_escape();
    head.tag = CONS;
    Value* current_new = head;
    Value* current_old = v;

    while (true) {
        current_new.cons_val.car = promote_to_escape(current_old.cons_val.car, interp);
        if (ctx != null && ctx.aborted) return v;
        Value* old_cdr = current_old.cons_val.cdr;
        if (promote_escape_should_iterate_cons_tail(old_cdr, interp)) {
            if (!promote_escape_can_mutate(v, ctx)) return v;
            Value* next_new = interp.alloc_value_escape();
            next_new.tag = CONS;
            current_new.cons_val.cdr = next_new;
            current_new = next_new;
            current_old = old_cdr;
        } else {
            current_new.cons_val.cdr = promote_to_escape(old_cdr, interp);
            if (ctx != null && ctx.aborted) return v;
            break;
        }
    }
    return head;
}

fn Value* promote_escape_closure(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = CLOSURE;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_closure);
    result.closure_val = (Closure*)interp.current_scope.alloc_escape(Closure.sizeof);
    *result.closure_val = *v.closure_val;
    result.closure_val.scope_gen = interp.current_scope.escape_generation;

    if (v.closure_val.param_count > 0) {
        result.closure_val.params = (SymbolId*)interp.current_scope.alloc_escape(SymbolId.sizeof * v.closure_val.param_count);
        for (usz i = 0; i < v.closure_val.param_count; i++) {
            result.closure_val.params[i] = v.closure_val.params[i];
        }
    } else {
        result.closure_val.params = null;
    }

    if (v.closure_val.type_sig != null) {
        result.closure_val.type_sig = (MethodSignature*)interp.current_scope.alloc_escape(MethodSignature.sizeof);
        if (result.closure_val.type_sig == null ||
            !method_signature_copy_to_escape(result.closure_val.type_sig, v.closure_val.type_sig, interp.current_scope)) {
            if (ctx != null) {
                ctx.aborted = true;
                ctx.abort_reason = PROMOTION_ABORT_BUDGET_EXHAUSTED;
            }
            return raise_error(interp, boundary_copy_fault_message(BOUNDARY_COPY_FAULT_CLOSURE_TYPE_SIG_COPY));
        }
    }

    if (promote_escape_closure_env_needs_detach(v, interp)) {
        main::ScopeRegion* env_scope = main::scope_create(null);
        if (env_scope == null) {
            if (ctx != null) {
                ctx.aborted = true;
                ctx.abort_reason = PROMOTION_ABORT_BUDGET_EXHAUSTED;
            }
            return raise_error(interp, "boundary: failed to allocate detached closure env scope");
        }
        BoundaryEnvCopyResult copied = boundary_copy_env_to_target_scope_checked(v.closure_val.env, interp, env_scope, 0, ctx);
        if (!boundary_env_copy_result_ok(copied) || copied.env == null) {
            if (ctx != null) {
                ctx.aborted = true;
                if (ctx.abort_reason == PROMOTION_ABORT_NONE) {
                    ctx.abort_reason = PROMOTION_ABORT_BUDGET_EXHAUSTED;
                }
            }
            main::scope_release(env_scope);
            return raise_error(interp, "boundary: failed to detach closure env during escape promotion");
        }
        result.closure_val.env = copied.env;
        result.closure_val.env_scope = env_scope;
    } else if (closure_env_scope_is_root_owned(result, interp)) {
        result.closure_val.env_scope = null;
    } else if (result.closure_val.env_scope != null) {
        main::scope_retain(result.closure_val.env_scope);
    }
    return result;
}
