module lisp;

import std::core::mem;
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

fn Value* promote_escape_array_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    Value* result = interp.alloc_value_escape();
    if (result == null) {
        return raise_error(interp, "boundary: failed to allocate array escape copy");
    }
    result.tag = ARRAY;
    result.array_val = null;

    Array* src_arr = v.array_val;
    if (src_arr == null) {
        main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    Array* dst_arr = (Array*)mem::malloc(Array.sizeof);
    if (dst_arr == null) {
        return raise_error(interp, "boundary: failed to allocate array escape copy");
    }

    usz cap = src_arr.capacity;
    if (cap < src_arr.length) cap = src_arr.length;
    if (cap < 4) cap = 4;

    dst_arr.capacity = cap;
    dst_arr.length = src_arr.length;
    dst_arr.items = (Value**)mem::malloc(Value*.sizeof * cap);
    if (dst_arr.items == null) {
        mem::free(dst_arr);
        return raise_error(interp, "boundary: failed to allocate array escape copy");
    }

    for (usz i = 0; i < src_arr.length; i++) {
        Value* promoted = promote_to_escape(src_arr.items[i], interp);
        if (src_arr.items[i] != null && (promoted == null || is_error(promoted))) {
            boundary_cleanup_partial_array_copy(src_arr, dst_arr, i);
            return promoted == null
                ? raise_error(interp, "boundary: failed to promote array item during escape promotion")
                : promoted;
        }
        dst_arr.items[i] = promoted;
    }

    result.array_val = dst_arr;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_hashmap_backed_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    Value* result = interp.alloc_value_escape();
    if (result == null) {
        return raise_error(interp, "boundary: failed to allocate hashmap escape copy");
    }
    result.tag = v.tag;
    result.hashmap_val = null;

    HashMap* src_map = v.hashmap_val;
    if (src_map == null) {
        main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    HashMap* dst_map = (HashMap*)mem::malloc(HashMap.sizeof);
    if (dst_map == null) {
        return raise_error(interp, "boundary: failed to allocate hashmap escape copy");
    }
    dst_map.capacity = src_map.capacity;
    dst_map.count = src_map.count;
    dst_map.mask = src_map.mask;
    dst_map.entries = null;

    if (src_map.capacity > 0) {
        dst_map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * src_map.capacity);
        if (dst_map.entries == null) {
            mem::free(dst_map);
            return raise_error(interp, "boundary: failed to allocate hashmap escape copy");
        }

        for (uint i = 0; i < src_map.capacity; i++) {
            HashEntry* src_e = &src_map.entries[i];
            HashEntry* dst_e = &dst_map.entries[i];
            dst_e.key = null;
            dst_e.value = null;
            if (src_e.key != null) {
                Value* promoted_key = promote_to_escape(src_e.key, interp);
                if (promoted_key == null || is_error(promoted_key)) {
                    boundary_cleanup_partial_hashmap_copy(src_map, dst_map, i + 1);
                    return promoted_key == null
                        ? raise_error(interp, "boundary: failed to promote hashmap key during escape promotion")
                        : promoted_key;
                }
                dst_e.key = promoted_key;
                Value* promoted_value = promote_to_escape(src_e.value, interp);
                if (src_e.value != null && (promoted_value == null || is_error(promoted_value))) {
                    boundary_cleanup_partial_hashmap_copy(src_map, dst_map, i + 1);
                    return promoted_value == null
                        ? raise_error(interp, "boundary: failed to promote hashmap value during escape promotion")
                        : promoted_value;
                }
                dst_e.value = promoted_value;
            }
        }
    }

    result.hashmap_val = dst_map;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_method_table_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    Value* result = interp.alloc_value_escape();
    if (result == null) {
        return raise_error(interp, "boundary: failed to allocate method table escape copy");
    }
    result.tag = METHOD_TABLE;
    result.method_table_val = null;

    MethodTable* src_mt = v.method_table_val;
    if (src_mt == null) {
        main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    MethodTable* dst_mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
    if (dst_mt == null) {
        return raise_error(interp, "boundary: failed to allocate method table escape copy");
    }
    dst_mt.name = src_mt.name;
    dst_mt.entry_count = src_mt.entry_count;
    dst_mt.capacity = src_mt.capacity;
    if (dst_mt.capacity < dst_mt.entry_count) dst_mt.capacity = dst_mt.entry_count;
    dst_mt.entries = null;
    dst_mt.fallback = null;

    if (dst_mt.capacity > 0) {
        dst_mt.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * dst_mt.capacity);
        if (dst_mt.entries == null) {
            mem::free(dst_mt);
            return raise_error(interp, "boundary: failed to allocate method table escape copy");
        }
    }

    for (usz i = 0; i < dst_mt.entry_count; i++) {
        method_signature_init_empty(&dst_mt.entries[i].sig);
        dst_mt.entries[i].implementation = null;
        if (!method_signature_copy_to_heap(&dst_mt.entries[i].sig, &src_mt.entries[i].sig)) {
            boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, i);
            return raise_error(interp, "boundary: failed to copy method signature during escape promotion");
        }
        Value* promoted_impl = promote_to_escape(src_mt.entries[i].implementation, interp);
        if (src_mt.entries[i].implementation != null && (promoted_impl == null || is_error(promoted_impl))) {
            boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, i);
            return promoted_impl == null
                ? raise_error(interp, "boundary: failed to promote method implementation during escape promotion")
                : promoted_impl;
        }
        dst_mt.entries[i].implementation = promoted_impl;
    }

    Value* promoted_fallback = promote_to_escape(src_mt.fallback, interp);
    if (src_mt.fallback != null && (promoted_fallback == null || is_error(promoted_fallback))) {
        boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, dst_mt.entry_count);
        return promoted_fallback == null
            ? raise_error(interp, "boundary: failed to promote method fallback during escape promotion")
            : promoted_fallback;
    }
    dst_mt.fallback = promoted_fallback;

    result.method_table_val = dst_mt;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
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
    Value* head = null;
    Value* current_new = null;
    Value* current_old = v;

    while (true) {
        Value* promoted_car = promote_to_escape(current_old.cons_val.car, interp);
        if (current_old.cons_val.car != null && (promoted_car == null || is_error(promoted_car))) {
            return promoted_car == null
                ? raise_error(interp, "boundary: failed to promote cons car during escape promotion")
                : promoted_car;
        }
        if (head == null) {
            head = interp.alloc_value_escape();
            if (head == null) {
                return raise_error(interp, "boundary: failed to allocate cons escape copy");
            }
            head.tag = CONS;
            current_new = head;
        }
        current_new.cons_val.car = promoted_car;
        if (ctx != null && ctx.aborted) return v;
        Value* old_cdr = current_old.cons_val.cdr;
        if (promote_escape_should_iterate_cons_tail(old_cdr, interp)) {
            if (!promote_escape_can_mutate(v, ctx)) return v;
            Value* next_new = interp.alloc_value_escape();
            if (next_new == null) {
                return raise_error(interp, "boundary: failed to allocate cons escape tail");
            }
            next_new.tag = CONS;
            current_new.cons_val.cdr = next_new;
            current_new = next_new;
            current_old = old_cdr;
        } else {
            Value* promoted_cdr = promote_to_escape(old_cdr, interp);
            if (old_cdr != null && (promoted_cdr == null || is_error(promoted_cdr))) {
                return promoted_cdr == null
                    ? raise_error(interp, "boundary: failed to promote cons cdr during escape promotion")
                    : promoted_cdr;
            }
            current_new.cons_val.cdr = promoted_cdr;
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
