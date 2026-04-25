module lisp;

import std::core::mem;
import main;

fn bool method_signature_copy_to_escape(MethodSignature* dst, MethodSignature* src, main::ScopeRegion* scope) {
    if (dst == null || src == null || scope == null) return false;
    method_signature_init_empty(dst);

    if (src.param_count > 0) {
        if (src.param_count > usz.max / TypeId.sizeof ||
            src.param_count > usz.max / SymbolId.sizeof ||
            src.param_count > usz.max / ValueLiteralKey.sizeof ||
            src.param_count > usz.max / bool.sizeof) {
            return false;
        }
        dst.param_types = (TypeId*)scope.alloc_escape_attributed(TypeId.sizeof * src.param_count, main::ScopeEscapeAllocSource.SCOPE_ESCAPE_ALLOC_PROMOTION_SIGNATURE);
        dst.param_type_vars = (SymbolId*)scope.alloc_escape_attributed(SymbolId.sizeof * src.param_count, main::ScopeEscapeAllocSource.SCOPE_ESCAPE_ALLOC_PROMOTION_SIGNATURE);
        dst.val_literals = (ValueLiteralKey*)scope.alloc_escape_attributed(ValueLiteralKey.sizeof * src.param_count, main::ScopeEscapeAllocSource.SCOPE_ESCAPE_ALLOC_PROMOTION_SIGNATURE);
        dst.has_val_literal = (bool*)scope.alloc_escape_attributed(bool.sizeof * src.param_count, main::ScopeEscapeAllocSource.SCOPE_ESCAPE_ALLOC_PROMOTION_SIGNATURE);
        if (dst.param_types == null || dst.param_type_vars == null || dst.val_literals == null || dst.has_val_literal == null) {
            method_signature_init_empty(dst);
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
        if (src.constraint_count > usz.max / MethodConstraint.sizeof) {
            method_signature_init_empty(dst);
            return false;
        }
        dst.constraints = (MethodConstraint*)scope.alloc_escape_attributed(MethodConstraint.sizeof * src.constraint_count, main::ScopeEscapeAllocSource.SCOPE_ESCAPE_ALLOC_PROMOTION_SIGNATURE);
        if (dst.constraints == null) {
            method_signature_init_empty(dst);
            return false;
        }
        for (usz i = 0; i < src.constraint_count; i++) {
            dst.constraints[i] = src.constraints[i];
        }
    }

    dst.param_count = src.param_count;
    dst.constraint_count = src.constraint_count;
    return true;
}

fn Value* promote_escape_array_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    Array* src_arr = v.array_val;
    if (src_arr == null) {
        Value* result = boundary_alloc_value_in_current_escape(interp);
        if (result == null) {
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate array escape copy");
        }
        result.tag = ARRAY;
        result.array_val = null;
        main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    Array* dst_arr = (Array*)mem::malloc(Array.sizeof);
    if (dst_arr == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate array escape copy");
    }
    dst_arr.items = null;
    usz promoted_items = 0;
    bool dst_arr_committed = false;
    defer {
        if (!dst_arr_committed) {
            boundary_cleanup_partial_array_copy(src_arr, dst_arr, promoted_items, interp.current_scope, true);
        }
    }

    usz cap = src_arr.capacity;
    if (cap < src_arr.length) cap = src_arr.length;
    if (cap < 4) cap = 4;

    dst_arr.capacity = cap;
    dst_arr.length = src_arr.length;
    if (cap > usz.max / Value*.sizeof) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate array escape copy");
    }
    dst_arr.items = (Value**)mem::malloc(Value*.sizeof * cap);
    if (dst_arr.items == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate array escape copy");
    }

    for (usz i = 0; i < src_arr.length; i++) {
        Value* promoted = promote_to_escape(src_arr.items[i], interp);
        if (src_arr.items[i] != null && (promoted == null || is_error(promoted))) {
            return promoted == null
                ? boundary_raise(interp, "boundary/copy-fault", "boundary: failed to promote array item during escape promotion")
                : promoted;
        }
        dst_arr.items[i] = promoted;
        promoted_items = i + 1;
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate array escape copy");
    }
    result.tag = ARRAY;
    result.array_val = dst_arr;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    dst_arr_committed = true;
    return result;
}

fn Value* promote_escape_hashmap_backed_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    HashMap* src_map = v.hashmap_val;
    if (src_map == null) {
        Value* result = boundary_alloc_value_in_current_escape(interp);
        if (result == null) {
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate hashmap escape copy");
        }
        result.tag = v.tag;
        result.hashmap_val = null;
        main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    HashMap* dst_map = (HashMap*)mem::malloc(HashMap.sizeof);
    if (dst_map == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate hashmap escape copy");
    }
    dst_map.capacity = src_map.capacity;
    dst_map.count = src_map.count;
    dst_map.mask = src_map.mask;
    dst_map.entries = null;
    uint processed_slots = 0;
    bool dst_map_committed = false;
    defer {
        if (!dst_map_committed) {
            boundary_cleanup_partial_hashmap_copy(src_map, dst_map, processed_slots, interp.current_scope, true);
        }
    }

    if (src_map.capacity > 0) {
        if ((usz)src_map.capacity > usz.max / HashEntry.sizeof) {
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate hashmap escape copy");
        }
        dst_map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * src_map.capacity);
        if (dst_map.entries == null) {
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate hashmap escape copy");
        }

        for (uint i = 0; i < src_map.capacity; i++) {
            HashEntry* src_e = &src_map.entries[i];
            HashEntry* dst_e = &dst_map.entries[i];
            dst_e.key = null;
            dst_e.value = null;
            processed_slots = i + 1;
            if (src_e.key != null) {
                Value* promoted_key = promote_to_escape(src_e.key, interp);
                if (promoted_key == null || is_error(promoted_key)) {
                    return promoted_key == null
                        ? boundary_raise(interp, "boundary/copy-fault", "boundary: failed to promote hashmap key during escape promotion")
                        : promoted_key;
                }
                dst_e.key = promoted_key;
                Value* promoted_value = promote_to_escape(src_e.value, interp);
                if (src_e.value != null && (promoted_value == null || is_error(promoted_value))) {
                    return promoted_value == null
                        ? boundary_raise(interp, "boundary/copy-fault", "boundary: failed to promote hashmap value during escape promotion")
                        : promoted_value;
                }
                dst_e.value = promoted_value;
            }
        }
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate hashmap escape copy");
    }
    result.tag = v.tag;
    result.hashmap_val = dst_map;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    dst_map_committed = true;
    return result;
}

fn Value* promote_escape_method_table_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    MethodTable* src_mt = v.method_table_val;
    if (src_mt == null) {
        Value* result = boundary_alloc_value_in_current_escape(interp);
        if (result == null) {
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate method table escape copy");
        }
        result.tag = METHOD_TABLE;
        result.method_table_val = null;
        main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    MethodTable* dst_mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
    if (dst_mt == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate method table escape copy");
    }
    dst_mt.name = src_mt.name;
    dst_mt.entry_count = src_mt.entry_count;
    dst_mt.capacity = src_mt.capacity;
    if (dst_mt.capacity < dst_mt.entry_count) dst_mt.capacity = dst_mt.entry_count;
    dst_mt.entries = null;
    dst_mt.fallback = null;
    usz promoted_entries = 0;
    bool promoted_fallback_ready = false;
    bool dst_mt_committed = false;
    defer {
        if (!dst_mt_committed) {
            boundary_cleanup_partial_method_table_copy(
                src_mt,
                dst_mt,
                promoted_entries,
                promoted_fallback_ready,
                interp.current_scope,
                true
            );
        }
    }

    if (dst_mt.capacity > 0) {
        if (dst_mt.capacity > usz.max / MethodEntry.sizeof) {
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate method table escape copy");
        }
        dst_mt.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * dst_mt.capacity);
        if (dst_mt.entries == null) {
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate method table escape copy");
        }
    }

    for (usz i = 0; i < dst_mt.entry_count; i++) {
        method_signature_init_empty(&dst_mt.entries[i].sig);
        dst_mt.entries[i].implementation = null;
        if (!method_signature_copy_to_heap(&dst_mt.entries[i].sig, &src_mt.entries[i].sig)) {
            return boundary_raise(interp, "boundary/copy-fault", "boundary: failed to copy method signature during escape promotion");
        }
        promoted_entries = i + 1;
        Value* promoted_impl = promote_to_escape(src_mt.entries[i].implementation, interp);
        if (src_mt.entries[i].implementation != null && (promoted_impl == null || is_error(promoted_impl))) {
            return promoted_impl == null
                ? boundary_raise(interp, "boundary/copy-fault", "boundary: failed to promote method implementation during escape promotion")
                : promoted_impl;
        }
        dst_mt.entries[i].implementation = promoted_impl;
    }

    Value* promoted_fallback = promote_to_escape(src_mt.fallback, interp);
    if (src_mt.fallback != null && (promoted_fallback == null || is_error(promoted_fallback))) {
        return promoted_fallback == null
            ? boundary_raise(interp, "boundary/copy-fault", "boundary: failed to promote method fallback during escape promotion")
            : promoted_fallback;
    }
    dst_mt.fallback = promoted_fallback;
    promoted_fallback_ready = true;

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate method table escape copy");
    }
    result.tag = METHOD_TABLE;
    result.method_table_val = dst_mt;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    dst_mt_committed = true;
    return result;
}

fn bool promote_escape_closure_env_needs_detach(Value* v, Interp* interp) @inline {
    if (v == null || interp == null || v.tag != CLOSURE || v.closure_val == null) return false;
    if (v.closure_val.env == null) return false;
    if (interp.releasing_scope != null &&
        env_chain_has_frame_in_scope(v.closure_val.env, interp.releasing_scope, interp.global_env)) {
        return true;
    }
    if (v.closure_val.env_scope != null) {
        return !boundary_ptr_in_scope((void*)v.closure_val.env, v.closure_val.env_scope);
    }
    if (interp.current_scope == null) return false;
    return env_chain_has_frame_in_scope(v.closure_val.env, interp.current_scope, interp.global_env);
}

fn bool promote_escape_should_iterate_cons_tail(Value* old_cdr, Interp* interp) @inline {
    if (old_cdr == null || old_cdr.tag != CONS) return false;
    // Keep long disjoint or alias-unsafe list tails iterative to avoid recursive
    // depth blowups under default or ASAN stack pressure.
    if (boundary_value_in_releasing_scope(old_cdr, interp)) return true;
    if (old_cdr.scope_gen == interp.current_scope.generation) return true;
    if (boundary_ptr_in_target_scope_chain_with_hint((void*)old_cdr, interp, old_cdr.scope_gen)) {
        return boundary_graph_alias_unsafe_for_reuse(old_cdr, interp);
    }
    return true;
}

fn Value* promote_escape_cons_abort_original(Value* original, Value* head, Interp* interp) @inline {
    boundary_cleanup_materialized_value(original, head, interp.current_scope, true);
    return original;
}

fn void promote_escape_abort_budget(PromotionContext* ctx) @inline {
    if (ctx == null) return;
    ctx.aborted = true;
    ctx.abort_reason = PROMOTION_ABORT_BUDGET_EXHAUSTED;
}

fn void promote_escape_cleanup_partial_closure(Closure* cloned) @inline {
    if (cloned == null || cloned.env_scope == null) return;
    main::scope_release(cloned.env_scope);
    cloned.env_scope = null;
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
            boundary_cleanup_materialized_value(v, head, interp.current_scope, true);
            return promoted_car == null
                ? boundary_raise(interp, "boundary/copy-fault", "boundary: failed to promote cons car during escape promotion")
                : promoted_car;
        }
        if (head == null) {
            head = boundary_alloc_value_in_current_escape(interp);
            if (head == null) {
                boundary_cleanup_materialized_value(
                    current_old.cons_val.car,
                    promoted_car,
                    interp.current_scope,
                    true
                );
                return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate cons escape copy");
            }
            head.tag = CONS;
            head.cons_val.car = null;
            head.cons_val.cdr = null;
            current_new = head;
        }
        current_new.cons_val.car = promoted_car;
        if (ctx != null && ctx.aborted) {
            return promote_escape_cons_abort_original(v, head, interp);
        }
        Value* old_cdr = current_old.cons_val.cdr;
        if (promote_escape_should_iterate_cons_tail(old_cdr, interp)) {
            if (!promote_escape_can_mutate(v, ctx)) {
                return promote_escape_cons_abort_original(v, head, interp);
            }
            Value* next_new = boundary_alloc_value_in_current_escape(interp);
            if (next_new == null) {
                boundary_cleanup_materialized_value(v, head, interp.current_scope, true);
                return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate cons escape tail");
            }
            next_new.tag = CONS;
            next_new.cons_val.car = null;
            next_new.cons_val.cdr = null;
            current_new.cons_val.cdr = next_new;
            current_new = next_new;
            current_old = old_cdr;
        } else {
            Value* promoted_cdr = promote_to_escape(old_cdr, interp);
            if (old_cdr != null && (promoted_cdr == null || is_error(promoted_cdr))) {
                boundary_cleanup_materialized_value(v, head, interp.current_scope, true);
                return promoted_cdr == null
                    ? boundary_raise(interp, "boundary/copy-fault", "boundary: failed to promote cons cdr during escape promotion")
                    : promoted_cdr;
            }
            current_new.cons_val.cdr = promoted_cdr;
            if (ctx != null && ctx.aborted) {
                return promote_escape_cons_abort_original(v, head, interp);
            }
            break;
        }
    }
    return head;
}

fn Value* promote_escape_closure(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Closure* cloned = (Closure*)interp.current_scope.alloc_escape_attributed(
        Closure.sizeof,
        main::ScopeEscapeAllocSource.SCOPE_ESCAPE_ALLOC_PROMOTION_CLOSURE
    );
    if (cloned == null) {
        promote_escape_abort_budget(ctx);
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate closure escape copy");
    }
    *cloned = *v.closure_val;
    cloned.scope_gen = interp.current_scope.escape_generation;
    cloned.params = null;
    cloned.type_sig = null;
    cloned.env_scope = null;

    if (v.closure_val.param_count > 0) {
        if (v.closure_val.param_count > usz.max / SymbolId.sizeof) {
            promote_escape_abort_budget(ctx);
            promote_escape_cleanup_partial_closure(cloned);
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate closure params during escape promotion");
        }
        cloned.params = (SymbolId*)interp.current_scope.alloc_escape_attributed(
            SymbolId.sizeof * v.closure_val.param_count,
            main::ScopeEscapeAllocSource.SCOPE_ESCAPE_ALLOC_PROMOTION_CLOSURE
        );
        if (cloned.params == null) {
            promote_escape_abort_budget(ctx);
            promote_escape_cleanup_partial_closure(cloned);
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate closure params during escape promotion");
        }
        for (usz i = 0; i < v.closure_val.param_count; i++) {
            cloned.params[i] = v.closure_val.params[i];
        }
    }

    if (v.closure_val.type_sig != null) {
        MethodSignature* cloned_sig = (MethodSignature*)interp.current_scope.alloc_escape_attributed(
            MethodSignature.sizeof,
            main::ScopeEscapeAllocSource.SCOPE_ESCAPE_ALLOC_PROMOTION_CLOSURE
        );
        if (cloned_sig == null ||
            !method_signature_copy_to_escape(cloned_sig, v.closure_val.type_sig, interp.current_scope)) {
            promote_escape_abort_budget(ctx);
            promote_escape_cleanup_partial_closure(cloned);
            return boundary_raise(interp, "boundary/copy-fault", (String)boundary_copy_fault_message(BOUNDARY_COPY_FAULT_CLOSURE_TYPE_SIG_COPY));
        }
        cloned.type_sig = cloned_sig;
    }

    if (promote_escape_closure_env_needs_detach(v, interp)) {
        main::ScopeRegion* env_scope = main::scope_create(null);
        if (env_scope == null) {
            promote_escape_abort_budget(ctx);
            promote_escape_cleanup_partial_closure(cloned);
            return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate detached closure env scope");
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
            promote_escape_cleanup_partial_closure(cloned);
            return boundary_raise(interp, "boundary/copy-fault", "boundary: failed to detach closure env during escape promotion");
        }
        cloned.env = copied.env;
        cloned.env_scope = env_scope;
    } else if (closure_env_scope_is_root_owned(v, interp)) {
        cloned.env_scope = null;
    } else if (v.closure_val.env_scope != null) {
        main::scope_retain(v.closure_val.env_scope);
        cloned.env_scope = v.closure_val.env_scope;
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        promote_escape_abort_budget(ctx);
        promote_escape_cleanup_partial_closure(cloned);
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to allocate closure escape copy");
    }
    result.tag = CLOSURE;
    result.closure_val = cloned;
    if (!main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_closure)) {
        promote_escape_abort_budget(ctx);
        promote_escape_cleanup_partial_closure(cloned);
        return boundary_raise(interp, "boundary/out-of-memory", "boundary: failed to register closure escape destructor");
    }
    return result;
}
