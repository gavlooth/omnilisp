module lisp;

import std::io;
import main;

// =============================================================================
// SECTION 2.4: FRAME PUSH/POP (escape/root promotion)
// =============================================================================
fn Value* promote_to_escape_disjoint(Value* v, Interp* interp) {
    PromotionContext* ctx = interp.active_promotion_ctx;
    Value* result = promote_to_escape_by_tag(v, interp, ctx);
    if (ctx != null && ctx.aborted) return v;
    promote_to_escape_memo(v, result, interp, ctx);
    return result;
}

fn bool promote_escape_can_mutate(Value* v, PromotionContext* ctx) @inline {
    (void)v;
    return promotion_context_consume(ctx, 1);
}

fn bool promote_escape_should_iterate_cons_tail(Value* old_cdr, Interp* interp) @inline {
    if (old_cdr == null || old_cdr.tag != CONS) return false;
    // Keep long disjoint list promotion iterative to avoid recursive depth blowups
    // under ASAN stack pressure. Reusable target-chain tails still short-circuit.
    if (old_cdr.scope_gen == interp.current_scope.generation) return true;
    if (boundary_ptr_in_target_scope_chain_with_hint((void*)old_cdr, interp, old_cdr.scope_gen)) return false;
    return true;
}

fn Value* promote_escape_string_or_error(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = v.tag;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    usz len = v.str_len;
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    result.str_chars = (char*)mem::malloc(cap);
    for (usz i = 0; i < len; i++) {
        result.str_chars[i] = v.str_chars[i];
    }
    result.str_chars[len] = 0;
    result.str_len = len;
    return result;
}

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
            result.closure_val.type_sig = null;
        }
    }

    if (result.closure_val.env_scope != null) {
        main::scope_retain(result.closure_val.env_scope);
    }
    return result;
}

fn Value* promote_escape_partial_prim(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = PARTIAL_PRIM;
    result.partial_val.func = v.partial_val.func;
    result.partial_val.remaining = v.partial_val.remaining;
    result.partial_val.first_arg = promote_to_escape(v.partial_val.first_arg, interp);
    result.partial_val.second_arg = promote_to_escape(v.partial_val.second_arg, interp);
    return result;
}

fn Value* promote_escape_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    *result = *v;
    result.scope_gen = interp.current_scope.escape_generation;
    return result;
}

fn Value* promote_escape_instance(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = INSTANCE;
    result.scope_gen = interp.current_scope.escape_generation;
    result.instance_val = v.instance_val;
    instance_retain(v.instance_val);
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_ffi_handle(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = FFI_HANDLE;
    result.ffi_val = v.ffi_val;
    ffi_handle_retain(result.ffi_val);
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn bool promote_to_escape_fast_path(Value* v, Interp* interp, Value** out) {
    if (v.scope_gen == interp.current_scope.escape_generation) {
        *out = v;
        return true;
    }
    if (v.scope_gen != interp.current_scope.generation) {
        if (boundary_ptr_in_target_scope_chain_with_hint((void*)v, interp, v.scope_gen)) {
            *out = v;
            return true;
        }
        // Explicit disjoint-lifetime route (e.g. standalone env_scope object):
        // route through TEMP copy first, then continue promotion into ESCAPE.
        *out = promote_to_escape_disjoint(v, interp);
        return true;
    }
    return false;
}

fn Value* promote_escape_scalar(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    switch (v.tag) {
        case NIL:
            result.tag = NIL;
        case INT:
            result.tag = INT;
            result.int_val = v.int_val;
        case DOUBLE:
            result.tag = DOUBLE;
            result.double_val = v.double_val;
        case SYMBOL:
            result.tag = SYMBOL;
            result.sym_val = v.sym_val;
        default:
            return v;
    }
    return result;
}

fn Value* promote_escape_continuation(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = CONTINUATION;
    result.cont_val = v.cont_val;
    return result;
}

fn Value* promote_escape_iterator(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = ITERATOR;
    result.iterator_val = promote_to_escape(v.iterator_val, interp);
    return result;
}

fn Value* promote_escape_coroutine(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = COROUTINE;
    result.coroutine_val = v.coroutine_val;
    return result;
}

fn Value* promote_escape_time_point(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;
    Value* result = interp.alloc_value_escape();
    result.tag = TIME_POINT;
    result.time_point_val = v.time_point_val;
    return result;
}

enum PromoteEscapeRoute : char {
    PE_ROUTE_SCALAR,
    PE_ROUTE_STRING_OR_ERROR,
    PE_ROUTE_CONS,
    PE_ROUTE_CLOSURE,
    PE_ROUTE_PARTIAL_PRIM,
    PE_ROUTE_CONTINUATION,
    PE_ROUTE_ITERATOR,
    PE_ROUTE_COROUTINE,
    PE_ROUTE_SHARED_WRAPPER,
    PE_ROUTE_INSTANCE,
    PE_ROUTE_FFI_HANDLE,
    PE_ROUTE_TIME_POINT,
}

fn PromoteEscapeRoute promote_escape_route_for_tag(ValueTag tag) {
    switch (tag) {
        case NIL:
        case INT:
        case DOUBLE:
        case SYMBOL:
            return PE_ROUTE_SCALAR;
        case STRING:
        case ERROR:
            return PE_ROUTE_STRING_OR_ERROR;
        case CONS:
            return PE_ROUTE_CONS;
        case CLOSURE:
            return PE_ROUTE_CLOSURE;
        case PARTIAL_PRIM:
            return PE_ROUTE_PARTIAL_PRIM;
        case CONTINUATION:
            return PE_ROUTE_CONTINUATION;
        case ITERATOR:
            return PE_ROUTE_ITERATOR;
        case COROUTINE:
            return PE_ROUTE_COROUTINE;
        case TIME_POINT:
            return PE_ROUTE_TIME_POINT;
        case PRIMITIVE:
        case HASHMAP:
        case ARRAY:
        case METHOD_TABLE:
        case TYPE_INFO:
        case MODULE:
            return PE_ROUTE_SHARED_WRAPPER;
        case INSTANCE:
            return PE_ROUTE_INSTANCE;
        case FFI_HANDLE:
            return PE_ROUTE_FFI_HANDLE;
    }
}

<*
@require v != null : "promote_to_escape_by_route: null value"
@require interp != null : "promote_to_escape_by_route: null interp"
*>
fn Value* promote_to_escape_by_route(
    Value* v,
    Interp* interp,
    PromotionContext* ctx,
    PromoteEscapeRoute route
) {
    switch (route) {
        case PE_ROUTE_SCALAR:
            return promote_escape_scalar(v, interp, ctx);
        case PE_ROUTE_STRING_OR_ERROR:
            return promote_escape_string_or_error(v, interp, ctx);
        case PE_ROUTE_CONS:
            return promote_escape_cons(v, interp, ctx);
        case PE_ROUTE_CLOSURE:
            return promote_escape_closure(v, interp, ctx);
        case PE_ROUTE_PARTIAL_PRIM:
            return promote_escape_partial_prim(v, interp, ctx);
        case PE_ROUTE_CONTINUATION:
            return promote_escape_continuation(v, interp, ctx);
        case PE_ROUTE_ITERATOR:
            return promote_escape_iterator(v, interp, ctx);
        case PE_ROUTE_COROUTINE:
            return promote_escape_coroutine(v, interp, ctx);
        case PE_ROUTE_TIME_POINT:
            return promote_escape_time_point(v, interp, ctx);
        case PE_ROUTE_SHARED_WRAPPER:
            // Wrapper promotion into ESCAPE lane; backing data/ownership remains unchanged.
            return promote_escape_shared_wrapper(v, interp, ctx);
        case PE_ROUTE_INSTANCE:
            return promote_escape_instance(v, interp, ctx);
        case PE_ROUTE_FFI_HANDLE:
            return promote_escape_ffi_handle(v, interp, ctx);
    }
}

fn Value* promote_to_escape_by_tag(Value* v, Interp* interp, PromotionContext* ctx) {
    PromoteEscapeRoute route = promote_escape_route_for_tag(v.tag);
    return promote_to_escape_by_route(v, interp, ctx, route);
}

fn void promote_to_escape_memo(Value* old_v, Value* new_v, Interp* interp, PromotionContext* ctx) {
    if (ctx == null || new_v == null || new_v == old_v) return;
    promotion_context_remember(ctx, old_v, new_v, interp);
}

/**
 * Promote a value from current scope TEMP lane to current scope ESCAPE lane.
 * This is an intra-scope promotion primitive for dual-lane rollout.
 * Values not owned by current TEMP lane are returned unchanged.
 */
fn Value* promote_to_escape(Value* v, Interp* interp) {
    if (v == null || interp == null || interp.current_scope == null) return v;
    PromotionContext* ctx = interp.active_promotion_ctx;
    if (ctx != null && ctx.aborted) return v;

    // Fast paths:
    // - already in current ESCAPE lane
    // - in current TEMP lane (normal case for lane promotion)
    // - in a surviving scope chain (root/parent/shared lifetime)
    Value* fast_path = null;
    if (promote_to_escape_fast_path(v, interp, &fast_path)) return fast_path;

    Value* result = promote_to_escape_by_tag(v, interp, ctx);
    promote_to_escape_memo(v, result, interp, ctx);
    return result;
}

fn Value* root_store_clone_primitive_to_scope(
    Value* src,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (src == null || interp == null || target_scope == null) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* result = interp.alloc_value_escape();
    result.tag = PRIMITIVE;
    result.prim_val = null;

    Primitive* src_prim = src.prim_val;
    if (src_prim == null) {
        main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    Primitive* dst_prim = (Primitive*)mem::malloc(Primitive.sizeof);
    if (dst_prim == null) return null;
    *dst_prim = *src_prim;

    result.prim_val = dst_prim;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* root_store_clone_array_to_scope(
    Value* src,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (src == null || interp == null || target_scope == null) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);

    Value* result = interp.alloc_value_escape();
    result.tag = ARRAY;
    result.array_val = null;

    Array* src_arr = src.array_val;
    if (src_arr == null) {
        main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    Array* dst_arr = (Array*)mem::malloc(Array.sizeof);
    if (dst_arr == null) return null;

    usz cap = src_arr.capacity;
    if (cap < src_arr.length) cap = src_arr.length;
    if (cap < 4) cap = 4;

    dst_arr.capacity = cap;
    dst_arr.length = src_arr.length;
    dst_arr.items = (Value**)mem::malloc(Value*.sizeof * cap);
    if (dst_arr.items == null) {
        mem::free(dst_arr);
        return null;
    }

    for (usz i = 0; i < src_arr.length; i++) {
        dst_arr.items[i] = boundary_copy_to_parent_site_ctx(
            src_arr.items[i],
            interp,
            COPY_SITE_PROMOTE_TO_ROOT,
            &copy_ctx
        );
    }

    result.array_val = dst_arr;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* root_store_clone_hashmap_to_scope(
    Value* src,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (src == null || interp == null || target_scope == null) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);

    Value* result = interp.alloc_value_escape();
    result.tag = HASHMAP;
    result.hashmap_val = null;

    HashMap* src_map = src.hashmap_val;
    if (src_map == null) {
        main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    HashMap* dst_map = (HashMap*)mem::malloc(HashMap.sizeof);
    if (dst_map == null) return null;
    dst_map.capacity = src_map.capacity;
    dst_map.count = src_map.count;
    dst_map.mask = src_map.mask;
    dst_map.entries = null;

    if (src_map.capacity > 0) {
        dst_map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * src_map.capacity);
        if (dst_map.entries == null) {
            mem::free(dst_map);
            return null;
        }

        for (uint i = 0; i < src_map.capacity; i++) {
            HashEntry* src_e = &src_map.entries[i];
            HashEntry* dst_e = &dst_map.entries[i];
            dst_e.key = null;
            dst_e.value = null;
            if (src_e.key != null) {
                dst_e.key = boundary_copy_to_parent_site_ctx(
                    src_e.key,
                    interp,
                    COPY_SITE_PROMOTE_TO_ROOT,
                    &copy_ctx
                );
                dst_e.value = boundary_copy_to_parent_site_ctx(
                    src_e.value,
                    interp,
                    COPY_SITE_PROMOTE_TO_ROOT,
                    &copy_ctx
                );
            }
        }
    }

    result.hashmap_val = dst_map;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn void root_store_free_method_table_partial(MethodTable* mt, usz copied_entries) {
    if (mt == null) return;
    if (mt.entries != null) {
        usz limit = copied_entries;
        if (limit > mt.entry_count) limit = mt.entry_count;
        for (usz i = 0; i < limit; i++) {
            method_signature_destroy_heap(&mt.entries[i].sig);
        }
        mem::free(mt.entries);
        mt.entries = null;
    }
    mem::free(mt);
}

fn Value* root_store_clone_method_table_to_scope(
    Value* src,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (src == null || interp == null || target_scope == null) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);

    Value* result = interp.alloc_value_escape();
    result.tag = METHOD_TABLE;
    result.method_table_val = null;

    MethodTable* src_mt = src.method_table_val;
    if (src_mt == null) {
        main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    MethodTable* dst_mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
    if (dst_mt == null) return null;
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
            return null;
        }
    }

    usz copied_entries = 0;
    for (usz i = 0; i < dst_mt.entry_count; i++) {
        method_signature_init_empty(&dst_mt.entries[i].sig);
        if (!method_signature_copy_to_heap(&dst_mt.entries[i].sig, &src_mt.entries[i].sig)) {
            root_store_free_method_table_partial(dst_mt, copied_entries);
            return null;
        }
        dst_mt.entries[i].implementation = boundary_copy_to_parent_site_ctx(
            src_mt.entries[i].implementation,
            interp,
            COPY_SITE_PROMOTE_TO_ROOT,
            &copy_ctx
        );
        copied_entries++;
    }

    dst_mt.fallback = boundary_copy_to_parent_site_ctx(
        src_mt.fallback,
        interp,
        COPY_SITE_PROMOTE_TO_ROOT,
        &copy_ctx
    );

    result.method_table_val = dst_mt;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* root_store_direct_promote_to_scope(
    Value* v,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);
    return boundary_copy_to_parent_site_ctx(v, interp, COPY_SITE_PROMOTE_TO_ROOT, &copy_ctx);
}

fn Value* root_store_route_to_scope(
    Value* v,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (v == null || interp == null || target_scope == null) return null;

    switch (v.tag) {
        case PRIMITIVE: {
            Value* cloned = root_store_clone_primitive_to_scope(
                v,
                interp,
                target_scope,
                releasing_scope
            );
            if (cloned == null) return raise_error(interp, "root-store: failed to clone primitive payload");
            return cloned;
        }
        case ARRAY: {
            Value* cloned = root_store_clone_array_to_scope(v, interp, target_scope, releasing_scope);
            if (cloned == null) return raise_error(interp, "root-store: failed to clone array payload");
            return cloned;
        }
        case HASHMAP: {
            Value* cloned = root_store_clone_hashmap_to_scope(v, interp, target_scope, releasing_scope);
            if (cloned == null) return raise_error(interp, "root-store: failed to clone hashmap payload");
            return cloned;
        }
        case METHOD_TABLE: {
            Value* cloned = root_store_clone_method_table_to_scope(
                v,
                interp,
                target_scope,
                releasing_scope
            );
            if (cloned == null) return raise_error(interp, "root-store: failed to clone method-table payload");
            return cloned;
        }
        case NIL:
        case INT:
        case STRING:
        case SYMBOL:
        case CONS:
        case CLOSURE:
        case CONTINUATION:
        case PARTIAL_PRIM:
        case ERROR:
        case DOUBLE:
        case FFI_HANDLE:
        case TYPE_INFO:
        case INSTANCE:
        case MODULE:
        case ITERATOR:
        case COROUTINE:
        case TIME_POINT: {
            Value* promoted = root_store_direct_promote_to_scope(
                v,
                interp,
                target_scope,
                releasing_scope
            );
            if (promoted != null) return promoted;
            return raise_error(interp, "root-store: destination promotion returned null");
        }
    }
}

// Boundary helper: promote a child-scope value into root using explicit
// transfer-safe routing:
// - releasing-owned + transfer-safe tags -> direct destination ESCAPE promotion
// - unsupported payload classes -> deterministic hard error
fn Value* promote_to_root_site(Value* v, Interp* interp, uint site) {
    if (v == null || interp == null || interp.current_scope == interp.root_scope) return v;
    (void)site;
    main::ScopeRegion* target_scope = interp.root_scope;
    main::ScopeRegion* releasing_scope = interp.current_scope;
    BoundaryReturnClassification c =
        boundary_classify_return_value(v, interp, target_scope, releasing_scope);
    switch (c.provenance) {
        case BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN:
            return v;
        case BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP:
        case BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE:
        case BOUNDARY_RETURN_MIXED_UNCERTAIN:
            return root_store_route_to_scope(v, interp, target_scope, releasing_scope);
    }
}

/**
 * Promote a value to root_scope so it survives child scope release.
 * No-op when already in root_scope (fast path for stdlib loading, etc.).
 * Used when storing values into root-scope data structures (hashmap, array).
 */
fn Value* promote_to_root(Value* v, Interp* interp) {
    return promote_to_root_site(v, interp, COPY_SITE_PROMOTE_TO_ROOT);
}

// Copy a value into an arbitrary target scope, preserving releasing_scope context.
