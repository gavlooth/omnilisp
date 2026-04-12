module lisp;

import main;

// =============================================================================
// SECTION 2.4: FRAME PUSH/POP (copy-to-parent core)
// =============================================================================

enum CopyParentRoute : char {
    CP_ROUTE_NIL,
    CP_ROUTE_VOID,
    CP_ROUTE_INT,
    CP_ROUTE_DOUBLE,
    CP_ROUTE_STRING,
    CP_ROUTE_SYMBOL,
    CP_ROUTE_CONS,
    CP_ROUTE_CLOSURE,
    CP_ROUTE_PRIMITIVE,
    CP_ROUTE_PARTIAL_PRIM,
    CP_ROUTE_CONTINUATION,
    CP_ROUTE_SHARED_WRAPPER,
    CP_ROUTE_INSTANCE,
    CP_ROUTE_FFI_HANDLE,
    CP_ROUTE_TYPE_INFO,
    CP_ROUTE_ITERATOR,
    CP_ROUTE_COROUTINE,
    CP_ROUTE_MODULE,
    CP_ROUTE_ERROR,
    CP_ROUTE_TIME_POINT,
}

fn CopyParentRoute copy_parent_route_for_tag(ValueTag tag) {
    switch (tag) {
        case NIL:
            return CP_ROUTE_NIL;
        case VOID:
            return CP_ROUTE_VOID;
        case INT:
            return CP_ROUTE_INT;
        case DOUBLE:
            return CP_ROUTE_DOUBLE;
        case STRING:
            return CP_ROUTE_STRING;
        case SYMBOL:
            return CP_ROUTE_SYMBOL;
        case CONS:
            return CP_ROUTE_CONS;
        case CLOSURE:
            return CP_ROUTE_CLOSURE;
        case PRIMITIVE:
            return CP_ROUTE_PRIMITIVE;
        case PARTIAL_PRIM:
            return CP_ROUTE_PARTIAL_PRIM;
        case CONTINUATION:
            return CP_ROUTE_CONTINUATION;
        case HASHMAP:
        case SET:
        case ARRAY:
        case METHOD_TABLE:
            return CP_ROUTE_SHARED_WRAPPER;
        case INSTANCE:
            return CP_ROUTE_INSTANCE;
        case FFI_HANDLE:
            return CP_ROUTE_FFI_HANDLE;
        case TYPE_INFO:
            return CP_ROUTE_TYPE_INFO;
        case ITERATOR:
            return CP_ROUTE_ITERATOR;
        case COROUTINE:
            return CP_ROUTE_COROUTINE;
        case MODULE:
            return CP_ROUTE_MODULE;
        case ERROR:
            return CP_ROUTE_ERROR;
        case TIME_POINT:
            return CP_ROUTE_TIME_POINT;
    }
}

fn Value* copy_to_parent_by_route(
    Value* v,
    Interp* interp,
    CopyParentRoute route,
    BoundaryCopyFault* fault_out = null
) {
    switch (route) {
        case CP_ROUTE_NIL:
            return make_nil(interp);
        case CP_ROUTE_VOID:
            return make_void(interp);
        case CP_ROUTE_INT:
            return make_int(interp, v.int_val);
        case CP_ROUTE_DOUBLE:
            return make_double(interp, v.double_val);
        case CP_ROUTE_STRING:
            return make_string(interp, v.str_chars[:v.str_len]);
        case CP_ROUTE_SYMBOL:
            return make_symbol(interp, v.sym_val);
        case CP_ROUTE_CONS:
            return copy_cons_to_parent(v, interp, fault_out);
        case CP_ROUTE_CLOSURE:
            return copy_closure_to_parent(v, interp, fault_out);
        case CP_ROUTE_PRIMITIVE:
            return copy_primitive_to_parent(v, interp, fault_out);
        case CP_ROUTE_PARTIAL_PRIM:
            // Deep copy: Value wrapper may be in child scope, and first_arg/second_arg may too.
            return copy_partial_prim_to_parent(v, interp, fault_out);
        case CP_ROUTE_CONTINUATION:
            // Shallow copy: Value wrapper may be in child scope, cont_val itself is interpreter-lifetime.
            return copy_continuation_to_parent(v, interp);
        case CP_ROUTE_SHARED_WRAPPER:
            // Shared wrappers must be cloned when they are not already reusable
            // in the target chain. This prevents aliasing disjoint collection
            // wrappers after reuse is declined.
            return copy_shared_wrapper_to_parent(v, interp, fault_out);
        case CP_ROUTE_INSTANCE:
            return copy_instance_to_parent(v, interp);
        case CP_ROUTE_FFI_HANDLE:
            return copy_ffi_handle_to_parent(v, interp);
        case CP_ROUTE_TYPE_INFO:
            // Type-info entries live in type registry lifetime.
            return v;
        case CP_ROUTE_ITERATOR:
            // Promote underlying closure thunk so iterator outlives child scope.
            return copy_iterator_to_parent(v, interp, fault_out);
        case CP_ROUTE_COROUTINE:
            // Transfer ownership into a single surviving wrapper.
            return copy_coroutine_to_parent(v, interp);
        case CP_ROUTE_MODULE:
            // Module lives in interpreter module table.
            return v;
        case CP_ROUTE_ERROR:
            return make_error(interp, v.str_chars[:v.str_len]);
        case CP_ROUTE_TIME_POINT:
            return copy_time_point_to_parent(v, interp);
    }
}

<*
@require interp != null : "copy_to_parent: null interp"
*>
fn Value* copy_to_parent(Value* v, Interp* interp) {
    return copy_to_parent_with_fault(v, interp, null);
}

fn Value* copy_to_parent_with_fault(
    Value* v,
    Interp* interp,
    BoundaryCopyFault* copy_fault_out
) @inline {
    if (copy_fault_out != null) *copy_fault_out = BOUNDARY_COPY_FAULT_NONE;
    if (v == null) return null;
    g_copy_to_parent_stats.total_calls++;
    copy_to_parent_note_tag(v.tag);
    if (interp.current_scope == null || interp.releasing_scope == null) {
        g_copy_to_parent_stats.defensive_copies++;
        return copy_to_parent_missing_scope_provenance_error(interp);
    }

    PromotionContext* active_ctx = interp.active_promotion_ctx;
    if (active_ctx != null) {
        Value* cached = promotion_context_lookup(active_ctx, v);
        if (cached != null) return cached;
    }

    // Scope-aware optimization: if value is NOT in the scope being released,
    // reuse only when it is already in the target scope chain.
    // Note: uses is_in_scope (chunk walk) rather than scope_gen stamp because
    // chunk-transfer paths (scope_splice_escapes) move allocations without
    // rewriting per-value scope_gen stamps.
    if (active_ctx == null) {
        Value* fast_reuse = null;
        if (copy_to_parent_try_fast_reuse(v, interp, &fast_reuse)) return fast_reuse;
    }
    copy_to_parent_note_cons_spine(v);

    CopyParentRoute route = copy_parent_route_for_tag(v.tag);
    return copy_to_parent_by_route(v, interp, route, copy_fault_out);
}

fn Value* copy_to_parent_site_checked(Value* v, Interp* interp, uint site, BoundaryCopyFault* copy_fault_out) @inline {
    if (v == null) return null;
    if (copy_fault_out != null) *copy_fault_out = BOUNDARY_COPY_FAULT_NONE;
    if (site < COPY_SITE_COUNT) g_copy_to_parent_stats.site_calls[(usz)site]++;
    return copy_to_parent_with_fault(v, interp, copy_fault_out);
}

fn Value* copy_to_parent_site(Value* v, Interp* interp, uint site) {
    if (site < COPY_SITE_COUNT) g_copy_to_parent_stats.site_calls[(usz)site]++;
    return copy_to_parent(v, interp);
}
