module lisp;

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

fn bool promote_to_escape_fast_path(Value* v, Interp* interp, Value** out) {
    if (v.tag == CONS && v.scope_gen == interp.current_scope.escape_generation) {
        *out = v;
        return true;
    }

    bool unsafe_alias = boundary_graph_alias_unsafe_for_reuse(v, interp);

    if (v.scope_gen == interp.current_scope.escape_generation) {
        if (unsafe_alias) {
            *out = promote_to_escape_disjoint(v, interp);
            return true;
        }
        *out = v;
        return true;
    }
    if (v.scope_gen != interp.current_scope.generation) {
        if (boundary_ptr_in_target_scope_chain_with_hint((void*)v, interp, v.scope_gen) && !unsafe_alias) {
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


enum PromoteEscapeRoute : char {
    PE_ROUTE_SCALAR,
    PE_ROUTE_BIG_INTEGER,
    PE_ROUTE_BIG_FLOAT,
    PE_ROUTE_BIG_COMPLEX,
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
    PE_ROUTE_TENSOR,
}

fn PromoteEscapeRoute promote_escape_route_for_tag(ValueTag tag) {
    switch (tag) {
        case NIL:
        case VOID:
        case INT:
        case DOUBLE:
        case SYMBOL:
            return PE_ROUTE_SCALAR;
        case BIG_INTEGER:
            return PE_ROUTE_BIG_INTEGER;
        case BIG_FLOAT:
            return PE_ROUTE_BIG_FLOAT;
        case BIG_COMPLEX:
            return PE_ROUTE_BIG_COMPLEX;
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
        case TENSOR:
            return PE_ROUTE_TENSOR;
        case PRIMITIVE:
        case HASHMAP:
        case SET:
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
        case PE_ROUTE_BIG_INTEGER:
            return promote_escape_big_integer(v, interp, ctx);
        case PE_ROUTE_BIG_FLOAT:
            return promote_escape_big_float(v, interp, ctx);
        case PE_ROUTE_BIG_COMPLEX:
            return promote_escape_big_complex(v, interp, ctx);
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
        case PE_ROUTE_TENSOR:
            return promote_escape_tensor_shared_wrapper(v, interp, ctx);
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
