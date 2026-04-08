module lisp;

import main;

<*
@require partial != null : "boundary_build_destination_partial_escape: null partial"
@require interp != null : "boundary_build_destination_partial_escape: null interp"
@require target_scope != null : "boundary_build_destination_partial_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_partial_escape: null releasing_scope"
@ensure return == partial || return.scope_gen == target_scope.escape_generation || boundary_ptr_in_scope((void*)return, target_scope) : "boundary_build_destination_partial_escape: expected ESCAPE-lane partial wrapper"
*>
fn Value* boundary_build_destination_partial_escape(
    Value* partial,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (partial.tag != PARTIAL_PRIM) return partial;
    if (ctx != null && ctx.aborted) return partial;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    main::ScopeRegion* build_scope = boundary_destination_build_scope_begin(
        interp,
        target_scope,
        releasing_scope
    );
    if (build_scope == null) return partial;
    defer boundary_destination_build_scope_abort(&build_scope);
    if (ctx != null && ctx.aborted) return partial;

    Value* escaped_first = partial.partial_val.first_arg;
    if (escaped_first != null &&
        !boundary_ptr_in_scope((void*)escaped_first, releasing_scope) &&
        boundary_ptr_in_scope_chain_with_hint(
            (void*)escaped_first,
            target_scope,
            escaped_first.scope_gen
        )) {
        // Preserve pointer identity for payloads already routed into the target
        // chain. The destination wrapper is still rebuilt below.
    } else {
        Value* routed_first = boundary_copy_to_parent_site_ctx(
            partial.partial_val.first_arg,
            interp,
            COPY_SITE_GENERIC,
            ctx
        );
        escaped_first = boundary_destination_promote_routed_escape(
            routed_first,
            interp,
            build_scope,
            ctx
        );
        if (ctx != null && ctx.aborted) return partial;
        if (escaped_first != null &&
            escaped_first.scope_gen != target_scope.escape_generation) {
            escaped_first.scope_gen = target_scope.escape_generation;
        }
    }
    if (ctx != null && ctx.aborted) return partial;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_first, build_scope),
        "boundary destination-partial: first arg must not point into target TEMP lane"
    );

    Value* escaped_second = partial.partial_val.second_arg;
    if (escaped_second != null &&
        !boundary_ptr_in_scope((void*)escaped_second, releasing_scope) &&
        boundary_ptr_in_scope_chain_with_hint(
            (void*)escaped_second,
            target_scope,
            escaped_second.scope_gen
        )) {
        // Preserve pointer identity for payloads already routed into the target
        // chain. The destination wrapper is still rebuilt below.
    } else {
        Value* routed_second = boundary_copy_to_parent_site_ctx(
            partial.partial_val.second_arg,
            interp,
            COPY_SITE_GENERIC,
            ctx
        );
        escaped_second = boundary_destination_promote_routed_escape(
            routed_second,
            interp,
            build_scope,
            ctx
        );
        if (ctx != null && ctx.aborted) return partial;
        if (escaped_second != null &&
            escaped_second.scope_gen != target_scope.escape_generation) {
            escaped_second.scope_gen = target_scope.escape_generation;
        }
    }
    if (ctx != null && ctx.aborted) return partial;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_second, build_scope),
        "boundary destination-partial: second arg must not point into target TEMP lane"
    );

    Value* result = interp.alloc_value_escape();
    result.tag = PARTIAL_PRIM;
    result.partial_val.func = partial.partial_val.func;
    result.partial_val.remaining = partial.partial_val.remaining;
    result.partial_val.first_arg = escaped_first;
    result.partial_val.second_arg = escaped_second;
    boundary_destination_build_scope_commit(&build_scope, target_scope);
    if (result != null && result.scope_gen != target_scope.escape_generation) {
        result.scope_gen = target_scope.escape_generation;
    }
    return result;
}

<*
@require iterator != null : "boundary_build_destination_iterator_escape: null iterator"
@require interp != null : "boundary_build_destination_iterator_escape: null interp"
@require target_scope != null : "boundary_build_destination_iterator_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_iterator_escape: null releasing_scope"
@ensure return == iterator || return.scope_gen == target_scope.escape_generation || boundary_ptr_in_scope((void*)return, target_scope) : "boundary_build_destination_iterator_escape: expected ESCAPE-lane iterator wrapper"
*>
fn Value* boundary_build_destination_iterator_escape(
    Value* iterator,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (iterator.tag != ITERATOR) return iterator;
    if (ctx != null && ctx.aborted) return iterator;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    main::ScopeRegion* build_scope = boundary_destination_build_scope_begin(
        interp,
        target_scope,
        releasing_scope
    );
    if (build_scope == null) return iterator;
    defer boundary_destination_build_scope_abort(&build_scope);
    if (ctx != null && ctx.aborted) return iterator;

    Value* routed_inner = boundary_copy_to_parent_site_ctx(
        iterator.iterator_val,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    routed_inner = boundary_destination_detach_iterator_inner(
        routed_inner,
        interp,
        target_scope,
        ctx
    );
    Value* escaped_inner = boundary_destination_promote_routed_escape(
        routed_inner,
        interp,
        build_scope,
        ctx
    );
    if (ctx != null && ctx.aborted) return iterator;
    if (escaped_inner != null &&
        boundary_ptr_in_scope((void*)escaped_inner, build_scope) &&
        escaped_inner.scope_gen != target_scope.escape_generation) {
        escaped_inner.scope_gen = target_scope.escape_generation;
    }
    assert(
        boundary_cons_escape_child_edge_safe(escaped_inner, build_scope),
        "boundary destination-iterator: iterator payload must not point into target TEMP lane"
    );

    Value* result = interp.alloc_value_escape();
    result.tag = ITERATOR;
    result.iterator_val = escaped_inner;
    boundary_destination_build_scope_commit(&build_scope, target_scope);
    if (result != null && result.scope_gen != target_scope.escape_generation) {
        result.scope_gen = target_scope.escape_generation;
    }
    return result;
}
