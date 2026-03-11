module lisp;

import std::core::mem;
import std::io;
import main;

fn Value* boundary_fallback_disallowed_error(Interp* interp, BoundaryCommitFault fault_code) @noinline {
    char[192] msg_buf;
    if (try msg = io::bprintf(
        &msg_buf,
        "boundary: traversal fallback disallowed for commit path (%s)",
        (ZString)boundary_commit_fault_name(fault_code)
    )) {
        return raise_error(interp, msg);
    }
    return raise_error(interp, "boundary: traversal fallback disallowed for commit path");
}

fn Value* boundary_destination_promote_routed_escape(
    Value* routed,
    Interp* interp,
    main::ScopeRegion* target_scope,
    PromotionContext* ctx = null
) @inline {
    if (routed == null) return null;
    if (ctx != null && ctx.aborted) return routed;
    if (routed.scope_gen == target_scope.escape_generation) return routed;
    return boundary_promote_to_escape(routed, interp);
}

<*
@require list != null : "boundary_build_destination_cons_escape: null list"
@require interp != null : "boundary_build_destination_cons_escape: null interp"
@require target_scope != null : "boundary_build_destination_cons_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_cons_escape: null releasing_scope"
@ensure return == list || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_cons_escape: expected ESCAPE-lane cons head"
*>
fn Value* boundary_build_destination_cons_escape(
    Value* list,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (list.tag != CONS) return list;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* head = interp.alloc_value_escape();
    head.tag = CONS;
    Value* current_new = head;
    Value* current_old = list;

    while (true) {
        Value* routed_car = boundary_copy_to_parent_site_ctx(
            current_old.cons_val.car,
            interp,
            COPY_SITE_CONS_BARRIER_CAR,
            ctx
        );
        current_new.cons_val.car = boundary_destination_promote_routed_escape(
            routed_car,
            interp,
            target_scope,
            ctx
        );
        if (ctx != null && ctx.aborted) return list;
        assert(
            boundary_cons_escape_child_edge_safe(current_new.cons_val.car, target_scope),
            "boundary destination-cons: ESCAPE car must not point into target TEMP lane"
        );

        Value* old_cdr = current_old.cons_val.cdr;
        bool extend_spine = old_cdr != null &&
            old_cdr.tag == CONS &&
            old_cdr.scope_gen == releasing_scope.generation &&
            boundary_ptr_in_scope((void*)old_cdr, releasing_scope);
        if (extend_spine) {
            Value* next_new = interp.alloc_value_escape();
            next_new.tag = CONS;
            current_new.cons_val.cdr = next_new;
            current_new = next_new;
            current_old = old_cdr;
            continue;
        }

        Value* routed_cdr = boundary_copy_to_parent_site_ctx(
            old_cdr,
            interp,
            COPY_SITE_CONS_BARRIER_CDR,
            ctx
        );
        current_new.cons_val.cdr = boundary_destination_promote_routed_escape(
            routed_cdr,
            interp,
            target_scope,
            ctx
        );
        if (ctx != null && ctx.aborted) return list;
        assert(
            boundary_cons_escape_child_edge_safe(current_new.cons_val.cdr, target_scope),
            "boundary destination-cons: ESCAPE cdr must not point into target TEMP lane"
        );
        break;
    }

    return head;
}

<*
@require partial != null : "boundary_build_destination_partial_escape: null partial"
@require interp != null : "boundary_build_destination_partial_escape: null interp"
@require target_scope != null : "boundary_build_destination_partial_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_partial_escape: null releasing_scope"
@ensure return == partial || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_partial_escape: expected ESCAPE-lane partial wrapper"
*>
fn Value* boundary_build_destination_partial_escape(
    Value* partial,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (partial.tag != PARTIAL_PRIM) return partial;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* routed_first = boundary_copy_to_parent_site_ctx(
        partial.partial_val.first_arg,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    Value* escaped_first = boundary_destination_promote_routed_escape(
        routed_first,
        interp,
        target_scope,
        ctx
    );
    if (ctx != null && ctx.aborted) return partial;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_first, target_scope),
        "boundary destination-partial: first arg must not point into target TEMP lane"
    );

    Value* routed_second = boundary_copy_to_parent_site_ctx(
        partial.partial_val.second_arg,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    Value* escaped_second = boundary_destination_promote_routed_escape(
        routed_second,
        interp,
        target_scope,
        ctx
    );
    if (ctx != null && ctx.aborted) return partial;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_second, target_scope),
        "boundary destination-partial: second arg must not point into target TEMP lane"
    );

    Value* result = interp.alloc_value_escape();
    result.tag = PARTIAL_PRIM;
    result.partial_val.func = partial.partial_val.func;
    result.partial_val.remaining = partial.partial_val.remaining;
    result.partial_val.first_arg = escaped_first;
    result.partial_val.second_arg = escaped_second;
    return result;
}

<*
@require iterator != null : "boundary_build_destination_iterator_escape: null iterator"
@require interp != null : "boundary_build_destination_iterator_escape: null interp"
@require target_scope != null : "boundary_build_destination_iterator_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_iterator_escape: null releasing_scope"
@ensure return == iterator || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_iterator_escape: expected ESCAPE-lane iterator wrapper"
*>
fn Value* boundary_build_destination_iterator_escape(
    Value* iterator,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (iterator.tag != ITERATOR) return iterator;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* routed_inner = boundary_copy_to_parent_site_ctx(
        iterator.iterator_val,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    Value* escaped_inner = boundary_destination_promote_routed_escape(
        routed_inner,
        interp,
        target_scope,
        ctx
    );
    if (ctx != null && ctx.aborted) return iterator;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_inner, target_scope),
        "boundary destination-iterator: iterator payload must not point into target TEMP lane"
    );

    Value* result = interp.alloc_value_escape();
    result.tag = ITERATOR;
    result.iterator_val = escaped_inner;
    return result;
}

<*
@require error_value != null : "boundary_build_destination_error_escape: null error value"
@require interp != null : "boundary_build_destination_error_escape: null interp"
@require target_scope != null : "boundary_build_destination_error_escape: null target_scope"
@ensure return == error_value || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_error_escape: expected ESCAPE-lane error wrapper"
*>
fn Value* boundary_build_destination_error_escape(
    Value* error_value,
    Interp* interp,
    main::ScopeRegion* target_scope
) {
    if (error_value.tag != ERROR) return error_value;
    Value* result = interp.alloc_value_escape();
    result.tag = ERROR;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);

    usz len = error_value.str_len;
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    result.str_chars = (char*)mem::malloc(cap);
    for (usz i = 0; i < len; i++) {
        result.str_chars[i] = error_value.str_chars[i];
    }
    result.str_chars[len] = 0;
    result.str_len = len;
    return result;
}
