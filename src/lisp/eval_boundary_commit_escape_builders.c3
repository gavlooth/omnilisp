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

fn bool boundary_destination_iterator_inner_needs_detach(
    Value* inner,
    main::ScopeRegion* target_scope
) @inline {
    if (inner == null || target_scope == null) return false;
    if (boundary_ptr_in_scope((void*)inner, target_scope) &&
        inner.scope_gen == target_scope.generation) {
        return true;
    }
    if (inner.tag == CLOSURE &&
        boundary_closure_payload_in_scope(inner, target_scope)) {
        return true;
    }
    return false;
}

fn Value* boundary_destination_detach_iterator_inner(
    Value* inner,
    Interp* interp,
    main::ScopeRegion* target_scope,
    PromotionContext* ctx = null
) {
    if (!boundary_destination_iterator_inner_needs_detach(inner, target_scope)) {
        return inner;
    }

    main::ScopeRegion* saved_releasing_scope = interp.releasing_scope;
    interp.releasing_scope = target_scope;
    defer interp.releasing_scope = saved_releasing_scope;
    // Detach must bypass the caller's promotion memo. The first routing pass can
    // legitimately cache "reuse the original thunk", but the second pass exists
    // specifically to override that decision for target TEMP-lane payloads.
    (void)ctx;
    return boundary_copy_to_parent_site_ctx(inner, interp, COPY_SITE_GENERIC, null);
}

fn main::ScopeRegion* boundary_destination_build_scope_begin(
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) @inline {
    main::ScopeRegion* build_scope = main::scope_create(target_scope);
    if (build_scope == null) return null;
    interp.current_scope = build_scope;
    interp.releasing_scope = releasing_scope;
    return build_scope;
}

fn void boundary_destination_build_scope_abort(main::ScopeRegion** build_scope_io) @inline {
    if (build_scope_io == null) return;
    if (*build_scope_io == null) return;
    main::scope_release(*build_scope_io);
    *build_scope_io = null;
}

fn void boundary_destination_build_scope_commit(
    main::ScopeRegion** build_scope_io,
    main::ScopeRegion* target_scope
) @inline {
    if (build_scope_io == null) return;
    main::ScopeRegion* build_scope = *build_scope_io;
    if (build_scope == null || target_scope == null) return;
    main::scope_splice_escapes(target_scope, build_scope);
    *build_scope_io = null;
}

<*
@require list != null : "boundary_build_destination_cons_escape: null list"
@require interp != null : "boundary_build_destination_cons_escape: null interp"
@require target_scope != null : "boundary_build_destination_cons_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_cons_escape: null releasing_scope"
@ensure return == list || return.scope_gen == target_scope.escape_generation || boundary_ptr_in_scope((void*)return, target_scope) : "boundary_build_destination_cons_escape: expected ESCAPE-lane cons head"
*>
fn Value* boundary_build_destination_cons_escape(
    Value* list,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (list.tag != CONS) return list;
    if (ctx != null && ctx.aborted) return list;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    main::ScopeRegion* build_scope = boundary_destination_build_scope_begin(
        interp,
        target_scope,
        releasing_scope
    );
    if (build_scope == null) return list;
    defer boundary_destination_build_scope_abort(&build_scope);
    if (ctx != null && ctx.aborted) return list;

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
            build_scope,
            ctx
        );
        if (ctx != null && ctx.aborted) return list;
        assert(
            boundary_cons_escape_child_edge_safe(current_new.cons_val.car, build_scope),
            "boundary destination-cons: ESCAPE car must not point into target TEMP lane"
        );

        Value* old_cdr = current_old.cons_val.cdr;
        bool extend_spine = old_cdr != null &&
            old_cdr.tag == CONS &&
            boundary_ptr_in_scope((void*)old_cdr, releasing_scope);
        if (extend_spine) {
            if (ctx != null && ctx.aborted) return list;
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
            build_scope,
            ctx
        );
        if (ctx != null && ctx.aborted) return list;
        assert(
            boundary_cons_escape_child_edge_safe(current_new.cons_val.cdr, build_scope),
            "boundary destination-cons: ESCAPE cdr must not point into target TEMP lane"
        );
        break;
    }

    boundary_destination_build_scope_commit(&build_scope, target_scope);
    if (head != null && head.scope_gen != target_scope.escape_generation) {
        head.scope_gen = target_scope.escape_generation;
    }
    return head;
}

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

<*
@require error_value != null : "boundary_build_destination_error_escape: null error value"
@require interp != null : "boundary_build_destination_error_escape: null interp"
@require target_scope != null : "boundary_build_destination_error_escape: null target_scope"
@ensure return == error_value || return.scope_gen == target_scope.escape_generation || boundary_ptr_in_scope((void*)return, target_scope) : "boundary_build_destination_error_escape: expected ESCAPE-lane error wrapper"
*>
fn Value* boundary_build_destination_error_escape(
    Value* error_value,
    Interp* interp,
    main::ScopeRegion* target_scope,
    PromotionContext* ctx = null
) {
    if (error_value.tag != ERROR) return error_value;
    if (ctx != null && ctx.aborted) return error_value;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    main::ScopeRegion* build_scope = boundary_destination_build_scope_begin(
        interp,
        target_scope,
        session.releasing_scope
    );
    if (build_scope == null) return error_value;
    defer boundary_destination_build_scope_abort(&build_scope);
    if (ctx != null && ctx.aborted) return error_value;

    Value* result = interp.alloc_value_escape();
    result.tag = ERROR;
    main::scope_register_dtor_escape(build_scope, (void*)result, &scope_dtor_value);

    usz len = error_value.str_len;
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    result.str_chars = (char*)mem::malloc(cap);
    for (usz i = 0; i < len; i++) {
        result.str_chars[i] = error_value.str_chars[i];
    }
    result.str_chars[len] = 0;
    result.str_len = len;
    boundary_destination_build_scope_commit(&build_scope, target_scope);
    if (result != null && result.scope_gen != target_scope.escape_generation) {
        result.scope_gen = target_scope.escape_generation;
    }
    return result;
}
