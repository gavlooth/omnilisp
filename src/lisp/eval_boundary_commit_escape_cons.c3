module lisp;

import main;

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
    BoundaryDestinationBuilderMemoScope saved_builder_scope =
        boundary_destination_builder_memo_scope_begin(ctx);
    defer boundary_destination_builder_memo_scope_end(ctx, saved_builder_scope);

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

    Value* head = boundary_alloc_value_in_current_escape(interp);
    if (head == null) return list;
    head.tag = CONS;
    head.cons_val.car = null;
    head.cons_val.cdr = null;
    Value* current_new = head;
    Value* current_old = list;

    while (true) {
        Value* old_car = current_old.cons_val.car;
        Value* routed_car = boundary_copy_to_parent_site_ctx(
            old_car,
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
        Value* car_fault = boundary_destination_bubble_child_fault(
            old_car,
            current_new.cons_val.car,
            list,
            interp,
            target_scope,
            ctx
        );
        if (car_fault != null) {
            return car_fault;
        }
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
            Value* next_new = boundary_alloc_value_in_current_escape(interp);
            if (next_new == null) return list;
            next_new.tag = CONS;
            next_new.cons_val.car = null;
            next_new.cons_val.cdr = null;
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
        Value* cdr_fault = boundary_destination_bubble_child_fault(
            old_cdr,
            current_new.cons_val.cdr,
            list,
            interp,
            target_scope,
            ctx
        );
        if (cdr_fault != null) {
            return cdr_fault;
        }
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
