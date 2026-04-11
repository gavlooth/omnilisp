module lisp;

import main;

fn void boundary_destination_build_scope_splice(
    main::ScopeRegion* target_scope,
    main::ScopeRegion* build_scope
) @inline {
    main::scope_splice_escapes(target_scope, build_scope);
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

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) return error_value;
    result.tag = ERROR;

    usz len = error_value.str_len;
    if (len == usz.max) return error_value;
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    result.str_chars = g_boundary_force_destination_error_escape_alloc_fail
        ? null
        : (char*)mem::malloc(cap);
    if (result.str_chars == null) return error_value;
    result.string_heap_owned = true;
    for (usz i = 0; i < len; i++) {
        result.str_chars[i] = error_value.str_chars[i];
    }
    result.str_chars[len] = 0;
    result.str_len = len;
    main::scope_register_dtor_escape(build_scope, (void*)result, &scope_dtor_value);
    boundary_destination_build_scope_commit(&build_scope, target_scope);
    if (result != null && result.scope_gen != target_scope.escape_generation) {
        result.scope_gen = target_scope.escape_generation;
    }
    return result;
}
