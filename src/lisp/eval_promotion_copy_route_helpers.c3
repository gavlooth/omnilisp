module lisp;

import main;

fn Value* copy_partial_prim_to_parent(Value* v, Interp* interp) {
    Value* result = interp.alloc_value();
    result.tag = PARTIAL_PRIM;
    result.partial_val.func = v.partial_val.func;
    result.partial_val.remaining = v.partial_val.remaining;
    result.partial_val.first_arg = copy_to_parent(v.partial_val.first_arg, interp);
    result.partial_val.second_arg = copy_to_parent(v.partial_val.second_arg, interp);
    return result;
}

fn Value* copy_continuation_to_parent(Value* v, Interp* interp) {
    if (v.cont_val != null) {
        if (v.cont_val.owner_interp != null && v.cont_val.owner_interp != interp) {
            return raise_error(interp, "boundary: refusing continuation alias across interpreters");
        }
        continuation_retain_handle_state_for_boundary(v.cont_val);
    }
    Value* result = interp.alloc_value();
    result.tag = CONTINUATION;
    result.cont_val = v.cont_val;
    return result;
}

fn Value* copy_iterator_to_parent(Value* v, Interp* interp) {
    Value* promoted_thunk = copy_to_parent(v.iterator_val, interp);
    Value* result = interp.alloc_value();
    result.tag = ITERATOR;
    result.iterator_val = promoted_thunk;
    return result;
}

fn Value* copy_coroutine_to_parent(Value* v, Interp* interp) {
    // Coroutine wrappers own StackCtx teardown. Never duplicate ownership across wrappers.
    if (!boundary_value_in_releasing_scope(v, interp)) {
        return raise_error(interp, "boundary: refusing coroutine wrapper alias across disjoint scopes");
    }
    Value* result = boundary_alloc_value_in_scope(interp, interp.current_scope, true);
    result.tag = COROUTINE;
    result.coroutine_val = v.coroutine_val;
    v.coroutine_val = null;
    return result;
}

fn Value* copy_time_point_to_parent(Value* v, Interp* interp) {
    Value* result = interp.alloc_value();
    result.tag = TIME_POINT;
    result.time_point_val = v.time_point_val;
    return result;
}
