module lisp;

import std::core::mem;
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

fn Value* copy_primitive_to_parent(
    Value* v,
    Interp* interp,
    BoundaryCopyFault* fault_out = null
) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;
    Value* result = interp.alloc_value();
    result.tag = PRIMITIVE;
    result.prim_val = null;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);

    Primitive* src_prim = v.prim_val;
    if (src_prim == null) return result;
    if (primitive_has_opaque_user_data(v)) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        return null;
    }

    Primitive* dst_prim = (Primitive*)mem::malloc(Primitive.sizeof);
    if (dst_prim == null) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return null;
    }
    if (!primitive_clone_plain_header(dst_prim, src_prim)) {
        mem::free(dst_prim);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        return null;
    }

    result.prim_val = dst_prim;
    return result;
}

fn Value* copy_iterator_to_parent(Value* v, Interp* interp) {
    Value* promoted_thunk = copy_to_parent(v.iterator_val, interp);
    Value* result = interp.alloc_value();
    result.tag = ITERATOR;
    result.iterator_val = promoted_thunk;
    return result;
}

fn CoroutineThunkState* copy_parent_clone_coroutine_state(main::StackCtx* source, Interp* interp) @inline {
    if (source == null || interp == null) return null;
    CoroutineThunkState* state = (CoroutineThunkState*)source.user_data;
    if (state == null || state.interp != interp) return null;
    return coroutine_alloc_thunk_state(state.thunk, interp);
}

fn main::StackCtx* copy_parent_clone_coroutine_ctx(main::StackCtx* source, Interp* interp) {
    if (source == null || interp == null) return null;
    if (source.owner_pool != &interp.stack_ctx_pool) return null;

    switch (source.status) {
        case main::StackCtxStatus.CTX_READY: {
            CoroutineThunkState* cloned_state = copy_parent_clone_coroutine_state(source, interp);
            if (cloned_state == null) return null;

            main::StackCtx* clone = main::stack_ctx_create(&interp.stack_ctx_pool);
            if (clone == null) {
                mem::free(cloned_state);
                return null;
            }

            clone.user_data = cloned_state;
            main::stack_ctx_init(clone, &coroutine_thunk_entry, cloned_state);
            return clone;
        }
        case main::StackCtxStatus.CTX_SUSPENDED: {
            main::StackCtx* clone = main::stack_ctx_clone(source, &interp.stack_ctx_pool);
            if (clone == null) return null;

            CoroutineThunkState* source_state = (CoroutineThunkState*)source.user_data;
            if (source_state != null) {
                CoroutineThunkState* cloned_state = copy_parent_clone_coroutine_state(source, interp);
                if (cloned_state == null) {
                    main::stack_ctx_destroy(clone, &interp.stack_ctx_pool);
                    return null;
                }
                clone.user_data = cloned_state;
            }
            return clone;
        }
        default:
            return null;
    }
}

fn Value* copy_coroutine_to_parent(Value* v, Interp* interp) {
    // Coroutine wrappers own StackCtx teardown. Boundary copies must produce an
    // independent context so source aliases stay semantically unchanged.
    if (!boundary_value_in_releasing_scope(v, interp)) {
        return raise_error(interp, "boundary: refusing coroutine wrapper alias across disjoint scopes");
    }
    main::StackCtx* cloned_ctx = copy_parent_clone_coroutine_ctx(v.coroutine_val, interp);
    if (cloned_ctx == null) {
        return raise_error(interp, "boundary: refusing coroutine wrapper copy without clonable context state");
    }
    Value* result = boundary_alloc_value_in_scope(interp, interp.current_scope, true);
    result.tag = COROUTINE;
    result.coroutine_val = cloned_ctx;
    return result;
}

fn Value* copy_time_point_to_parent(Value* v, Interp* interp) {
    Value* result = interp.alloc_value();
    result.tag = TIME_POINT;
    result.time_point_val = v.time_point_val;
    return result;
}
