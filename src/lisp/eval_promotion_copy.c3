module lisp;

import std::io;
import main;

// =============================================================================
// SECTION 2.4: FRAME PUSH/POP (copy-to-parent core)
// =============================================================================
fn Value* copy_cons_to_parent(Value* v, Interp* interp) {
    // Iterative copy to prevent C stack overflow on long lists.
    Value* head = interp.alloc_value();
    head.tag = CONS;
    Value* current_new = head;
    Value* current_old = v;

    while (true) {
        current_new.cons_val.car = copy_to_parent(current_old.cons_val.car, interp);
        Value* old_cdr = current_old.cons_val.cdr;
        if (old_cdr != null && old_cdr.tag == CONS) {
            if (boundary_can_reuse_value(old_cdr, interp)) {
                // Safe fast-path: tail already lives in a surviving scope chain.
                current_new.cons_val.cdr = old_cdr;
                break;
            }
            // Deep-copy tail iteratively to avoid recursive overflow on long lists.
            Value* next_new = interp.alloc_value();
            next_new.tag = CONS;
            current_new.cons_val.cdr = next_new;
            current_new = next_new;
            current_old = old_cdr;
        } else {
            current_new.cons_val.cdr = copy_to_parent(old_cdr, interp);
            break;
        }
    }
    return head;
}

fn bool copy_parent_should_reuse_closure(Value* v, Interp* interp) @inline {
    if (boundary_closure_payload_in_scope(v, interp.releasing_scope)) return false;
    return boundary_ptr_in_target_scope_chain_with_hint((void*)v, interp, v.scope_gen);
}

fn void copy_parent_clone_closure_payload(Value* result, Value* src, Interp* interp) {
    result.tag = CLOSURE;
    result.closure_val = (Closure*)interp.current_scope.alloc(Closure.sizeof);
    *result.closure_val = *src.closure_val;
    result.closure_val.scope_gen = interp.current_scope.generation;

    if (src.closure_val.param_count > 0) {
        result.closure_val.params = (SymbolId*)interp.current_scope.alloc(SymbolId.sizeof * src.closure_val.param_count);
        for (usz i = 0; i < src.closure_val.param_count; i++) {
            result.closure_val.params[i] = src.closure_val.params[i];
        }
    } else {
        result.closure_val.params = null;
    }

    if (src.closure_val.type_sig != null) {
        result.closure_val.type_sig = (MethodSignature*)interp.current_scope.alloc(MethodSignature.sizeof);
        if (result.closure_val.type_sig == null ||
            !method_signature_copy_to_scope(result.closure_val.type_sig, src.closure_val.type_sig, interp.current_scope)) {
            result.closure_val.type_sig = null;
        }
    }

    if (result.closure_val.env_scope != null) {
        main::scope_retain(result.closure_val.env_scope);
    }
}

fn Value* copy_closure_to_parent(Value* v, Interp* interp) {
    if (copy_parent_should_reuse_closure(v, interp)) return v;
    Value* result = interp.alloc_value();
    copy_parent_clone_closure_payload(result, v, interp);
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_closure);
    return result;
}

fn bool needs_wrapper_copy(Value* v, Interp* interp) @inline {
    return !boundary_can_reuse_value(v, interp);
}

fn Value* copy_instance_to_parent(Value* v, Interp* interp) {
    if (!needs_wrapper_copy(v, interp)) return v;
    Value* result = interp.alloc_value();
    result.tag = INSTANCE;
    result.instance_val = v.instance_val;
    instance_retain(v.instance_val);
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* copy_ffi_handle_to_parent(Value* v, Interp* interp) {
    if (!needs_wrapper_copy(v, interp)) return v;
    Value* result = interp.alloc_value();
    result.tag = FFI_HANDLE;
    result.ffi_val = v.ffi_val;
    ffi_handle_retain(result.ffi_val);
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* copy_to_parent_missing_scope_provenance_error(Interp* interp) @noinline {
    return raise_error(interp, "boundary: copy_to_parent requires target/releasing scope provenance");
}

<*
@require interp != null : "copy_to_parent_try_fast_reuse: null interp"
@require interp.current_scope != null : "copy_to_parent_try_fast_reuse: null current_scope"
@require interp.releasing_scope != null : "copy_to_parent_try_fast_reuse: null releasing_scope"
*>
fn bool copy_to_parent_try_fast_reuse(Value* v, Interp* interp, Value** out) {
    if (v == null) return false;
    BoundaryReturnClassification classification = boundary_classify_return_value(
        v,
        interp,
        interp.current_scope,
        interp.releasing_scope
    );
    if (classification.provenance == BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN) {
        g_copy_to_parent_stats.fast_reuse_hits++;
        *out = v;
        return true;
    }
    g_copy_to_parent_stats.defensive_copies++;
    return false;
}

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
    Value* result = interp.alloc_value();
    result.tag = COROUTINE;
    result.coroutine_val = v.coroutine_val;
    return result;
}

fn Value* copy_time_point_to_parent(Value* v, Interp* interp) {
    Value* result = interp.alloc_value();
    result.tag = TIME_POINT;
    result.time_point_val = v.time_point_val;
    return result;
}

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

<*
@require v != null : "copy_to_parent_by_route: null value"
@require interp != null : "copy_to_parent_by_route: null interp"
*>
fn Value* copy_to_parent_by_route(Value* v, Interp* interp, CopyParentRoute route) {
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
            return copy_cons_to_parent(v, interp);
        case CP_ROUTE_CLOSURE:
            return copy_closure_to_parent(v, interp);
        case CP_ROUTE_PRIMITIVE:
            return v;
        case CP_ROUTE_PARTIAL_PRIM:
            // Deep copy: Value wrapper may be in child scope, and first_arg/second_arg may too.
            return copy_partial_prim_to_parent(v, interp);
        case CP_ROUTE_CONTINUATION:
            // Shallow copy: Value wrapper may be in child scope, cont_val itself is interpreter-lifetime.
            return copy_continuation_to_parent(v, interp);
        case CP_ROUTE_SHARED_WRAPPER:
            // Value allocated in root_scope; backing data is malloc'd with registered destructors.
            return v;
        case CP_ROUTE_INSTANCE:
            return copy_instance_to_parent(v, interp);
        case CP_ROUTE_FFI_HANDLE:
            return copy_ffi_handle_to_parent(v, interp);
        case CP_ROUTE_TYPE_INFO:
            // Type-info entries live in type registry lifetime.
            return v;
        case CP_ROUTE_ITERATOR:
            // Promote underlying closure thunk so iterator outlives child scope.
            return copy_iterator_to_parent(v, interp);
        case CP_ROUTE_COROUTINE:
            // Shallow copy: wrapper may be in child scope, Coro* is stack-pool managed.
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
    if (v == null) return null;
    g_copy_to_parent_stats.total_calls++;
    copy_to_parent_note_tag(v.tag);
    if (interp.current_scope == null || interp.releasing_scope == null) {
        g_copy_to_parent_stats.defensive_copies++;
        return copy_to_parent_missing_scope_provenance_error(interp);
    }

    // Scope-aware optimization: if value is NOT in the scope being released,
    // reuse only when it is already in the target scope chain.
    // Note: uses is_in_scope (chunk walk) rather than scope_gen stamp because
    // chunk-transfer paths (scope_splice_escapes) move allocations without
    // rewriting per-value scope_gen stamps.
    Value* fast_reuse = null;
    if (copy_to_parent_try_fast_reuse(v, interp, &fast_reuse)) return fast_reuse;
    copy_to_parent_note_cons_spine(v);

    CopyParentRoute route = copy_parent_route_for_tag(v.tag);
    return copy_to_parent_by_route(v, interp, route);
}

fn Value* copy_to_parent_site(Value* v, Interp* interp, uint site) {
    if (site < COPY_SITE_COUNT) g_copy_to_parent_stats.site_calls[(usz)site]++;
    return copy_to_parent(v, interp);
}
