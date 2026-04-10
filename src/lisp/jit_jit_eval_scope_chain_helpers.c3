module lisp;

import main;

fn bool env_chain_has_frame_in_scope(Env* env, main::ScopeRegion* scope, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        if (!cur.is_root_persistent_box() &&
            (cur.scope_gen == scope.generation ||
             cur.scope_gen == scope.escape_generation ||
             boundary_ptr_in_scope((void*)cur, scope))) {
            return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn bool is_in_temp_lane(void* ptr, main::ScopeRegion* scope) {
    if (ptr == null || scope == null) return false;
    main::ScopeChunk* chunk = scope.chunks;
    while (chunk != null) {
        char* data = (char*)chunk + main::ScopeChunk.sizeof;
        if ((char*)ptr >= data && (char*)ptr < data + chunk.capacity) return true;
        chunk = chunk.next;
    }
    return false;
}

fn bool env_frame_in_temp_lane(Env* env, main::ScopeRegion* scope) @inline {
    if (env == null || scope == null || env.is_root_persistent_box()) return false;
    return env.scope_gen == scope.generation ||
        is_in_temp_lane((void*)env, scope);
}

fn bool value_in_temp_lane(Value* v, main::ScopeRegion* scope) @inline {
    if (v == null || scope == null) return false;
    return v.scope_gen == scope.generation ||
        is_in_temp_lane((void*)v, scope);
}

fn bool env_chain_has_binding_in_scope(Env* env, main::ScopeRegion* scope, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        for (usz i = 0; i < cur.binding_count; i++) {
            Value* v = cur.bindings[i].value;
            if (v != null &&
                (v.scope_gen == scope.generation ||
                 v.scope_gen == scope.escape_generation ||
                 is_in_temp_lane((void*)v, scope))) {
                return true;
            }
        }
        cur = cur.parent;
    }
    return false;
}

fn bool env_chain_has_frame_in_temp_scope(Env* env, main::ScopeRegion* scope, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        if (env_frame_in_temp_lane(cur, scope)) return true;
        cur = cur.parent;
    }
    return false;
}

fn bool env_chain_has_binding_in_temp_scope(Env* env, main::ScopeRegion* scope, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        for (usz i = 0; i < cur.binding_count; i++) {
            if (value_in_temp_lane(cur.bindings[i].value, scope)) return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn bool env_chain_contains_root_persistent_box(Env* env, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        if (cur.is_root_persistent_box()) return true;
        cur = cur.parent;
    }
    return false;
}

fn bool jit_value_is_graph_carrying(Value* v) @inline {
    if (v == null) return false;
    switch (v.tag) {
        case CONS:
        case ARRAY:
        case HASHMAP:
        case SET:
        case CLOSURE:
        case MODULE:
        case PARTIAL_PRIM:
        case METHOD_TABLE:
        case ITERATOR:
        case INSTANCE:
            return true;
        default:
            return false;
    }
}

fn bool env_chain_has_graph_binding_values(Env* env, Env* stop_at) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        for (usz i = 0; i < cur.binding_count; i++) {
            if (jit_value_is_graph_carrying(cur.bindings[i].value)) return true;
        }
        cur = cur.parent;
    }
    return false;
}

fn bool env_chain_has_graph_binding_values_in_temp_scope(
    Env* env,
    main::ScopeRegion* scope,
    Env* stop_at
) {
    Env* cur = env;
    while (cur != null && cur != stop_at) {
        for (usz i = 0; i < cur.binding_count; i++) {
            Value* v = cur.bindings[i].value;
            if (jit_value_is_graph_carrying(v) &&
                value_in_temp_lane(v, scope)) {
                return true;
            }
        }
        cur = cur.parent;
    }
    return false;
}

fn bool jit_env_parent_needs_rewrite(Env* parent, main::ScopeRegion* releasing_scope) @inline {
    if (parent == null || releasing_scope == null) return false;
    return boundary_ptr_in_scope((void*)parent, releasing_scope);
}

fn bool jit_binding_payload_in_releasing_scope(Value* v, main::ScopeRegion* releasing_scope) @inline {
    if (v == null || releasing_scope == null) return false;
    if (v.tag == CLOSURE) {
        return boundary_closure_payload_in_scope(v, releasing_scope);
    }
    if (v.tag == ITERATOR) {
        Value* payload = v.iterator_val;
        if (payload != null && boundary_ptr_in_scope((void*)payload, releasing_scope)) {
            return true;
        }
        if (payload != null &&
            payload.tag == CLOSURE &&
            boundary_closure_payload_in_scope(payload, releasing_scope)) {
            return true;
        }
    }
    return false;
}

fn bool jit_binding_graph_needs_copy(Value* v, Interp* interp, usz depth = 0) {
    if (v == null || interp == null || interp.releasing_scope == null) return false;
    if (depth >= 128) return true;
    if (boundary_ptr_in_scope((void*)v, interp.releasing_scope)) return true;
    if (boundary_ptr_in_target_scope_chain_with_hint((void*)v, interp, v.scope_gen)) {
        return boundary_graph_alias_unsafe_for_reuse(v, interp, depth);
    }
    if (jit_value_is_graph_carrying(v)) return true;
    if (copy_to_parent_payload_in_releasing_scope(v, interp)) return true;

    switch (v.tag) {
        case CONS:
            return jit_binding_graph_needs_copy(v.cons_val.car, interp, depth + 1) ||
                jit_binding_graph_needs_copy(v.cons_val.cdr, interp, depth + 1);
        default:
            return false;
    }
}

fn bool jit_tco_binding_needs_copy(
    Value* v,
    Interp* interp,
    main::ScopeRegion* releasing_scope
) @inline {
    if (v == null || interp == null || releasing_scope == null) return false;
    if (v.scope_gen == releasing_scope.generation ||
        v.scope_gen == releasing_scope.escape_generation ||
        boundary_ptr_in_scope((void*)v, releasing_scope)) {
        return true;
    }
    if (v.tag == CLOSURE) {
        return boundary_closure_alias_unsafe_for_reuse(v, interp);
    }
    if (v.tag == ITERATOR) {
        return boundary_iterator_payload_alias_unsafe_for_reuse(v, interp);
    }
    return jit_binding_graph_needs_copy(v, interp);
}
