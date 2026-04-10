module lisp;

import main;

<*
@require interp != null : "boundary_value_in_releasing_scope: null interp"
*>
fn bool boundary_value_in_releasing_scope(Value* v, Interp* interp) @inline {
    if (v == null || interp.releasing_scope == null) return false;
    return boundary_ptr_in_scope((void*)v, interp.releasing_scope);
}

fn bool boundary_graph_child_alias_unsafe_for_reuse(Value* child, Interp* interp, usz depth) @inline {
    if (child == null || interp == null) return false;
    if (boundary_value_in_releasing_scope(child, interp)) return true;
    if (!boundary_ptr_in_target_scope_chain_with_hint((void*)child, interp, child.scope_gen)) return true;
    return boundary_graph_alias_unsafe_for_reuse(child, interp, depth + 1);
}

fn bool boundary_array_payload_alias_unsafe_for_reuse(Value* array_v, Interp* interp, usz depth) {
    if (array_v == null || array_v.tag != ARRAY || array_v.array_val == null) return false;
    Array* arr = array_v.array_val;
    for (usz i = 0; i < arr.length; i++) {
        if (boundary_graph_child_alias_unsafe_for_reuse(arr.items[i], interp, depth)) return true;
    }
    return false;
}

fn bool boundary_hashmap_payload_alias_unsafe_for_reuse(Value* map_v, Interp* interp, usz depth) {
    if (map_v == null || (map_v.tag != HASHMAP && map_v.tag != SET) || map_v.hashmap_val == null) return false;
    HashMap* map = map_v.hashmap_val;
    if (map.entries == null) return false;

    for (uint i = 0; i < map.capacity; i++) {
        Value* key = map.entries[i].key;
        if (key == null) continue;
        if (boundary_graph_child_alias_unsafe_for_reuse(key, interp, depth)) return true;
        if (map_v.tag == HASHMAP &&
            boundary_graph_child_alias_unsafe_for_reuse(map.entries[i].value, interp, depth)) {
            return true;
        }
    }
    return false;
}

fn bool boundary_method_table_payload_alias_unsafe_for_reuse(Value* mt_v, Interp* interp, usz depth) {
    if (mt_v == null || mt_v.tag != METHOD_TABLE || mt_v.method_table_val == null) return false;
    MethodTable* mt = mt_v.method_table_val;
    for (usz i = 0; i < mt.entry_count; i++) {
        if (boundary_graph_child_alias_unsafe_for_reuse(mt.entries[i].implementation, interp, depth)) {
            return true;
        }
    }
    return boundary_graph_child_alias_unsafe_for_reuse(mt.fallback, interp, depth);
}

fn bool boundary_graph_alias_unsafe_for_reuse(Value* v, Interp* interp, usz depth = 0) {
    if (v == null || interp == null) return false;
    if (depth >= 128) return true;

    switch (v.tag) {
        case CLOSURE:
            return boundary_closure_alias_unsafe_for_reuse(v, interp);
        case PARTIAL_PRIM:
            return boundary_partial_payload_alias_unsafe_for_reuse(v, interp);
        case ITERATOR:
            return boundary_iterator_payload_alias_unsafe_for_reuse(v, interp);
        case CONS:
            return boundary_graph_alias_unsafe_for_reuse(v.cons_val.car, interp, depth + 1) ||
                boundary_graph_alias_unsafe_for_reuse(v.cons_val.cdr, interp, depth + 1);
        case ARRAY:
            return boundary_array_payload_alias_unsafe_for_reuse(v, interp, depth);
        case HASHMAP:
        case SET:
            return boundary_hashmap_payload_alias_unsafe_for_reuse(v, interp, depth);
        case METHOD_TABLE:
            return boundary_method_table_payload_alias_unsafe_for_reuse(v, interp, depth);
        default:
            return false;
    }
}

<*
@require interp != null : "boundary_can_reuse_value: null interp"
*>
fn bool boundary_can_reuse_value(Value* v, Interp* interp) @inline {
    if (v == null) return false;
    boundary_assert_interp_scope_chain(interp);
    if (boundary_value_in_releasing_scope(v, interp)) return false;
    if (boundary_graph_alias_unsafe_for_reuse(v, interp)) return false;
    return boundary_ptr_in_target_scope_chain_with_hint((void*)v, interp, v.scope_gen);
}

<*
@require interp != null : "boundary_classify_return_value: null interp"
@require target_scope != null : "boundary_classify_return_value: null target_scope"
@require releasing_scope != null : "boundary_classify_return_value: null releasing_scope"
*>
fn BoundaryReturnClassification boundary_classify_return_value(
    Value* v,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    BoundaryReturnClassification classification = {
        .provenance = BOUNDARY_RETURN_MIXED_UNCERTAIN,
        .in_target_scope_chain = false,
        .in_releasing_scope = false,
    };

    if (v == null) return classification;

    classification.in_releasing_scope = boundary_ptr_in_scope((void*)v, releasing_scope);
    classification.in_target_scope_chain = boundary_ptr_in_scope_chain_with_hint((void*)v, target_scope, v.scope_gen);
    if (!classification.in_releasing_scope && boundary_can_reuse_value(v, interp)) {
        classification.provenance = BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN;
        return classification;
    }

    if (classification.in_releasing_scope) {
        if (v.scope_gen == releasing_scope.generation) {
            classification.provenance = BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP;
            return classification;
        }
        if (v.scope_gen == releasing_scope.escape_generation) {
            classification.provenance = BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE;
            return classification;
        }
    }

    classification.provenance = BOUNDARY_RETURN_MIXED_UNCERTAIN;
    return classification;
}

fn BoundaryScopeTransferResult boundary_try_scope_splice_escapes_result(main::ScopeRegion* parent, main::ScopeRegion* child) @inline {
    BoundaryScopeTransferResult result = boundary_check_scope_transfer(parent, child);
    if (result.allowed) {
        main::scope_splice_escapes(parent, child);
    }
    boundary_note_splice_result(result);
    return result;
}

<*
@require interp != null : "boundary_should_copy_env: null interp"
*>
fn bool boundary_should_copy_env(Value* v, Interp* interp) @inline {
    if (v == null) return false;
    return !boundary_can_reuse_value(v, interp);
}

fn bool boundary_should_promote(Value* v, PromotionContext* ctx, main::ScopeRegion* source_scope) @inline {
    if (v == null || ctx == null || source_scope == null) return false;
    if (ctx.aborted) return false;
    return v.scope_gen == source_scope.escape_generation;
}

fn bool boundary_cons_escape_child_edge_safe(Value* edge, main::ScopeRegion* target_scope) @inline {
    if (edge == null || target_scope == null) return true;
    if (!boundary_ptr_in_scope((void*)edge, target_scope)) return true;
    return edge.scope_gen != target_scope.generation;
}

fn bool boundary_nonunique_destination_retry_allowed(Value* v) @inline {
    if (v == null) return false;
    switch (v.tag) {
        case NIL:
        case VOID:
        case INT:
        case DOUBLE:
        case SYMBOL:
        case TIME_POINT:
            return true;
        default:
            return false;
    }
}

fn bool boundary_closure_payload_in_scope(Value* v, main::ScopeRegion* scope) @inline {
    if (v == null || scope == null || v.tag != CLOSURE || v.closure_val == null) return false;
    if (v.closure_val.scope_gen == scope.generation) return true;
    return boundary_ptr_in_scope((void*)v.closure_val, scope);
}

fn bool boundary_closure_has_detached_env_scope(Value* v) @inline {
    return v != null &&
        v.tag == CLOSURE &&
        v.closure_val != null &&
        v.closure_val.env_scope != null;
}

<*
@require interp != null : "boundary_closure_alias_unsafe_for_reuse: null interp"
*>
fn bool boundary_closure_alias_unsafe_for_reuse(Value* v, Interp* interp) @inline {
    if (v == null || v.tag != CLOSURE || v.closure_val == null) return false;
    if (boundary_closure_has_detached_env_scope(v)) return false;

    bool wrapper_in_target =
        boundary_ptr_in_target_scope_chain_with_hint((void*)v, interp, v.scope_gen);
    bool payload_in_target =
        boundary_ptr_in_target_scope_chain_with_hint(
            (void*)v.closure_val,
            interp,
            v.closure_val.scope_gen
        );

    // Undelimited closures may only alias when both wrapper and payload already
    // reside in the target scope chain.
    return !(wrapper_in_target && payload_in_target);
}

<*
@require interp != null : "boundary_partial_payload_alias_unsafe_for_reuse: null interp"
*>
fn bool boundary_partial_payload_alias_unsafe_for_reuse(Value* partial, Interp* interp) @inline {
    if (partial == null || partial.tag != PARTIAL_PRIM) return false;

    Value* first = partial.partial_val.first_arg;
    if (first != null) {
        if (boundary_value_in_releasing_scope(first, interp)) return true;
        if (!boundary_ptr_in_target_scope_chain_with_hint((void*)first, interp, first.scope_gen)) return true;
        if (boundary_graph_alias_unsafe_for_reuse(first, interp, 1)) return true;
    }

    Value* second = partial.partial_val.second_arg;
    if (second != null) {
        if (boundary_value_in_releasing_scope(second, interp)) return true;
        if (!boundary_ptr_in_target_scope_chain_with_hint((void*)second, interp, second.scope_gen)) return true;
        if (boundary_graph_alias_unsafe_for_reuse(second, interp, 1)) return true;
    }

    return false;
}

<*
@require interp != null : "boundary_iterator_payload_alias_unsafe_for_reuse: null interp"
*>
fn bool boundary_iterator_payload_alias_unsafe_for_reuse(Value* iterator, Interp* interp) @inline {
    if (iterator == null || iterator.tag != ITERATOR) return false;
    Value* payload = iterator.iterator_val;
    if (payload == null) return false;

    if (payload.tag == CLOSURE) {
        return boundary_closure_alias_unsafe_for_reuse(payload, interp);
    }
    if (payload.tag == PARTIAL_PRIM) {
        return boundary_partial_payload_alias_unsafe_for_reuse(payload, interp);
    }

    if (!boundary_ptr_in_target_scope_chain_with_hint((void*)payload, interp, payload.scope_gen)) {
        return true;
    }

    return boundary_graph_alias_unsafe_for_reuse(payload, interp, 1);
}
