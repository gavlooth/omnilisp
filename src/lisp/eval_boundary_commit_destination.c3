module lisp;

import std::core::mem;
import main;

struct BoundaryCommitDestinationPromotion {
    Value* value;
    BoundaryCommitOutcome outcome;
    BoundaryCommitFault fault_code;
    bool committed;
    bool stable_materialization_used;
}

fn bool boundary_commit_destination_promoted_error(
    Value* value,
    main::ScopeRegion* target_scope
) @inline {
    return value != null &&
        value.tag == ERROR &&
        target_scope != null &&
        value.scope_gen == target_scope.escape_generation;
}

fn bool stable_escape_materialization_candidate(ValueTag tag) @inline {
    return boundary_value_tag_is_stable_materialization_candidate(tag);
}

fn Value* stable_escape_materialize_alloc_value(Interp* interp, bool register_value_dtor = false) @inline {
    return boundary_alloc_value_in_current_escape(interp, register_value_dtor);
}

fn void* stable_escape_materialize_alloc_payload(Interp* interp, usz size) @inline {
    if (interp == null || interp.current_scope == null) return null;
    return interp.current_scope.alloc_escape(size);
}

fn bool stable_escape_materialize_copy_chars(Value* dst, Value* src, ValueTag tag) {
    if (dst == null || src == null || src.str_chars == null) return false;
    if (src.str_len == usz.max) return false;
    usz cap = src.str_len + 1;
    char* chars = (char*)mem::malloc(cap < 32 ? 32 : cap);
    if (chars == null) return false;
    for (usz i = 0; i < src.str_len; i++) chars[i] = src.str_chars[i];
    chars[src.str_len] = 0;
    dst.tag = tag;
    dst.str_chars = chars;
    dst.str_len = src.str_len;
    dst.string_heap_owned = true;
    return true;
}

fn bool stable_escape_materialize_clone_big_payload(Value* dst, StableEscapePreparedNode* node) {
    if (dst == null || node == null || node.source == null) return false;
    switch (node.tag) {
        case BIG_INTEGER:
            dst.tag = BIG_INTEGER;
            dst.big_integer_val = node.source.big_integer_val == null
                ? null
                : omni_big_integer_clone(node.source.big_integer_val);
            return node.source.big_integer_val == null || dst.big_integer_val != null;
        case BIG_FLOAT:
            dst.tag = BIG_FLOAT;
            dst.big_float_val = node.source.big_float_val == null
                ? null
                : omni_big_float_clone(node.source.big_float_val);
            return node.source.big_float_val == null || dst.big_float_val != null;
        case BIG_COMPLEX:
            dst.tag = BIG_COMPLEX;
            dst.big_complex_val = node.source.big_complex_val == null
                ? null
                : omni_big_complex_clone(node.source.big_complex_val);
            return node.source.big_complex_val == null || dst.big_complex_val != null;
        default:
            return false;
    }
}

fn bool stable_escape_materialize_scalar_node(Value* dst, StableEscapePreparedNode* node) {
    if (dst == null || node == null || node.source == null) return false;
    switch (node.tag) {
        case NIL:
        case VOID:
        case INT:
        case DOUBLE:
        case FLOAT32:
        case COMPLEX128:
        case COMPLEX64:
        case SYMBOL:
        case TIME_POINT:
            *dst = *node.source;
            return true;
        case STRING:
        case ERROR:
            return stable_escape_materialize_copy_chars(dst, node.source, node.tag);
        case BIG_INTEGER:
        case BIG_FLOAT:
        case BIG_COMPLEX:
            return stable_escape_materialize_clone_big_payload(dst, node);
        default:
            return false;
    }
}

fn bool stable_escape_materialize_init_array(Interp* interp, Value* dst, StableEscapePreparedNode* node) {
    if (interp == null || dst == null || node == null || node.source == null) return false;
    Array* src = node.source.array_val;
    if (src == null || node.child_count != src.length) return false;
    Array* arr = (Array*)mem::malloc(Array.sizeof);
    if (arr == null) return false;
    arr.items = null;
    arr.length = src.length;
    arr.capacity = src.capacity;
    if (arr.capacity < arr.length) arr.capacity = arr.length;
    if (arr.capacity < 4) arr.capacity = 4;
    if (arr.capacity > usz.max / Value*.sizeof) {
        mem::free(arr);
        return false;
    }
    arr.items = (Value**)mem::malloc(Value*.sizeof * arr.capacity);
    if (arr.items == null) {
        mem::free(arr);
        return false;
    }
    for (usz i = 0; i < arr.capacity; i++) arr.items[i] = null;
    dst.tag = ARRAY;
    dst.array_val = arr;
    return true;
}

fn bool stable_escape_materialize_init_hashmap_like(Interp* interp, Value* dst, StableEscapePreparedNode* node) {
    if (interp == null || dst == null || node == null || node.source == null) return false;
    HashMap* src = node.source.hashmap_val;
    if (src == null || src.entries == null) return false;
    if (node.tag == HASHMAP && node.child_count != (usz)src.count * (usz)2) return false;
    if (node.tag == SET && node.child_count != (usz)src.count) return false;
    HashMap* map = (HashMap*)mem::malloc(HashMap.sizeof);
    if (map == null) return false;
    map.entries = null;
    map.capacity = src.capacity;
    map.count = src.count;
    map.mask = src.mask;
    if (map.capacity > 0) {
        if ((usz)map.capacity > usz.max / HashEntry.sizeof) {
            mem::free(map);
            return false;
        }
        map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * map.capacity);
        if (map.entries == null) {
            mem::free(map);
            return false;
        }
        for (uint i = 0; i < map.capacity; i++) {
            map.entries[i].key = null;
            map.entries[i].value = null;
        }
    }
    dst.tag = node.tag;
    dst.hashmap_val = map;
    return true;
}

fn bool stable_escape_materialize_init_closure(Interp* interp, Value* dst, StableEscapePreparedNode* node) {
    if (interp == null || dst == null || node == null || node.source == null) return false;
    Closure* src = node.source.closure_val;
    if (src == null) return false;
    Closure* closure = (Closure*)stable_escape_materialize_alloc_payload(interp, Closure.sizeof);
    if (closure == null) return false;
    *closure = *src;
    closure.scope_gen = interp.current_scope.escape_generation;
    closure.env = src.env;
    closure.env_scope = null;
    if (src.param_count > 0) {
        if (src.param_count > usz.max / SymbolId.sizeof) return false;
        closure.params = (SymbolId*)stable_escape_materialize_alloc_payload(interp, SymbolId.sizeof * src.param_count);
        if (closure.params == null) return false;
        for (usz i = 0; i < src.param_count; i++) closure.params[i] = src.params[i];
    } else {
        closure.params = null;
    }
    if (src.type_sig != null) {
        closure.type_sig = (MethodSignature*)stable_escape_materialize_alloc_payload(interp, MethodSignature.sizeof);
        if (closure.type_sig == null ||
            !method_signature_copy_to_escape(closure.type_sig, src.type_sig, interp.current_scope)) {
            return false;
        }
    } else {
        closure.type_sig = null;
    }
    dst.tag = CLOSURE;
    dst.closure_val = closure;
    main::scope_register_dtor_escape(interp.current_scope, (void*)dst, &scope_dtor_closure);
    return true;
}

fn bool stable_escape_materialize_init_shell(Interp* interp, Value* dst, StableEscapePreparedNode* node) {
    if (node == null) return false;
    switch (node.tag) {
        case CONS:
            dst.tag = CONS;
            dst.cons_val.car = null;
            dst.cons_val.cdr = null;
            return true;
        case ARRAY:
            return stable_escape_materialize_init_array(interp, dst, node);
        case HASHMAP:
        case SET:
            return stable_escape_materialize_init_hashmap_like(interp, dst, node);
        case CLOSURE:
            return stable_escape_materialize_init_closure(interp, dst, node);
        default:
            return stable_escape_materialize_scalar_node(dst, node);
    }
}

fn usz stable_escape_materialize_estimated_node_bytes(StableEscapePreparedNode* node) {
    if (node == null || node.source == null) return Value.sizeof;
    usz bytes = Value.sizeof;
    switch (node.tag) {
        case STRING:
        case ERROR: {
            usz cap = node.source.str_len == usz.max ? 0 : node.source.str_len + 1;
            bytes += cap < 32 ? 32 : cap;
            break;
        }
        case ARRAY: {
            Array* arr = node.source.array_val;
            if (arr != null) {
                bytes += Array.sizeof;
                bytes += (usz)arr.capacity * Value*.sizeof;
            }
            break;
        }
        case HASHMAP:
        case SET: {
            HashMap* map = node.source.hashmap_val;
            if (map != null) {
                bytes += HashMap.sizeof;
                bytes += (usz)map.capacity * HashEntry.sizeof;
            }
            break;
        }
        case CLOSURE: {
            Closure* closure = node.source.closure_val;
            if (closure != null) {
                bytes += Closure.sizeof;
                bytes += closure.param_count * SymbolId.sizeof;
                if (closure.type_sig != null) bytes += MethodSignature.sizeof;
            }
            break;
        }
        default:
            break;
    }
    return bytes;
}

fn usz stable_escape_materialize_estimated_copy_bytes(StableEscapePreparedGraphScratch* scratch) {
    if (scratch == null) return 0;
    usz total = 0;
    for (usz i = 0; i < scratch.node_count; i++) {
        total += stable_escape_materialize_estimated_node_bytes(&scratch.nodes[i]);
    }
    total += scratch.edge_count * usz.sizeof;
    return total;
}

fn bool stable_escape_materialize_edge_indices_valid(StableEscapePreparedGraphScratch* scratch, StableEscapePreparedNode* node) @inline {
    if (scratch == null || node == null) return false;
    if (node.child_count == 0) return node.child_offset == STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    if (node.child_offset == STABLE_ESCAPE_PREPARED_INVALID_INDEX) return false;
    if (node.child_offset > scratch.edge_count) return false;
    return scratch.edge_count - node.child_offset >= node.child_count;
}

fn bool stable_escape_materialize_wire_collection(Interp* interp, StableEscapePreparedGraphScratch* scratch, Value** materialized, usz node_index) {
    StableEscapePreparedNode* node = &scratch.nodes[node_index];
    if (!stable_escape_materialize_edge_indices_valid(scratch, node)) return false;
    switch (node.tag) {
        case CONS:
            if (node.child_count != 2) return false;
            usz car_index = scratch.edges[node.child_offset];
            usz cdr_index = scratch.edges[node.child_offset + 1];
            if (car_index >= scratch.node_count || cdr_index >= scratch.node_count) return false;
            materialized[node_index].cons_val.car = materialized[car_index];
            materialized[node_index].cons_val.cdr = materialized[cdr_index];
            return true;
        case ARRAY:
            Array* arr = materialized[node_index].array_val;
            if (arr == null || arr.length != node.child_count) return false;
            for (usz i = 0; i < node.child_count; i++) {
                usz child_index = scratch.edges[node.child_offset + i];
                if (child_index >= scratch.node_count) return false;
                arr.items[i] = materialized[child_index];
            }
            return true;
        default:
            return true;
    }
}

fn bool stable_escape_materialize_wire_hashmap_like(Interp* interp, StableEscapePreparedGraphScratch* scratch, Value** materialized, usz node_index) {
    StableEscapePreparedNode* node = &scratch.nodes[node_index];
    if (node.tag != HASHMAP && node.tag != SET) return true;
    if (!stable_escape_materialize_edge_indices_valid(scratch, node)) return false;
    HashMap* src = node.source.hashmap_val;
    HashMap* dst = materialized[node_index].hashmap_val;
    if (src == null || dst == null || src.capacity != dst.capacity) return false;
    Value* set_true = null;
    if (node.tag == SET) {
        set_true = stable_escape_materialize_alloc_value(interp);
        if (set_true == null) return false;
        set_true.tag = SYMBOL;
        set_true.sym_val = interp.sym_true;
    }
    usz edge_ordinal = 0;
    for (uint i = 0; i < src.capacity; i++) {
        if (src.entries[i].key == null) continue;
        if (node.tag == HASHMAP) {
            if (edge_ordinal + 1 >= node.child_count) return false;
            usz key_index = scratch.edges[node.child_offset + edge_ordinal++];
            usz value_index = scratch.edges[node.child_offset + edge_ordinal++];
            if (key_index >= scratch.node_count || value_index >= scratch.node_count) return false;
            dst.entries[i].key = materialized[key_index];
            dst.entries[i].value = materialized[value_index];
        } else {
            if (edge_ordinal >= node.child_count) return false;
            usz member_index = scratch.edges[node.child_offset + edge_ordinal++];
            if (member_index >= scratch.node_count) return false;
            dst.entries[i].key = materialized[member_index];
            dst.entries[i].value = set_true;
        }
    }
    return edge_ordinal == node.child_count;
}

fn bool stable_escape_materialize_finalize_closure(Interp* interp, StableEscapePreparedNode* node, Value* dst) {
    if (node.tag != CLOSURE) return true;
    Closure* closure = dst.closure_val;
    if (closure == null || node.source.closure_val == null) return false;
    if (copy_parent_closure_env_needs_detach(node.source, interp)) {
        main::ScopeRegion* env_scope = main::scope_create(null);
        if (env_scope == null) return false;
        BoundarySession session = boundary_session_begin(interp);
        defer boundary_session_end(interp, &session);
        interp.current_scope = env_scope;
        BoundaryEnvCopyResult copied = boundary_copy_env_to_scope_checked(node.source.closure_val.env, interp);
        if (!boundary_env_copy_result_ok(copied) || copied.env == null) {
            main::scope_release(env_scope);
            return false;
        }
        closure.env = copied.env;
        closure.env_scope = env_scope;
        return true;
    }
    if (closure_env_scope_is_root_owned(node.source, interp)) {
        closure.env_scope = null;
        return true;
    }
    if (node.source.closure_val.env_scope != null) {
        main::scope_retain(node.source.closure_val.env_scope);
        closure.env_scope = node.source.closure_val.env_scope;
    }
    return true;
}

fn Value* stable_escape_prepare_and_materialize_value(Interp* interp, Value* root,
    main::ScopeRegion* target_scope, main::ScopeRegion* releasing_scope, PromotionContext* ctx) {
    if (interp == null || root == null || target_scope == null) return null;
    StableEscapePreparedGraphScratch scratch = {};
    scratch.global_env = interp.global_env;
    usz root_index = STABLE_ESCAPE_PREPARED_INVALID_INDEX;
    if (!stable_escape_prepare_graph_node(&scratch, root, &root_index)) return null;
    if (root_index == STABLE_ESCAPE_PREPARED_INVALID_INDEX || root_index >= scratch.node_count) return null;

    BoundaryDestinationBuilderMemoScope saved_builder_scope =
        boundary_destination_builder_memo_scope_begin(ctx);
    defer boundary_destination_builder_memo_scope_end(ctx, saved_builder_scope);
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    main::ScopeRegion* build_scope = boundary_destination_build_scope_begin(interp, target_scope, releasing_scope);
    if (build_scope == null) return null;
    defer boundary_destination_build_scope_abort(&build_scope);

    Value*[STABLE_ESCAPE_PREPARED_MAX_NODES] materialized;
    for (usz i = 0; i < scratch.node_count; i++) materialized[i] = null;
    for (usz i = 0; i < scratch.node_count; i++) {
        Value* dst = stable_escape_materialize_alloc_value(interp, true);
        if (dst == null) return null;
        if (!stable_escape_materialize_init_shell(interp, dst, &scratch.nodes[i])) return null;
        materialized[i] = dst;
        if (ctx != null) promotion_context_remember(ctx, scratch.nodes[i].source, dst, interp);
    }

    for (usz i = 0; i < scratch.node_count; i++) {
        if (!stable_escape_materialize_wire_collection(interp, &scratch, &materialized[0], i)) return null;
        if (!stable_escape_materialize_wire_hashmap_like(interp, &scratch, &materialized[0], i)) return null;
    }
    for (usz i = 0; i < scratch.node_count; i++) {
        if (!stable_escape_materialize_finalize_closure(interp, &scratch.nodes[i], materialized[i])) return null;
    }

    if (!promotion_context_consume(ctx, scratch.node_count)) return null;
    Value* result = materialized[root_index];
    boundary_destination_build_scope_commit(&build_scope, target_scope);
    for (usz i = 0; i < scratch.node_count; i++) {
        if (materialized[i] != null) materialized[i].scope_gen = target_scope.escape_generation;
    }
    boundary_note_materialization_copy_debt_enabled(
        scratch.node_count,
        stable_escape_materialize_estimated_copy_bytes(&scratch)
    );
    return result;
}

fn BoundaryCommitDestinationPromotion boundary_commit_try_destination_promotion(
    Value* result,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* active_ctx,
    BoundaryPlanRoute planned_route,
    bool allow_direct_promotion
) {
    BoundaryCommitDestinationPromotion promotion = {
        .value = result,
        .outcome = BOUNDARY_COMMIT_PASSTHROUGH,
        .fault_code = BOUNDARY_COMMIT_FAULT_NONE,
        .committed = false,
        .stable_materialization_used = false,
    };
    if (result == null) return promotion;

    bool allow_stable_materialization =
        planned_route == BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION;
    bool allow_compatibility_destination =
        planned_route == BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION;

    if (allow_compatibility_destination && result.tag == ERROR) {
        Value* destination_error = boundary_build_destination_error_escape(
            result,
            interp,
            target_scope,
            active_ctx
        );
        if (!active_ctx.aborted &&
            destination_error != null &&
            destination_error != result &&
            destination_error.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_error;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
            promotion.committed = true;
            return promotion;
        }
        promotion.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_ERROR_BUILD;
        return promotion;
    }

    if (allow_stable_materialization && stable_escape_materialization_candidate(result.tag)) {
        Value* destination_value = stable_escape_prepare_and_materialize_value(
            interp,
            result,
            target_scope,
            releasing_scope,
            active_ctx
        );
        bool stable_materialized = destination_value != null;
        if (!active_ctx.aborted &&
            boundary_commit_destination_promoted_error(destination_value, target_scope)) {
            promotion.value = destination_value;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
            promotion.committed = true;
            promotion.stable_materialization_used = stable_materialized;
            return promotion;
        }
        if (!active_ctx.aborted &&
            destination_value != null &&
            destination_value != result &&
            destination_value.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_value;
            promotion.outcome = result.tag == CONS
                ? BOUNDARY_COMMIT_DESTINATION_CONS_BUILT
                : BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
            promotion.committed = true;
            promotion.stable_materialization_used = stable_materialized;
            return promotion;
        }
        promotion.fault_code = result.tag == CONS
            ? BOUNDARY_COMMIT_FAULT_DESTINATION_CONS_BUILD
            : BOUNDARY_COMMIT_FAULT_DESTINATION_DIRECT_PROMOTION;
        return promotion;
    }

    if (allow_compatibility_destination && result.tag == CONS) {
        Value* destination_cons = boundary_build_destination_cons_escape(
            result,
            interp,
            target_scope,
            releasing_scope,
            active_ctx
        );
        if (!active_ctx.aborted &&
            destination_cons != null &&
            destination_cons != result &&
            destination_cons.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_cons;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_CONS_BUILT;
            promotion.committed = true;
            return promotion;
        }
        promotion.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_CONS_BUILD;
        return promotion;
    }

    if (allow_compatibility_destination && result.tag == PARTIAL_PRIM) {
        Value* destination_partial = boundary_build_destination_partial_escape(
            result,
            interp,
            target_scope,
            releasing_scope,
            active_ctx
        );
        if (!active_ctx.aborted &&
            boundary_commit_destination_promoted_error(destination_partial, target_scope)) {
            promotion.value = destination_partial;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
            promotion.committed = true;
            return promotion;
        }
        if (!active_ctx.aborted &&
            destination_partial != null &&
            destination_partial != result &&
            destination_partial.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_partial;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_PARTIAL_BUILT;
            promotion.committed = true;
            return promotion;
        }
        promotion.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_PARTIAL_BUILD;
        return promotion;
    }

    if (allow_compatibility_destination && result.tag == ITERATOR) {
        Value* destination_iterator = boundary_build_destination_iterator_escape(
            result,
            interp,
            target_scope,
            releasing_scope,
            active_ctx
        );
        if (!active_ctx.aborted &&
            boundary_commit_destination_promoted_error(destination_iterator, target_scope)) {
            promotion.value = destination_iterator;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
            promotion.committed = true;
            return promotion;
        }
        if (!active_ctx.aborted &&
            destination_iterator != null &&
            destination_iterator != result &&
            destination_iterator.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_iterator;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_ITERATOR_BUILT;
            promotion.committed = true;
            return promotion;
        }
        promotion.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_ITERATOR_BUILD;
        return promotion;
    }

    if (allow_compatibility_destination && allow_direct_promotion) {
        Value* destination_direct = boundary_promote_to_escape_ctx(result, interp, active_ctx);
        if (!active_ctx.aborted &&
            destination_direct != null &&
            destination_direct != result &&
            destination_direct.scope_gen == target_scope.escape_generation) {
            promotion.value = destination_direct;
            promotion.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
            promotion.committed = true;
            return promotion;
        }
        promotion.fault_code = BOUNDARY_COMMIT_FAULT_DESTINATION_DIRECT_PROMOTION;
    }
    return promotion;
}
