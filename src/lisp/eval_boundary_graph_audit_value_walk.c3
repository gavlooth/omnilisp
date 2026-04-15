module lisp;

import main;

fn BoundaryGraphAuditResult boundary_graph_audit_visit_value(
    Value* cur,
    Value** value_stack,
    usz* value_stack_len,
    Env** env_stack,
    usz* env_stack_len,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* forbidden_scope,
    Env* global_env,
    bool emit,
    usz root_tag,
    usz root_scope_gen
) {
    (void)global_env;
    if (boundary_value_points_into_temp(cur, target_scope, forbidden_scope)) {
        return boundary_graph_audit_result(
            emit,
            BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_VALUE,
            target_scope,
            root_tag,
            (usz)cur.tag,
            root_scope_gen,
            (usz)cur.scope_gen
        );
    }

    BoundaryGraphEdgeClass edge_class = boundary_graph_audit_edge_class(cur.tag);
    switch (cur.tag) {
        case CONS:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: CONS edge class mismatch");
            if (!boundary_graph_audit_push_value(value_stack, value_stack_len, cur.cons_val.car) ||
                !boundary_graph_audit_push_value(value_stack, value_stack_len, cur.cons_val.cdr)) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                    root_scope_gen,
                    0
                );
            }
            break;
        case ARRAY:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: ARRAY edge class mismatch");
            if (cur.array_val != null) {
                for (usz i = 0; i < cur.array_val.length; i++) {
                    if (!boundary_graph_audit_push_value(value_stack, value_stack_len, cur.array_val.items[i])) {
                        return boundary_graph_audit_result(
                            emit,
                            BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                            target_scope,
                            root_tag,
                            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                            root_scope_gen,
                            0
                        );
                    }
                }
            }
            break;
        case HASHMAP:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: HASHMAP edge class mismatch");
            if (cur.hashmap_val != null && cur.hashmap_val.entries != null) {
                for (uint i = 0; i < cur.hashmap_val.capacity; i++) {
                    Value* key = cur.hashmap_val.entries[i].key;
                    if (key == null) continue;
                    Value* val = cur.hashmap_val.entries[i].value;
                    if (!boundary_graph_audit_push_value(value_stack, value_stack_len, key) ||
                        !boundary_graph_audit_push_value(value_stack, value_stack_len, val)) {
                        return boundary_graph_audit_result(
                            emit,
                            BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                            target_scope,
                            root_tag,
                            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                            root_scope_gen,
                            0
                        );
                    }
                }
            }
            break;
        case SET:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: SET edge class mismatch");
            if (cur.hashmap_val != null && cur.hashmap_val.entries != null) {
                for (uint i = 0; i < cur.hashmap_val.capacity; i++) {
                    Value* key = cur.hashmap_val.entries[i].key;
                    if (key == null) continue;
                    if (!boundary_graph_audit_push_value(value_stack, value_stack_len, key)) {
                        return boundary_graph_audit_result(
                            emit,
                            BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                            target_scope,
                            root_tag,
                            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                            root_scope_gen,
                            0
                        );
                    }
                }
            }
            break;
        case CLOSURE:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_ENV, "boundary graph audit: CLOSURE edge class mismatch");
            if (cur.closure_val != null &&
                !boundary_graph_audit_push_env(env_stack, env_stack_len, cur.closure_val.env)) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_PUSH_ENV_OVERFLOW,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                    root_scope_gen,
                    0
                );
            }
            break;
        case PARTIAL_PRIM:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: PARTIAL_PRIM edge class mismatch");
            if (!boundary_graph_audit_push_value(value_stack, value_stack_len, cur.partial_val.first_arg) ||
                !boundary_graph_audit_push_value(value_stack, value_stack_len, cur.partial_val.second_arg)) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                    root_scope_gen,
                    0
                );
            }
            break;
        case METHOD_TABLE:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: METHOD_TABLE edge class mismatch");
            if (cur.method_table_val != null) {
                MethodTable* mt = cur.method_table_val;
                for (usz i = 0; i < mt.entry_count; i++) {
                    if (!boundary_graph_audit_push_value(value_stack, value_stack_len, mt.entries[i].implementation)) {
                        return boundary_graph_audit_result(
                            emit,
                            BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                            target_scope,
                            root_tag,
                            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                            root_scope_gen,
                            0
                        );
                    }
                }
                if (!boundary_graph_audit_push_value(value_stack, value_stack_len, mt.fallback)) {
                    return boundary_graph_audit_result(
                        emit,
                        BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                        target_scope,
                        root_tag,
                        BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                        root_scope_gen,
                        0
                    );
                }
            }
            break;
        case MODULE:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_ENV, "boundary graph audit: MODULE edge class mismatch");
            if (cur.module_val != null &&
                !boundary_graph_audit_push_env(env_stack, env_stack_len, cur.module_val.env)) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_PUSH_ENV_OVERFLOW,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                    root_scope_gen,
                    0
                );
            }
            break;
        case ITERATOR:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: ITERATOR edge class mismatch");
            if (!boundary_graph_audit_push_value(value_stack, value_stack_len, cur.iterator_val)) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                    root_scope_gen,
                    0
                );
            }
            break;
        case INSTANCE:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: INSTANCE edge class mismatch");
            if (cur.instance_val != null && cur.instance_val.fields != null) {
                for (usz i = 0; i < cur.instance_val.field_count; i++) {
                    if (!boundary_graph_audit_push_value(value_stack, value_stack_len, &cur.instance_val.fields[i])) {
                        return boundary_graph_audit_result(
                            emit,
                            BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                            target_scope,
                            root_tag,
                            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                            root_scope_gen,
                            0
                        );
                    }
                }
            }
            break;
        case TENSOR:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_VALUE, "boundary graph audit: TENSOR edge class mismatch");
            if (cur.tensor_val != null) {
                TensorVal* tensor = cur.tensor_val;
                switch (tensor.kind) {
                    case TENSOR_PAYLOAD_CONCRETE:
                        break;
                    case TENSOR_PAYLOAD_MAP:
                        // Builtin dispatch method tables are shared callable descriptors;
                        // Tensor expression ownership is about captured function objects
                        // and tensor operands, not cloning global dispatch registries.
                        if (!(tensor.map_func == null ||
                                tensor.map_func.tag == METHOD_TABLE ||
                                boundary_graph_audit_push_value(value_stack, value_stack_len, tensor.map_func)) ||
                            !boundary_graph_audit_push_value(value_stack, value_stack_len, tensor.map_left) ||
                            !boundary_graph_audit_push_value(value_stack, value_stack_len, tensor.map_right)) {
                            return boundary_graph_audit_result(
                                emit,
                                BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                                target_scope,
                                root_tag,
                                BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                                root_scope_gen,
                                0
                            );
                        }
                        break;
                    case TENSOR_PAYLOAD_CONTRACT:
                        if (!boundary_graph_audit_push_value(value_stack, value_stack_len, tensor.contract_left) ||
                            !boundary_graph_audit_push_value(value_stack, value_stack_len, tensor.contract_right)) {
                            return boundary_graph_audit_result(
                                emit,
                                BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                                target_scope,
                                root_tag,
                                BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                                root_scope_gen,
                                0
                            );
                        }
                        break;
                }
            }
            break;
        case NIL:
        case INT:
        case STRING:
        case SYMBOL:
        case CONTINUATION:
        case PRIMITIVE:
        case ERROR:
        case DOUBLE:
        case BIG_INTEGER:
        case BIG_FLOAT:
        case BIG_COMPLEX:
        case FFI_HANDLE:
        case TYPE_INFO:
        case COROUTINE:
        case TIME_POINT:
        case VOID:
            assert(edge_class == BOUNDARY_GRAPH_EDGE_NONE, "boundary graph audit: leaf edge class mismatch");
            break;
    }

    return {
        .ok = true,
        .reason = BOUNDARY_GRAPH_AUDIT_OK,
        .root_tag = root_tag,
        .violating_edge_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
        .root_scope_gen = root_scope_gen,
        .violating_scope_gen = 0,
        .target_scope_gen = (usz)target_scope.generation,
        .target_escape_gen = (usz)target_scope.escape_generation
    };
}
