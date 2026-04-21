module lisp;

import main;
import std::core::mem;

<*
@require interp != null : "boundary_value_in_releasing_scope: null interp"
*>
fn bool boundary_value_in_releasing_scope(Value* v, Interp* interp) @inline {
    if (v == null || interp.releasing_scope == null) return false;
    return boundary_ptr_in_scope((void*)v, interp.releasing_scope);
}

fn bool boundary_alias_value_seen(Value** seen, usz seen_len, Value* value) @inline {
    for (usz i = 0; i < seen_len; i++) {
        if (seen[i] == value) return true;
    }
    return false;
}

fn usz boundary_alias_value_seen_hash(Value* value) @inline {
    usz h = (usz)value;
    return h ^ (h >> 4) ^ (h >> 12);
}

fn bool boundary_alias_value_seen_fast(
    BoundaryAliasSeenFastSet* fast_set,
    Value** seen,
    usz seen_len,
    Value* value
) @inline {
    if (fast_set == null || fast_set.saturated) return boundary_alias_value_seen(seen, seen_len, value);

    usz slot = boundary_alias_value_seen_hash(value) & BOUNDARY_ALIAS_SEEN_FAST_MASK;
    for (usz probe = 0; probe < BOUNDARY_ALIAS_SEEN_FAST_CAPACITY; probe++) {
        ushort encoded = fast_set.slots[slot];
        if (encoded == 0) return false;

        usz idx = (usz)encoded - 1;
        if (idx >= seen_len) {
            fast_set.saturated = true;
            return boundary_alias_value_seen(seen, seen_len, value);
        }
        if (seen[idx] == value) return true;
        slot = (slot + 1) & BOUNDARY_ALIAS_SEEN_FAST_MASK;
    }

    fast_set.saturated = true;
    return boundary_alias_value_seen(seen, seen_len, value);
}

fn void boundary_alias_seen_fast_record(
    BoundaryAliasSeenFastSet* fast_set,
    Value** seen,
    usz seen_len,
    usz index,
    Value* value
) @inline {
    if (fast_set == null || fast_set.saturated) return;
    if (index > (usz)ushort.max - 1) {
        fast_set.saturated = true;
        return;
    }

    usz slot = boundary_alias_value_seen_hash(value) & BOUNDARY_ALIAS_SEEN_FAST_MASK;
    for (usz probe = 0; probe < BOUNDARY_ALIAS_SEEN_FAST_CAPACITY; probe++) {
        ushort encoded = fast_set.slots[slot];
        if (encoded == 0) {
            fast_set.slots[slot] = (ushort)(index + 1);
            return;
        }

        usz idx = (usz)encoded - 1;
        if (idx >= seen_len) {
            fast_set.saturated = true;
            return;
        }
        if (seen[idx] == value) return;
        slot = (slot + 1) & BOUNDARY_ALIAS_SEEN_FAST_MASK;
    }

    fast_set.saturated = true;
}

fn bool boundary_alias_value_has_child_payload(Value* value) @inline {
    if (value == null) return false;
    switch (value.tag) {
        case CLOSURE:
        case PARTIAL_PRIM:
        case ITERATOR:
        case CONS:
        case ARRAY:
        case HASHMAP:
        case SET:
        case METHOD_TABLE:
        case INSTANCE:
        case MODULE:
        case TENSOR:
            return true;
        case NIL:
        case VOID:
        case INT:
        case DOUBLE:
        case FLOAT32:
        case BIG_INTEGER:
        case BIG_FLOAT:
        case BIG_COMPLEX:
        case COMPLEX128:
        case COMPLEX64:
        case STRING:
        case SYMBOL:
        case CONTINUATION:
        case PRIMITIVE:
        case ERROR:
        case FFI_HANDLE:
        case TYPE_INFO:
        case COROUTINE:
        case TIME_POINT:
            return false;
    }
}

fn bool boundary_alias_reach_value_seen(Value** seen, usz seen_len, Value* value) @inline {
    for (usz i = 0; i < seen_len; i++) {
        if (seen[i] == value) return true;
    }
    return false;
}

fn bool boundary_alias_reach_env_seen(Env** seen, usz seen_len, Env* env) @inline {
    for (usz i = 0; i < seen_len; i++) {
        if (seen[i] == env) return true;
    }
    return false;
}

fn bool boundary_alias_reach_push_value(Value** stack, usz* stack_len, Value* value) @inline {
    if (value == null) return true;
    if (*stack_len >= BOUNDARY_ALIAS_REACH_MAX_NODES) return false;
    stack[*stack_len] = value;
    (*stack_len)++;
    return true;
}

fn bool boundary_alias_reach_push_env(Env** stack, usz* stack_len, Env* env) @inline {
    if (env == null) return true;
    if (*stack_len >= BOUNDARY_ALIAS_REACH_MAX_NODES) return false;
    stack[*stack_len] = env;
    (*stack_len)++;
    return true;
}

fn bool boundary_alias_graph_reaches_scope(Value* root, main::ScopeRegion* scope, Env* stop_at) @noinline {
    if (root == null || scope == null) return false;

    BoundaryAliasReachScan* scan = (BoundaryAliasReachScan*)mem::malloc(BoundaryAliasReachScan.sizeof);
    if (scan == null) return true;
    defer mem::free(scan);

    usz value_stack_len = 0;
    usz seen_value_len = 0;
    usz env_stack_len = 0;
    usz seen_env_len = 0;

    if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, root)) return true;

    while (value_stack_len > 0 || env_stack_len > 0) {
        while (value_stack_len > 0) {
            Value* cur = scan.value_stack[--value_stack_len];
            if (cur == null) continue;
            if (boundary_alias_reach_value_seen(&scan.seen_values[0], seen_value_len, cur)) continue;
            if (seen_value_len >= BOUNDARY_ALIAS_REACH_MAX_NODES) return true;
            scan.seen_values[seen_value_len++] = cur;

            if (boundary_ptr_in_scope((void*)cur, scope) ||
                cur.scope_gen == scope.generation ||
                cur.scope_gen == scope.escape_generation) {
                return true;
            }

            switch (cur.tag) {
                case CONS:
                    if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, cur.cons_val.car) ||
                        !boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, cur.cons_val.cdr)) {
                        return true;
                    }
                    break;
                case ARRAY:
                    if (cur.array_val != null) {
                        for (usz i = 0; i < cur.array_val.length; i++) {
                            if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, cur.array_val.items[i])) {
                                return true;
                            }
                        }
                    }
                    break;
                case HASHMAP:
                case SET:
                    if (cur.hashmap_val != null && cur.hashmap_val.entries != null) {
                        HashMap* map = cur.hashmap_val;
                        for (uint i = 0; i < map.capacity; i++) {
                            Value* key = map.entries[i].key;
                            if (key == null) continue;
                            if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, key)) {
                                return true;
                            }
                            if (cur.tag == HASHMAP &&
                                !boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, map.entries[i].value)) {
                                return true;
                            }
                        }
                    }
                    break;
                case CLOSURE:
                    if (cur.closure_val != null) {
                        if (boundary_ptr_in_scope((void*)cur.closure_val, scope) ||
                            cur.closure_val.scope_gen == scope.generation ||
                            cur.closure_val.scope_gen == scope.escape_generation) {
                            return true;
                        }
                        if (!boundary_alias_reach_push_env(&scan.env_stack[0], &env_stack_len, cur.closure_val.env)) {
                            return true;
                        }
                    }
                    break;
                case PARTIAL_PRIM:
                    if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, cur.partial_val.first_arg) ||
                        !boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, cur.partial_val.second_arg)) {
                        return true;
                    }
                    break;
                case METHOD_TABLE:
                    if (cur.method_table_val != null) {
                        MethodTable* mt = cur.method_table_val;
                        for (usz i = 0; i < mt.entry_count; i++) {
                            if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, mt.entries[i].implementation)) {
                                return true;
                            }
                        }
                        if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, mt.fallback)) {
                            return true;
                        }
                    }
                    break;
                case MODULE:
                    if (cur.module_val != null &&
                        !boundary_alias_reach_push_env(&scan.env_stack[0], &env_stack_len, cur.module_val.env)) {
                        return true;
                    }
                    break;
                case ITERATOR:
                    if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, cur.iterator_val)) {
                        return true;
                    }
                    break;
                case INSTANCE:
                    if (cur.instance_val != null && cur.instance_val.fields != null) {
                        if (boundary_ptr_in_scope((void*)cur.instance_val, scope) ||
                            boundary_ptr_in_scope((void*)cur.instance_val.fields, scope)) {
                            return true;
                        }
                        for (usz i = 0; i < cur.instance_val.field_count; i++) {
                            if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, &cur.instance_val.fields[i])) {
                                return true;
                            }
                        }
                    }
                    break;
                case TENSOR:
                    if (cur.tensor_val != null) {
                        TensorVal* tensor = cur.tensor_val;
                        if (boundary_ptr_in_scope((void*)tensor, scope) ||
                            boundary_ptr_in_scope((void*)tensor.shape, scope) ||
                            boundary_ptr_in_scope((void*)tensor.strides, scope) ||
                            boundary_ptr_in_scope(tensor.data, scope) ||
                            boundary_ptr_in_scope((void*)tensor.contract_left_axes, scope) ||
                            boundary_ptr_in_scope((void*)tensor.contract_right_axes, scope)) {
                            return true;
                        }
                        switch (tensor.kind) {
                            case TENSOR_PAYLOAD_CONCRETE:
                                break;
                            case TENSOR_PAYLOAD_MAP:
                                if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, tensor.map_func) ||
                                    !boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, tensor.map_left) ||
                                    !boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, tensor.map_right)) {
                                    return true;
                                }
                                break;
                            case TENSOR_PAYLOAD_CONTRACT:
                                if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, tensor.contract_left) ||
                                    !boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, tensor.contract_right)) {
                                    return true;
                                }
                                break;
                            case TENSOR_PAYLOAD_VIEW:
                                if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, tensor.view_source)) {
                                    return true;
                                }
                                break;
                        }
                    }
                    break;
                case NIL:
                case VOID:
                case INT:
                case DOUBLE:
                case FLOAT32:
                case BIG_INTEGER:
                case BIG_FLOAT:
                case BIG_COMPLEX:
                case COMPLEX128:
                case COMPLEX64:
                case STRING:
                case SYMBOL:
                case CONTINUATION:
                case PRIMITIVE:
                case ERROR:
                case FFI_HANDLE:
                case TYPE_INFO:
                case COROUTINE:
                case TIME_POINT:
                    break;
            }
        }

        while (env_stack_len > 0) {
            Env* env = scan.env_stack[--env_stack_len];
            if (env == null || env == stop_at) continue;
            if (boundary_alias_reach_env_seen(&scan.seen_envs[0], seen_env_len, env)) continue;
            if (seen_env_len >= BOUNDARY_ALIAS_REACH_MAX_NODES) return true;
            scan.seen_envs[seen_env_len++] = env;

            if (!env.is_root_persistent_box() &&
                (boundary_ptr_in_scope((void*)env, scope) ||
                 env.scope_gen == scope.generation ||
                 env.scope_gen == scope.escape_generation)) {
                return true;
            }
            if (!boundary_alias_reach_push_env(&scan.env_stack[0], &env_stack_len, env.parent)) {
                return true;
            }
            for (usz i = 0; i < env.binding_count; i++) {
                if (!boundary_alias_reach_push_value(&scan.value_stack[0], &value_stack_len, env.bindings[i].value)) {
                    return true;
                }
            }
        }
    }

    return false;
}
