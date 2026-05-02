module lisp;

import std::core::mem;
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
        case TENSOR:
            return true;
        default:
            return false;
    }
}

const usz JIT_TEMP_GRAPH_SCAN_MAX_NODES = 4096;
const usz JIT_TEMP_CONS_SPINE_SCAN_MAX_NODES = 65536;

struct JitTempGraphScan {
    Value*[JIT_TEMP_GRAPH_SCAN_MAX_NODES] value_stack;
    Value*[JIT_TEMP_GRAPH_SCAN_MAX_NODES] seen_values;
    Env*[JIT_TEMP_GRAPH_SCAN_MAX_NODES] env_stack;
    Env*[JIT_TEMP_GRAPH_SCAN_MAX_NODES] seen_envs;
}

fn bool jit_temp_graph_value_seen(Value** seen, usz seen_len, Value* v) @inline {
    for (usz i = 0; i < seen_len; i++) {
        if (seen[i] == v) return true;
    }
    return false;
}

fn bool jit_temp_graph_env_seen(Env** seen, usz seen_len, Env* env) @inline {
    for (usz i = 0; i < seen_len; i++) {
        if (seen[i] == env) return true;
    }
    return false;
}

fn bool jit_temp_graph_push_value(
    Value** stack,
    usz* stack_len,
    Value* v
) @inline {
    if (v == null) return true;
    if (*stack_len >= JIT_TEMP_GRAPH_SCAN_MAX_NODES) return false;
    stack[*stack_len] = v;
    (*stack_len)++;
    return true;
}

fn bool jit_temp_graph_push_env(
    Env** stack,
    usz* stack_len,
    Env* env
) @inline {
    if (env == null) return true;
    if (*stack_len >= JIT_TEMP_GRAPH_SCAN_MAX_NODES) return false;
    stack[*stack_len] = env;
    (*stack_len)++;
    return true;
}

fn bool jit_temp_graph_push_child_or_detect_temp(
    Value** stack,
    usz* stack_len,
    Value* value,
    main::ScopeRegion* scope
) @inline {
    if (value == null) return false;
    if (value_in_temp_lane(value, scope)) return true;
    if (!jit_value_is_graph_carrying(value)) return false;
    return !jit_temp_graph_push_value(stack, stack_len, value);
}

fn bool jit_graph_binding_reaches_temp_scope(
    Value* root,
    main::ScopeRegion* scope,
    Env* stop_at
) {
    if (root == null || scope == null) return false;

    JitTempGraphScan* scan = (JitTempGraphScan*)mem::malloc(JitTempGraphScan.sizeof);
    if (scan == null) return true;
    defer mem::free(scan);
    usz value_stack_len = 0;
    usz seen_value_len = 0;
    usz env_stack_len = 0;
    usz seen_env_len = 0;

    if (!jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, root)) {
        return true;
    }

    while (value_stack_len > 0 || env_stack_len > 0) {
        while (value_stack_len > 0) {
            Value* cur = scan.value_stack[--value_stack_len];
            if (cur == null) continue;
            if (jit_temp_graph_value_seen(&scan.seen_values[0], seen_value_len, cur)) continue;
            if (seen_value_len >= JIT_TEMP_GRAPH_SCAN_MAX_NODES) return true;
            scan.seen_values[seen_value_len++] = cur;

            if (value_in_temp_lane(cur, scope)) return true;

            switch (cur.tag) {
                case CONS:
                    Value* current = cur;
                    usz list_nodes = 0;
                    while (current != null && current.tag == CONS) {
                        if (list_nodes >= JIT_TEMP_CONS_SPINE_SCAN_MAX_NODES) return true;
                        if (value_in_temp_lane(current, scope)) return true;
                        if (jit_temp_graph_push_child_or_detect_temp(
                            &scan.value_stack[0],
                            &value_stack_len,
                            current.cons_val.car,
                            scope
                        )) {
                            return true;
                        }

                        Value* cdr = current.cons_val.cdr;
                        if (cdr == null) break;
                        if (cdr.tag != CONS) {
                            if (jit_temp_graph_push_child_or_detect_temp(
                                &scan.value_stack[0],
                                &value_stack_len,
                                cdr,
                                scope
                            )) {
                                return true;
                            }
                            break;
                        }
                        current = cdr;
                        list_nodes++;
                    }
                    break;
                case ARRAY:
                    if (cur.array_val != null) {
foreach (item : cur.array_val.items[0:cur.array_val.length]) {
                            if (jit_temp_graph_push_child_or_detect_temp(
                                &scan.value_stack[0],
                                &value_stack_len,
                                item,
                                scope
                            )) {
                                return true;
                            }
}
                    }
                    break;
                case HASHMAP:
                    if (cur.hashmap_val != null && cur.hashmap_val.entries != null) {
                        for (uint i = 0; i < cur.hashmap_val.capacity; i++) {
                            Value* key = cur.hashmap_val.entries[i].key;
                            if (key == null) continue;
                            if (jit_temp_graph_push_child_or_detect_temp(
                                &scan.value_stack[0],
                                &value_stack_len,
                                key,
                                scope
                            ) ||
                                jit_temp_graph_push_child_or_detect_temp(
                                    &scan.value_stack[0],
                                    &value_stack_len,
                                    cur.hashmap_val.entries[i].value,
                                    scope
                                )) {
                                return true;
                            }
                        }
                    }
                    break;
                case SET:
                    if (cur.hashmap_val != null && cur.hashmap_val.entries != null) {
                        for (uint i = 0; i < cur.hashmap_val.capacity; i++) {
                            Value* key = cur.hashmap_val.entries[i].key;
                            if (key == null) continue;
                            if (jit_temp_graph_push_child_or_detect_temp(
                                &scan.value_stack[0],
                                &value_stack_len,
                                key,
                                scope
                            )) {
                                return true;
                            }
                        }
                    }
                    break;
                case CLOSURE:
                    if (cur.closure_val != null) {
                        if (cur.closure_val.scope_gen == scope.generation ||
                            boundary_ptr_in_scope((void*)cur.closure_val, scope)) {
                            return true;
                        }
                        if (!jit_temp_graph_push_env(&scan.env_stack[0], &env_stack_len, cur.closure_val.env)) {
                            return true;
                        }
                    }
                    break;
                case PARTIAL_PRIM:
                    if (!jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, cur.partial_val.first_arg) ||
                        !jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, cur.partial_val.second_arg)) {
                        return true;
                    }
                    break;
                case METHOD_TABLE:
                    if (cur.method_table_val != null) {
                        MethodTable* mt = cur.method_table_val;
                        for (usz i = 0; i < mt.entry_count; i++) {
                            if (!jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, mt.entries[i].implementation)) {
                                return true;
                            }
                        }
                        if (!jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, mt.fallback)) {
                            return true;
                        }
                    }
                    break;
                case MODULE:
                    if (cur.module_val != null &&
                        !jit_temp_graph_push_env(&scan.env_stack[0], &env_stack_len, cur.module_val.env)) {
                        return true;
                    }
                    break;
                case ITERATOR:
                    if (!jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, cur.iterator_val)) {
                        return true;
                    }
                    break;
                case INSTANCE:
                    if (cur.instance_val != null && cur.instance_val.fields != null) {
                        for (usz i = 0; i < cur.instance_val.field_count; i++) {
                            if (!jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, &cur.instance_val.fields[i])) {
                                return true;
                            }
                        }
                    }
                    break;
                case TENSOR:
                    if (cur.tensor_val != null) {
                        TensorVal* tensor = cur.tensor_val;
                        switch (tensor.kind) {
                            case TENSOR_PAYLOAD_CONCRETE:
                                break;
                            case TENSOR_PAYLOAD_MAP:
                                if (!jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, tensor.map_func) ||
                                    !jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, tensor.map_left) ||
                                    !jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, tensor.map_right)) {
                                    return true;
                                }
                                break;
                            case TENSOR_PAYLOAD_CONTRACT:
                                if (!jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, tensor.contract_left) ||
                                    !jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, tensor.contract_right)) {
                                    return true;
                                }
                                break;
                            case TENSOR_PAYLOAD_VIEW:
                                if (!jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, tensor.view_source)) {
                                    return true;
                                }
                                break;
                        }
                    }
                    break;
                default:
                    break;
            }
        }

        while (env_stack_len > 0) {
            Env* env = scan.env_stack[--env_stack_len];
            if (env == null || env == stop_at) continue;
            if (jit_temp_graph_env_seen(&scan.seen_envs[0], seen_env_len, env)) continue;
            if (seen_env_len >= JIT_TEMP_GRAPH_SCAN_MAX_NODES) return true;
            scan.seen_envs[seen_env_len++] = env;

            if (env_frame_in_temp_lane(env, scope)) return true;
            if (!jit_temp_graph_push_env(&scan.env_stack[0], &env_stack_len, env.parent)) {
                return true;
            }
            for (usz i = 0; i < env.binding_count; i++) {
                if (!jit_temp_graph_push_value(&scan.value_stack[0], &value_stack_len, env.bindings[i].value)) {
                    return true;
                }
            }
        }
    }

    return false;
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
                jit_graph_binding_reaches_temp_scope(v, scope, stop_at)) {
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
    if (v.tag == TENSOR && v.tensor_val != null) {
        TensorVal* tensor = v.tensor_val;
        if (boundary_ptr_in_scope((void*)tensor, releasing_scope) ||
            boundary_ptr_in_scope((void*)tensor.shape, releasing_scope) ||
            boundary_ptr_in_scope((void*)tensor.strides, releasing_scope) ||
            boundary_ptr_in_scope(tensor.data, releasing_scope) ||
            boundary_ptr_in_scope((void*)tensor.contract_left_axes, releasing_scope) ||
            boundary_ptr_in_scope((void*)tensor.contract_right_axes, releasing_scope)) {
            return true;
        }
        switch (tensor.kind) {
            case TENSOR_PAYLOAD_CONCRETE:
                return false;
            case TENSOR_PAYLOAD_MAP:
                return value_in_temp_lane(tensor.map_func, releasing_scope) ||
                    value_in_temp_lane(tensor.map_left, releasing_scope) ||
                    value_in_temp_lane(tensor.map_right, releasing_scope);
            case TENSOR_PAYLOAD_CONTRACT:
                return value_in_temp_lane(tensor.contract_left, releasing_scope) ||
                    value_in_temp_lane(tensor.contract_right, releasing_scope);
            case TENSOR_PAYLOAD_VIEW:
                return value_in_temp_lane(tensor.view_source, releasing_scope);
        }
    }
    return false;
}

fn bool jit_binding_graph_needs_copy(Value* v, Interp* interp) {
    if (v == null || interp == null || interp.releasing_scope == null) return false;
    if (boundary_ptr_in_scope((void*)v, interp.releasing_scope)) return true;

    Value** stack = (Value**)mem::malloc(Value*.sizeof * 256);
    if (stack == null) return true;
    defer mem::free(stack);
    usz stack_cap = 256;
    usz stack_count = 0;

    stack[stack_count++] = v;

    while (stack_count > 0) {
        stack_count--;
        Value* current = stack[stack_count];

        if (boundary_ptr_in_scope((void*)current, interp.releasing_scope)) return true;

        switch (current.tag) {
            case CONS:
                {
                    if (!boundary_ptr_in_target_scope_chain_with_hint((void*)current, interp, current.scope_gen)) {
                        return true;
                    }
                    Value* spine = current;
                    usz spine_nodes = 0;
                    while (spine != null && spine.tag == CONS) {
                        if (spine_nodes >= JIT_TEMP_CONS_SPINE_SCAN_MAX_NODES) return true;

                        Value* car = spine.cons_val.car;
                        if (car != null) {
                            if (boundary_value_in_releasing_scope(car, interp)) return true;
                            if (!boundary_ptr_in_target_scope_chain_with_hint((void*)car, interp, car.scope_gen)) {
                                return true;
                            }
                            if (stack_count >= stack_cap) {
                                usz new_cap = stack_cap * 2;
                                Value** new_stack = (Value**)mem::malloc(Value*.sizeof * new_cap);
                                if (new_stack == null) return true;
                                for (usz i = 0; i < stack_count; i++) new_stack[i] = stack[i];
                                mem::free(stack);
                                stack = new_stack;
                                stack_cap = new_cap;
                            }
                            stack[stack_count++] = car;
                        }

                        Value* cdr = spine.cons_val.cdr;
                        if (cdr == null) break;
                        if (cdr.tag != CONS) {
                            if (boundary_value_in_releasing_scope(cdr, interp)) return true;
                            if (!boundary_ptr_in_target_scope_chain_with_hint((void*)cdr, interp, cdr.scope_gen)) {
                                return true;
                            }
                            if (stack_count >= stack_cap) {
                                usz new_cap = stack_cap * 2;
                                Value** new_stack = (Value**)mem::malloc(Value*.sizeof * new_cap);
                                if (new_stack == null) return true;
                                for (usz i = 0; i < stack_count; i++) new_stack[i] = stack[i];
                                mem::free(stack);
                                stack = new_stack;
                                stack_cap = new_cap;
                            }
                            stack[stack_count++] = cdr;
                            break;
                        }
                        if (boundary_value_in_releasing_scope(cdr, interp)) return true;
                        if (!boundary_ptr_in_target_scope_chain_with_hint((void*)cdr, interp, cdr.scope_gen)) {
                            return true;
                        }

                        spine = cdr;
                        spine_nodes++;
                    }
                }
            default:
                if (boundary_ptr_in_target_scope_chain_with_hint((void*)current, interp, current.scope_gen)) {
                    if (boundary_graph_alias_unsafe_for_reuse(current, interp, 0)) return true;
                }
                if (jit_value_is_graph_carrying(current)) return true;
                if (copy_to_parent_payload_in_releasing_scope(current, interp)) return true;
        }
    }
    return false;
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
