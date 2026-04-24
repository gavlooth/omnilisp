module lisp;

import main;
import std::core::mem;

const usz BOUNDARY_ALIAS_MAX_VISITS = 2048;
const usz BOUNDARY_ALIAS_MAX_DEPTH = 128;
const usz BOUNDARY_ALIAS_SEEN_FAST_CAPACITY = 256;
const usz BOUNDARY_ALIAS_SEEN_FAST_MASK = BOUNDARY_ALIAS_SEEN_FAST_CAPACITY - 1;
const usz BOUNDARY_ALIAS_STACK_MIN_HEADROOM = 64 * 1024;
const usz BOUNDARY_ALIAS_REACH_MAX_NODES = 2048;

struct BoundaryAliasVisit {
    Value* value;
    usz depth;
}

struct BoundaryAliasSeenFastSet {
    ushort[BOUNDARY_ALIAS_SEEN_FAST_CAPACITY] slots;
    bool saturated;
}

struct BoundaryAliasReachScan {
    Value*[BOUNDARY_ALIAS_REACH_MAX_NODES] value_stack;
    Value*[BOUNDARY_ALIAS_REACH_MAX_NODES] seen_values;
    Env*[BOUNDARY_ALIAS_REACH_MAX_NODES] env_stack;
    Env*[BOUNDARY_ALIAS_REACH_MAX_NODES] seen_envs;
}

struct BoundaryAliasGraphScan {
    BoundaryAliasVisit[BOUNDARY_ALIAS_MAX_VISITS] stack;
    Value*[BOUNDARY_ALIAS_MAX_VISITS] seen;
    BoundaryAliasSeenFastSet fast_set;
}

<*
@require stack != null : "boundary_alias_push_visit: null stack"
@require stack_len != null : "boundary_alias_push_visit: null stack_len"
@require seen != null : "boundary_alias_push_visit: null seen"
@require seen_len != null : "boundary_alias_push_visit: null seen_len"
@require fast_set != null : "boundary_alias_push_visit: null fast_set"
*>
fn bool boundary_alias_push_visit(
    BoundaryAliasVisit* stack,
    usz* stack_len,
    Value** seen,
    usz* seen_len,
    BoundaryAliasSeenFastSet* fast_set,
    Value* value,
    usz depth
) @inline {
    if (value == null) return false;
    if (!boundary_alias_value_has_child_payload(value)) return false;
    if (depth >= BOUNDARY_ALIAS_MAX_DEPTH) return true;
    if (boundary_alias_value_seen_fast(fast_set, seen, *seen_len, value)) return false;
    if (*seen_len >= BOUNDARY_ALIAS_MAX_VISITS) return true;
    usz seen_index = *seen_len;
    seen[seen_index] = value;
    (*seen_len)++;
    boundary_alias_seen_fast_record(fast_set, seen, *seen_len, seen_index, value);
    if (*stack_len >= BOUNDARY_ALIAS_MAX_VISITS) return true;
    stack[*stack_len] = { .value = value, .depth = depth };
    (*stack_len)++;
    return false;
}

<*
@require stack != null : "boundary_alias_push_child: null stack"
@require stack_len != null : "boundary_alias_push_child: null stack_len"
@require seen != null : "boundary_alias_push_child: null seen"
@require seen_len != null : "boundary_alias_push_child: null seen_len"
@require fast_set != null : "boundary_alias_push_child: null fast_set"
@require interp != null : "boundary_alias_push_child: null interp"
*>
fn bool boundary_alias_push_child(
    BoundaryAliasVisit* stack,
    usz* stack_len,
    Value** seen,
    usz* seen_len,
    BoundaryAliasSeenFastSet* fast_set,
    Value* child,
    Interp* interp,
    usz depth
) @inline {
    if (child == null || interp == null) return false;
    if (boundary_value_in_releasing_scope(child, interp)) return true;
    if (!boundary_ptr_in_target_scope_chain_with_hint((void*)child, interp, child.scope_gen)) return true;
    return boundary_alias_push_visit(stack, stack_len, seen, seen_len, fast_set, child, depth + 1);
}

fn bool boundary_graph_alias_unsafe_for_reuse(Value* v, Interp* interp, usz depth = 0) @noinline {
    if (v == null || interp == null) return false;
    if (depth >= BOUNDARY_ALIAS_MAX_DEPTH) {
        boundary_escape_shape_note_root(v, true);
        return true;
    }
    if (!boundary_alias_value_has_child_payload(v)) {
        boundary_escape_shape_note_root(v, false);
        return false;
    }
    if (!main::stack_has_apply_headroom(BOUNDARY_ALIAS_STACK_MIN_HEADROOM)) {
        boundary_escape_shape_note_root(v, true);
        return true;
    }

    bool unsafe = boundary_graph_alias_unsafe_for_reuse_walk(v, interp, depth);
    boundary_escape_shape_note_root(v, unsafe);
    return unsafe;
}

<*
@require v != null : "boundary_graph_alias_unsafe_for_reuse_walk: null value"
@require interp != null : "boundary_graph_alias_unsafe_for_reuse_walk: null interp"
@require depth < BOUNDARY_ALIAS_MAX_DEPTH : "boundary_graph_alias_unsafe_for_reuse_walk: depth exhausted"
*>
fn bool boundary_graph_alias_unsafe_for_reuse_walk(Value* v, Interp* interp, usz depth) @noinline {
    // This scan can run on small continuation stacks during effect resume.
    // Keep the large traversal buffers off the C stack.
    BoundaryAliasGraphScan* scan = (BoundaryAliasGraphScan*)mem::malloc(BoundaryAliasGraphScan.sizeof);
    if (scan == null) return true;
    defer mem::free(scan);
    *scan = {};
    usz stack_len = 0;
    usz seen_len = 0;
    if (boundary_alias_push_visit(&scan.stack[0], &stack_len, &scan.seen[0], &seen_len, &scan.fast_set, v, depth)) return true;

    while (stack_len > 0) {
        BoundaryAliasVisit visit = scan.stack[--stack_len];
        Value* cur = visit.value;
        usz cur_depth = visit.depth;
        if (cur == null) continue;
        if (cur_depth >= BOUNDARY_ALIAS_MAX_DEPTH) return true;

        switch (cur.tag) {
            case CLOSURE:
                if (boundary_closure_alias_unsafe_for_reuse(cur, interp)) return true;
                break;
            case PARTIAL_PRIM:
                if (cur.partial_val.first_arg != null &&
                    boundary_alias_push_child(
                        &scan.stack[0],
                        &stack_len,
                        &scan.seen[0],
                        &seen_len,
                        &scan.fast_set,
                        cur.partial_val.first_arg,
                        interp,
                        cur_depth
                    )) {
                    return true;
                }
                if (cur.partial_val.second_arg != null &&
                    boundary_alias_push_child(
                        &scan.stack[0],
                        &stack_len,
                        &scan.seen[0],
                        &seen_len,
                        &scan.fast_set,
                        cur.partial_val.second_arg,
                        interp,
                        cur_depth
                    )) {
                    return true;
                }
                break;
            case ITERATOR:
                if (cur.iterator_val != null &&
                    boundary_alias_push_child(
                        &scan.stack[0],
                        &stack_len,
                        &scan.seen[0],
                        &seen_len,
                        &scan.fast_set,
                        cur.iterator_val,
                        interp,
                        cur_depth
                    )) {
                    return true;
                }
                break;
            case CONS:
                Value* current = cur;
                usz list_depth = cur_depth;
                while (current != null && current.tag == CONS) {
                    if (list_depth >= BOUNDARY_ALIAS_MAX_DEPTH) return true;

                    Value* car = current.cons_val.car;
                    if (car != null) {
                        if (boundary_alias_push_child(
                            &scan.stack[0],
                            &stack_len,
                            &scan.seen[0],
                            &seen_len,
                            &scan.fast_set,
                            car,
                            interp,
                            list_depth
                        )) {
                            return true;
                        }
                    }

                    Value* cdr = current.cons_val.cdr;
                    if (cdr == null) break;
                    if (cdr.tag != CONS) {
                        if (boundary_alias_push_child(
                            &scan.stack[0],
                            &stack_len,
                            &scan.seen[0],
                            &seen_len,
                            &scan.fast_set,
                            cdr,
                            interp,
                            list_depth
                        )) {
                            return true;
                        }
                        break;
                    }
                    if (boundary_value_in_releasing_scope(cdr, interp)) return true;

                    current = cdr;
                    list_depth++;
                }
                break;
            case ARRAY:
                if (cur.array_val != null) {
                    Array* arr = cur.array_val;
foreach (item : arr.items[0:arr.length]) {
                        if (boundary_alias_push_child(
                            &scan.stack[0],
                            &stack_len,
                            &scan.seen[0],
                            &seen_len,
                            &scan.fast_set,
                            item,
                            interp,
                            cur_depth
                        )) {
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
                        if (boundary_alias_push_child(&scan.stack[0], &stack_len, &scan.seen[0], &seen_len, &scan.fast_set, key, interp, cur_depth)) {
                            return true;
                        }
                        if (cur.tag == HASHMAP &&
                            boundary_alias_push_child(
                                &scan.stack[0],
                                &stack_len,
                                &scan.seen[0],
                                &seen_len,
                                &scan.fast_set,
                                map.entries[i].value,
                                interp,
                                cur_depth
                            )) {
                            return true;
                        }
                    }
                }
                break;
            case METHOD_TABLE:
                if (cur.method_table_val != null) {
                    MethodTable* mt = cur.method_table_val;
                    for (usz i = 0; i < mt.entry_count; i++) {
                        if (boundary_alias_push_child(
                            &scan.stack[0],
                            &stack_len,
                            &scan.seen[0],
                            &seen_len,
                            &scan.fast_set,
                            mt.entries[i].implementation,
                            interp,
                            cur_depth
                        )) {
                            return true;
                        }
                    }
                    if (boundary_alias_push_child(
                        &scan.stack[0],
                        &stack_len,
                        &scan.seen[0],
                        &seen_len,
                        &scan.fast_set,
                        mt.fallback,
                        interp,
                        cur_depth
                    )) {
                        return true;
                    }
                }
                break;
            case MODULE:
            case INSTANCE:
                if (boundary_alias_graph_reaches_scope(cur, interp.releasing_scope, interp.global_env)) return true;
                break;
            case TENSOR:
                if (cur.tensor_val != null &&
                    (boundary_ptr_in_scope((void*)cur.tensor_val, interp.releasing_scope) ||
                     boundary_ptr_in_scope((void*)cur.tensor_val.shape, interp.releasing_scope) ||
                     boundary_ptr_in_scope((void*)cur.tensor_val.strides, interp.releasing_scope) ||
                     boundary_ptr_in_scope(cur.tensor_val.data, interp.releasing_scope) ||
                     boundary_ptr_in_scope((void*)cur.tensor_val.contract_left_axes, interp.releasing_scope) ||
                     boundary_ptr_in_scope((void*)cur.tensor_val.contract_right_axes, interp.releasing_scope))) {
                    return true;
                }
                if (cur.tensor_val != null) {
                    TensorVal* tensor = cur.tensor_val;
                    switch (tensor.kind) {
                        case TENSOR_PAYLOAD_CONCRETE:
                            break;
                        case TENSOR_PAYLOAD_MAP:
                            if (boundary_alias_push_child(&scan.stack[0], &stack_len, &scan.seen[0], &seen_len, &scan.fast_set, tensor.map_func, interp, cur_depth) ||
                                boundary_alias_push_child(&scan.stack[0], &stack_len, &scan.seen[0], &seen_len, &scan.fast_set, tensor.map_left, interp, cur_depth) ||
                                boundary_alias_push_child(&scan.stack[0], &stack_len, &scan.seen[0], &seen_len, &scan.fast_set, tensor.map_right, interp, cur_depth)) {
                                return true;
                            }
                            break;
                        case TENSOR_PAYLOAD_CONTRACT:
                            if (boundary_alias_push_child(&scan.stack[0], &stack_len, &scan.seen[0], &seen_len, &scan.fast_set, tensor.contract_left, interp, cur_depth) ||
                                boundary_alias_push_child(&scan.stack[0], &stack_len, &scan.seen[0], &seen_len, &scan.fast_set, tensor.contract_right, interp, cur_depth)) {
                                return true;
                            }
                            break;
                        case TENSOR_PAYLOAD_VIEW:
                            if (boundary_alias_push_child(&scan.stack[0], &stack_len, &scan.seen[0], &seen_len, &scan.fast_set, tensor.view_source, interp, cur_depth)) {
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
    return false;
}

<*
@require interp != null : "boundary_can_reuse_value: null interp"
*>
fn bool boundary_can_reuse_value_with_target_chain(
    Value* v,
    Interp* interp,
    bool in_target_scope_chain
) @inline {
    if (v == null) return false;
    boundary_assert_interp_scope_chain(interp);
    if (boundary_value_in_releasing_scope(v, interp)) return false;
    if (boundary_graph_alias_unsafe_for_reuse(v, interp)) return false;
    return in_target_scope_chain;
}

<*
@require interp != null : "boundary_can_reuse_value: null interp"
*>
fn bool boundary_can_reuse_value(Value* v, Interp* interp) @inline {
    if (v == null) return false;
    bool in_target_scope_chain =
        boundary_ptr_in_target_scope_chain_with_hint((void*)v, interp, v.scope_gen);
    return boundary_can_reuse_value_with_target_chain(v, interp, in_target_scope_chain);
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
    classification.in_target_scope_chain =
        boundary_ptr_in_scope_chain_with_hint_cached((void*)v, interp, target_scope, v.scope_gen);
    if (!classification.in_releasing_scope &&
        boundary_can_reuse_value_with_target_chain(v, interp, classification.in_target_scope_chain)) {
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

fn bool boundary_rewrite_spliced_escape_stamps(Value* root, main::ScopeRegion* parent, main::ScopeRegion* child) @noinline {
    if (root == null || parent == null || child == null) return root == null;

    Value** value_stack = (Value**)mem::malloc(Value*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Value** seen_values = (Value**)mem::malloc(Value*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Env** env_stack = (Env**)mem::malloc(Env*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Env** seen_envs = (Env**)mem::malloc(Env*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    if (value_stack == null || seen_values == null || env_stack == null || seen_envs == null) {
        if (value_stack != null) mem::free(value_stack);
        if (seen_values != null) mem::free(seen_values);
        if (env_stack != null) mem::free(env_stack);
        if (seen_envs != null) mem::free(seen_envs);
        return false;
    }
    defer {
        mem::free(value_stack);
        mem::free(seen_values);
        mem::free(env_stack);
        mem::free(seen_envs);
    }

    usz value_stack_len = 0;
    usz seen_value_len = 0;
    usz env_stack_len = 0;
    usz seen_env_len = 0;
    if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, root)) return false;

    usz root_tag = (usz)root.tag;
    usz root_scope_gen = (usz)root.scope_gen;
    while (value_stack_len > 0 || env_stack_len > 0) {
        while (value_stack_len > 0) {
            Value* cur = value_stack[--value_stack_len];
            if (cur == null) continue;
            if (boundary_graph_audit_value_seen(seen_values, seen_value_len, cur)) continue;
            if (seen_value_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) return false;
            seen_values[seen_value_len++] = cur;

            if (main::scope_contains(child, (void*)cur) && cur.scope_gen == child.escape_generation) {
                cur.scope_gen = parent.escape_generation;
            }
            if (cur.tag == CLOSURE && cur.closure_val != null) {
                if (cur.closure_val.scope_gen == child.escape_generation) {
                    cur.closure_val.scope_gen = parent.escape_generation;
                }
                if (cur.closure_val.env_scope == child) {
                    cur.closure_val.env_scope = parent;
                }
            }

            BoundaryGraphAuditResult step = boundary_graph_audit_visit_value(
                cur,
                value_stack,
                &value_stack_len,
                env_stack,
                &env_stack_len,
                parent,
                child,
                null,
                false,
                root_tag,
                root_scope_gen
            );
            if (!step.ok) return false;
        }

        while (env_stack_len > 0) {
            Env* env = env_stack[--env_stack_len];
            if (env == null) continue;
            if (boundary_graph_audit_env_seen(seen_envs, seen_env_len, env)) continue;
            if (seen_env_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) return false;
            seen_envs[seen_env_len++] = env;

            if (main::scope_contains(child, (void*)env) && env.scope_gen == child.escape_generation) {
                env.scope_gen = parent.escape_generation;
            }
            if (env.owner_scope == child && env.owner_escape_lane) {
                env.owner_scope = parent;
            }

            BoundaryGraphAuditResult step = boundary_graph_audit_visit_env(
                env,
                value_stack,
                &value_stack_len,
                env_stack,
                &env_stack_len,
                parent,
                child,
                null,
                false,
                root_tag,
                root_scope_gen
            );
            if (!step.ok) return false;
        }
    }
    return true;
}

fn BoundaryScopeTransferResult boundary_try_scope_splice_escapes_root_result(
    main::ScopeRegion* parent,
    main::ScopeRegion* child,
    Value* root,
    Env* global_env = null
) {
    BoundaryScopeTransferResult result = boundary_check_scope_transfer(parent, child);
    if (result.allowed) {
        if (!boundary_debug_graph_audit_pre_splice_escape_root(root, parent, child, global_env)) {
            result.allowed = false;
            result.reason = BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID;
        } else if (!boundary_rewrite_spliced_escape_stamps(root, parent, child)) {
            result.allowed = false;
            result.reason = BOUNDARY_SCOPE_TRANSFER_CHILD_ESCAPE_LANE_INVALID;
        } else {
            main::scope_splice_escapes(parent, child);
        }
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
        case FLOAT32:
        case BIG_INTEGER:
        case BIG_FLOAT:
        case BIG_COMPLEX:
        case COMPLEX128:
        case COMPLEX64:
        case SYMBOL:
        case TIME_POINT:
        case TENSOR:
            return true;
        case STRING:
        case CONS:
        case CLOSURE:
        case CONTINUATION:
        case PRIMITIVE:
        case PARTIAL_PRIM:
        case ERROR:
        case HASHMAP:
        case SET:
        case FFI_HANDLE:
        case ARRAY:
        case TYPE_INFO:
        case INSTANCE:
        case METHOD_TABLE:
        case MODULE:
        case ITERATOR:
        case COROUTINE:
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
