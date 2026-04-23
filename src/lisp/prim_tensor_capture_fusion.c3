module lisp;

fn bool tensor_capture_fusion_node_is(Value* node, Interp* interp, char[] kind_name) {
    if (node == null || node.tag != HASHMAP) return false;
    return nn_symbol_is(nn_dict_get(node, interp, "kind"), interp, kind_name);
}

fn Value* tensor_capture_fusion_node_id(Value* node, Interp* interp) {
    if (node == null || node.tag != HASHMAP) return null;
    Value* id = nn_dict_get(node, interp, "id");
    if (id == null || id.tag != INT) return null;
    return id;
}

fn Value* tensor_capture_fusion_ids(Value* nodes, Interp* interp, usz count) {
    Value* ids = make_array_checked(interp, count, "tensor/capture: fusion ids out of memory");
    if (ids == null || ids.tag == ERROR) return ids;
    if (ids.array_val == null) return nn_oom(interp);
foreach (item : nodes.array_val.items[0:nodes.array_val.length]) {
        Value* node = item;
        if (!tensor_capture_fusion_node_is(node, interp, "tensor-map")) continue;
        Value* id = tensor_capture_fusion_node_id(node, interp);
        if (id == null) return tensor_runtime_raise(interp, "tensor/capture: fusion node id is missing");
        if (Value* err = tensor_capture_array_push(ids, id, interp)) return err;
}
    return ids;
}

fn Value* tensor_capture_fusion_candidate(Value* nodes, Interp* interp, usz map_count) {
    Value* candidate = nn_make_dict(interp, 10);
    if (candidate == null || candidate.tag == ERROR) return candidate;
    Value* ids = tensor_capture_fusion_ids(nodes, interp, map_count);
    if (ids == null || ids.tag == ERROR) return ids;
    if (Value* err = nn_dict_set(candidate, interp, "id", make_int(interp, 0))) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "kind", "map-chain")) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "execution", "not-fused")) return err;
    if (Value* err = nn_dict_set(candidate, interp, "nodes", ids)) return err;
    if (Value* err = nn_dict_set(candidate, interp, "operation-count", tensor_capture_usz_value(interp, map_count, "tensor/capture: fusion operation-count is too large"))) return err;
    if (Value* err = nn_dict_set(candidate, interp, "requires-runtime-support", make_bool_value(interp, true))) return err;
    if (Value* err = nn_dict_set(candidate, interp, "fused", make_bool_value(interp, false))) return err;
    return candidate;
}

fn char[] tensor_capture_fusion_barrier_reason(Value* node, Interp* interp) {
    if (tensor_capture_fusion_node_is(node, interp, "tensor-contract")) return "contract-boundary";
    if (tensor_capture_fusion_node_is(node, interp, "tensor-view")) return "view-boundary";
    return "";
}

fn Value* tensor_capture_fusion_barriers(Value* nodes, Interp* interp, usz barrier_count) {
    Value* barriers = make_array_checked(interp, barrier_count, "tensor/capture: fusion barriers out of memory");
    if (barriers == null || barriers.tag == ERROR) return barriers;
    if (barriers.array_val == null) return nn_oom(interp);
foreach (item : nodes.array_val.items[0:nodes.array_val.length]) {
        Value* node = item;
        char[] reason = tensor_capture_fusion_barrier_reason(node, interp);
        if (reason.len == 0) continue;
        Value* id = tensor_capture_fusion_node_id(node, interp);
        if (id == null) return tensor_runtime_raise(interp, "tensor/capture: fusion barrier node id is missing");
        Value* barrier = nn_make_dict(interp, 4);
        if (barrier == null || barrier.tag == ERROR) return barrier;
        if (Value* err = nn_dict_set(barrier, interp, "node", id)) return err;
        if (Value* err = nn_dict_set(barrier, interp, "kind", nn_dict_get(node, interp, "kind"))) return err;
        if (Value* err = nn_dict_set_symbol(barrier, interp, "reason", reason)) return err;
        if (Value* err = tensor_capture_array_push(barriers, barrier, interp)) return err;
}
    return barriers;
}

fn Value* tensor_capture_fusion_plan(Value* nodes, Interp* interp) {
    if (nodes == null || nodes.tag != ARRAY || nodes.array_val == null) return nn_oom(interp);
    usz map_count = 0;
    usz launch_count = 0;
    usz barrier_count = 0;
foreach (item : nodes.array_val.items[0:nodes.array_val.length]) {
        Value* node = item;
        if (tensor_capture_node_launches(node, interp)) launch_count++;
        if (tensor_capture_fusion_node_is(node, interp, "tensor-map")) map_count++;
        if (tensor_capture_fusion_barrier_reason(node, interp).len > 0) barrier_count++;
}

    bool eligible = map_count >= 2 && map_count == launch_count && barrier_count == 0;
    Value* candidates = make_array_checked(interp, eligible ? 1 : 0, "tensor/capture: fusion candidates out of memory");
    if (candidates == null || candidates.tag == ERROR) return candidates;
    if (candidates.array_val == null) return nn_oom(interp);
    if (eligible) {
        Value* candidate = tensor_capture_fusion_candidate(nodes, interp, map_count);
        if (candidate == null || candidate.tag == ERROR) return candidate;
        if (Value* err = tensor_capture_array_push(candidates, candidate, interp)) return err;
    }

    Value* barriers = tensor_capture_fusion_barriers(nodes, interp, barrier_count);
    if (barriers == null || barriers.tag == ERROR) return barriers;
    Value* plan = nn_make_dict(interp, 12);
    if (plan == null || plan.tag == ERROR) return plan;
    if (Value* err = nn_dict_set_symbol(plan, interp, "kind", "tensor-fusion-plan")) return err;
    if (Value* err = nn_dict_set(plan, interp, "version", make_int(interp, 1))) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "policy", "eligibility-only")) return err;
    if (Value* err = nn_dict_set(plan, interp, "eligible", make_bool_value(interp, eligible))) return err;
    if (Value* err = nn_dict_set(plan, interp, "candidate-count", tensor_capture_usz_value(interp, eligible ? 1 : 0, "tensor/capture: fusion candidate-count is too large"))) return err;
    if (Value* err = nn_dict_set(plan, interp, "candidates", candidates)) return err;
    if (Value* err = nn_dict_set(plan, interp, "barrier-count", tensor_capture_usz_value(interp, barrier_count, "tensor/capture: fusion barrier-count is too large"))) return err;
    if (Value* err = nn_dict_set(plan, interp, "barriers", barriers)) return err;
    if (Value* err = nn_dict_set(plan, interp, "fused", make_bool_value(interp, false))) return err;
    return plan;
}
