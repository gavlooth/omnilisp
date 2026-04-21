module lisp;

fn bool tensor_capture_selected_node_kind_is(Value* node, Interp* interp, char[] kind_name) {
    return node != null && node.tag == HASHMAP && nn_symbol_is(nn_dict_get(node, interp, "kind"), interp, kind_name);
}

fn bool tensor_capture_selected_node_id_is(Value* node, Interp* interp, usz expected) {
    Value* id = nn_dict_get(node, interp, "id");
    return id != null && id.tag == INT && id.int_val >= 0 && (usz)id.int_val == expected;
}

fn bool tensor_capture_selected_single_input_is(Value* node, Interp* interp, usz expected) {
    Value* inputs = nn_dict_get(node, interp, "inputs");
    if (inputs == null || inputs.tag != ARRAY || inputs.array_val == null || inputs.array_val.length != 1) return false;
    Value* input = inputs.array_val.items[0];
    return input != null && input.tag == INT && input.int_val >= 0 && (usz)input.int_val == expected;
}

fn bool tensor_capture_selected_tensor_map_inputs_are(Value* node, Interp* interp, usz left, usz right) {
    if (!tensor_capture_selected_node_kind_is(node, interp, "tensor-map")) return false;
    Value* inputs = nn_dict_get(node, interp, "inputs");
    Value* arity = nn_dict_get(node, interp, "arity");
    if (inputs == null || inputs.tag != ARRAY || inputs.array_val == null || inputs.array_val.length != 2 ||
        arity == null || arity.tag != INT || arity.int_val != 2 ||
        nn_dict_get(node, interp, "left-scalar") != null || nn_dict_get(node, interp, "right-scalar") != null) {
        return false;
    }
    Value* actual_left = inputs.array_val.items[0];
    Value* actual_right = inputs.array_val.items[1];
    return actual_left != null && actual_right != null &&
        actual_left.tag == INT && actual_right.tag == INT &&
        actual_left.int_val >= 0 && actual_right.int_val >= 0 &&
        (usz)actual_left.int_val == left && (usz)actual_right.int_val == right;
}

fn bool tensor_capture_selected_scalar_tail_is(Value* nodes, Interp* interp, usz first, usz previous) {
    if (nodes == null || nodes.tag != ARRAY || nodes.array_val == null || first > nodes.array_val.length) return false;
    for (usz i = first; i < nodes.array_val.length; i++) {
        Value* node = nodes.array_val.items[i];
        if (!tensor_capture_selected_node_kind_is(node, interp, "tensor-map") ||
            !tensor_capture_selected_node_id_is(node, interp, i) ||
            !tensor_capture_selected_single_input_is(node, interp, previous)) {
            return false;
        }
        Value* arity = nn_dict_get(node, interp, "arity");
        bool has_left_scalar = nn_dict_get(node, interp, "left-scalar") != null;
        bool has_right_scalar = nn_dict_get(node, interp, "right-scalar") != null;
        if (arity == null || arity.tag != INT || arity.int_val != 2 || has_left_scalar == has_right_scalar) return false;
        previous = i;
    }
    return true;
}

fn char[] tensor_capture_selected_region_family(Value* nodes, Interp* interp) {
    if (nodes == null || nodes.tag != ARRAY || nodes.array_val == null || nodes.array_val.length < 3) return "";
    if (!nn_symbol_is(nn_dict_get(nodes.array_val.items[0], interp, "kind"), interp, "tensor-source")) return "";
    if (!tensor_capture_selected_node_id_is(nodes.array_val.items[0], interp, 0)) return "";

    if (nodes.array_val.length >= 5 &&
        tensor_capture_selected_node_kind_is(nodes.array_val.items[1], interp, "tensor-view") &&
        tensor_capture_selected_single_input_is(nodes.array_val.items[1], interp, 0) &&
        tensor_capture_selected_node_kind_is(nodes.array_val.items[2], interp, "tensor-source") &&
        tensor_capture_selected_node_id_is(nodes.array_val.items[2], interp, 2) &&
        tensor_capture_selected_node_id_is(nodes.array_val.items[3], interp, 3) &&
        tensor_capture_selected_tensor_map_inputs_are(nodes.array_val.items[3], interp, 1, 2) &&
        tensor_capture_selected_scalar_tail_is(nodes, interp, 4, 3)) {
        return "view-tensor-scalar-map-chain";
    }

    if (nodes.array_val.length >= 3 &&
        tensor_capture_selected_node_kind_is(nodes.array_val.items[1], interp, "tensor-view") &&
        tensor_capture_selected_single_input_is(nodes.array_val.items[1], interp, 0) &&
        tensor_capture_selected_scalar_tail_is(nodes, interp, 2, 1)) {
        return "view-scalar-map-chain";
    }

    if (nodes.array_val.length >= 4 &&
        tensor_capture_selected_node_kind_is(nodes.array_val.items[1], interp, "tensor-source") &&
        tensor_capture_selected_node_id_is(nodes.array_val.items[1], interp, 1) &&
        tensor_capture_selected_node_kind_is(nodes.array_val.items[2], interp, "tensor-contract") &&
        tensor_capture_selected_node_id_is(nodes.array_val.items[2], interp, 2) &&
        tensor_capture_selected_tensor_map_inputs_are(nodes.array_val.items[2], interp, 0, 1) == false &&
        tensor_capture_selected_scalar_tail_is(nodes, interp, 3, 2)) {
        return "contract-scalar-map-chain";
    }

    if (nodes.array_val.length >= 4 &&
        tensor_capture_selected_node_kind_is(nodes.array_val.items[1], interp, "tensor-source") &&
        tensor_capture_selected_node_id_is(nodes.array_val.items[1], interp, 1) &&
        tensor_capture_selected_node_id_is(nodes.array_val.items[2], interp, 2) &&
        tensor_capture_selected_tensor_map_inputs_are(nodes.array_val.items[2], interp, 0, 1) &&
        tensor_capture_selected_scalar_tail_is(nodes, interp, 3, 2)) {
        return "tensor-scalar-map-chain";
    }

    if (tensor_capture_selected_scalar_tail_is(nodes, interp, 1, 0)) return "scalar-map-chain";
    return "";
}

fn Value* tensor_capture_selected_region_candidate(Value* nodes, Value* batch, Interp* interp, char[] family) {
    Value* candidate = nn_make_dict(interp, 18);
    if (candidate == null || candidate.tag == ERROR) return candidate;
    if (Value* err = nn_dict_set(candidate, interp, "id", make_int(interp, 0))) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "kind", "selected-region")) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "family", family)) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "lowering", "command-buffer-candidate")) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "runtime-support", "native-selected-region-executor")) return err;
    if (Value* err = nn_dict_set_symbol(candidate, interp, "memory-plan", "metadata-only")) return err;
    if (Value* err = nn_dict_set(candidate, interp, "command-batch", make_int(interp, 0))) return err;
    if (Value* err = nn_dict_set(candidate, interp, "nodes", nn_dict_get(batch, interp, "nodes"))) return err;
    if (Value* err = nn_dict_set(candidate, interp, "depends-on", nn_dict_get(batch, interp, "depends-on"))) return err;
    if (Value* err = nn_dict_set(candidate, interp, "dispatches", nn_dict_get(batch, interp, "dispatches"))) return err;
    if (Value* err = nn_dict_set(candidate, interp, "launch-count", nn_dict_get(batch, interp, "launch-count"))) return err;
    if (Value* err = nn_dict_set(candidate, interp, "dispatch-count", nn_dict_get(batch, interp, "dispatch-count"))) return err;
    if (Value* err = nn_dict_set(candidate, interp, "barrier-count", nn_dict_get(batch, interp, "barrier-count"))) return err;
    if (Value* err = nn_dict_set(candidate, interp, "node-count", tensor_capture_usz_value(interp, nodes.array_val.length, "tensor/capture: selected-region node-count is too large"))) return err;
    return candidate;
}

fn Value* tensor_capture_selected_region_plan(Value* nodes, Value* command_batches, Interp* interp) {
    char[] family = tensor_capture_selected_region_family(nodes, interp);
    bool eligible = family.len > 0 && command_batches != null && command_batches.tag == ARRAY &&
        command_batches.array_val != null && command_batches.array_val.length == 1;
    Value* candidates = make_array_checked(interp, eligible ? 1 : 0, "tensor/capture: selected-region candidates out of memory");
    if (candidates == null || candidates.tag == ERROR) return candidates;
    if (candidates.array_val == null) return nn_oom(interp);
    if (eligible) {
        Value* batch = command_batches.array_val.items[0];
        if (batch == null || batch.tag != HASHMAP) return tensor_runtime_raise(interp, "tensor/capture: selected-region command batch is invalid");
        Value* candidate = tensor_capture_selected_region_candidate(nodes, batch, interp, family);
        if (candidate == null || candidate.tag == ERROR) return candidate;
        if (Value* err = tensor_capture_array_push(candidates, candidate, interp)) return err;
    }

    Value* plan = nn_make_dict(interp, 14);
    if (plan == null || plan.tag == ERROR) return plan;
    if (Value* err = nn_dict_set_symbol(plan, interp, "kind", "tensor-selected-region-plan")) return err;
    if (Value* err = nn_dict_set(plan, interp, "version", make_int(interp, 1))) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "policy", "selected-region-command-plan")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "planner", "capture-selected-region-v1")) return err;
    if (Value* err = nn_dict_set(plan, interp, "candidate-count", tensor_capture_usz_value(interp, eligible ? 1 : 0, "tensor/capture: selected-region candidate-count is too large"))) return err;
    if (Value* err = nn_dict_set(plan, interp, "candidates", candidates)) return err;
    if (Value* err = nn_dict_set(plan, interp, "executes", make_bool_value(interp, false))) return err;
    if (Value* err = nn_dict_set(plan, interp, "consumes-command-batches", make_bool_value(interp, eligible))) return err;
    if (Value* err = nn_dict_set(plan, interp, "consumes-memory-plan", make_bool_value(interp, false))) return err;
    return plan;
}
