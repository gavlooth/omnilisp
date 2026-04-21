module lisp;

fn bool tensor_graph_run_int_arrays_equal(Value* left, Value* right) {
    if (left == null || right == null || left.tag != ARRAY || right.tag != ARRAY ||
        left.array_val == null || right.array_val == null ||
        left.array_val.length != right.array_val.length) {
        return false;
    }
    for (usz i = 0; i < left.array_val.length; i++) {
        Value* a = left.array_val.items[i];
        Value* b = right.array_val.items[i];
        if (a == null || b == null || a.tag != INT || b.tag != INT || a.int_val != b.int_val) return false;
    }
    return true;
}

fn bool tensor_graph_run_dispatch_nodes_equal(Value* left, Value* right, Interp* interp) {
    if (left == null || right == null || left.tag != ARRAY || right.tag != ARRAY ||
        left.array_val == null || right.array_val == null ||
        left.array_val.length != right.array_val.length) {
        return false;
    }
    for (usz i = 0; i < left.array_val.length; i++) {
        Value* a = left.array_val.items[i];
        Value* b = right.array_val.items[i];
        if (a == null || b == null || a.tag != HASHMAP || b.tag != HASHMAP) return false;
        Value* a_node = nn_dict_get(a, interp, "node");
        Value* b_node = nn_dict_get(b, interp, "node");
        if (a_node == null || b_node == null || a_node.tag != INT || b_node.tag != INT || a_node.int_val != b_node.int_val) return false;
    }
    return true;
}

fn bool tensor_graph_run_selected_region_plan_matches(Value* graph, Interp* interp, char[] family) {
    Value* plan = nn_dict_get(graph, interp, "selected-region-plan");
    if (plan == null || plan.tag != HASHMAP) return false;
    if (!nn_symbol_is(nn_dict_get(plan, interp, "kind"), interp, "tensor-selected-region-plan") ||
        !nn_symbol_is(nn_dict_get(plan, interp, "policy"), interp, "selected-region-command-plan") ||
        !nn_symbol_is(nn_dict_get(plan, interp, "planner"), interp, "capture-selected-region-v1") ||
        !tensor_graph_run_int_field_is(plan, interp, "version", 1) ||
        !tensor_graph_run_int_field_is(plan, interp, "candidate-count", 1)) {
        return false;
    }
    Value* candidates = nn_dict_get(plan, interp, "candidates");
    if (candidates == null || candidates.tag != ARRAY || candidates.array_val == null || candidates.array_val.length != 1) return false;
    Value* candidate = candidates.array_val.items[0];
    if (candidate == null || candidate.tag != HASHMAP) return false;
    if (!nn_symbol_is(nn_dict_get(candidate, interp, "kind"), interp, "selected-region") ||
        !nn_symbol_is(nn_dict_get(candidate, interp, "family"), interp, family) ||
        !nn_symbol_is(nn_dict_get(candidate, interp, "lowering"), interp, "command-buffer-candidate") ||
        !nn_symbol_is(nn_dict_get(candidate, interp, "runtime-support"), interp, "native-selected-region-executor") ||
        !tensor_graph_run_int_field_is(candidate, interp, "command-batch", 0)) {
        return false;
    }

    Value* batches = nn_dict_get(graph, interp, "command-batches");
    if (batches == null || batches.tag != ARRAY || batches.array_val == null || batches.array_val.length != 1) return false;
    Value* batch = batches.array_val.items[0];
    if (batch == null || batch.tag != HASHMAP) return false;
    if (!tensor_graph_run_int_arrays_equal(nn_dict_get(candidate, interp, "nodes"), nn_dict_get(batch, interp, "nodes")) ||
        !tensor_graph_run_int_arrays_equal(nn_dict_get(candidate, interp, "depends-on"), nn_dict_get(batch, interp, "depends-on")) ||
        !tensor_graph_run_dispatch_nodes_equal(nn_dict_get(candidate, interp, "dispatches"), nn_dict_get(batch, interp, "dispatches"), interp)) {
        return false;
    }
    Value* launch_count = nn_dict_get(batch, interp, "launch-count");
    Value* dispatch_count = nn_dict_get(batch, interp, "dispatch-count");
    Value* barrier_count = nn_dict_get(batch, interp, "barrier-count");
    Value* candidate_launch = nn_dict_get(candidate, interp, "launch-count");
    Value* candidate_dispatch = nn_dict_get(candidate, interp, "dispatch-count");
    Value* candidate_barrier = nn_dict_get(candidate, interp, "barrier-count");
    return launch_count != null && dispatch_count != null && barrier_count != null &&
        candidate_launch != null && candidate_dispatch != null && candidate_barrier != null &&
        launch_count.tag == INT && dispatch_count.tag == INT && barrier_count.tag == INT &&
        candidate_launch.tag == INT && candidate_dispatch.tag == INT && candidate_barrier.tag == INT &&
        launch_count.int_val == candidate_launch.int_val &&
        dispatch_count.int_val == candidate_dispatch.int_val &&
        barrier_count.int_val == candidate_barrier.int_val;
}
