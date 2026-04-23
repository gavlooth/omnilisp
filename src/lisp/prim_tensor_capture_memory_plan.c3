module lisp;

fn SymbolId tensor_capture_memory_allocation_symbol(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return interp.symbols.intern("unknown");
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return interp.symbols.intern("external");
        case TENSOR_PAYLOAD_MAP:
        case TENSOR_PAYLOAD_CONTRACT:
            return interp.symbols.intern("transient");
        case TENSOR_PAYLOAD_VIEW:
            return interp.symbols.intern("alias");
    }
}

fn SymbolId tensor_capture_memory_owner_symbol(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return interp.symbols.intern("invalid");
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return interp.symbols.intern("self");
        case TENSOR_PAYLOAD_MAP:
        case TENSOR_PAYLOAD_CONTRACT:
            return interp.symbols.intern("planned-transient");
        case TENSOR_PAYLOAD_VIEW:
            return interp.symbols.intern("view-source");
    }
}

fn SymbolId tensor_capture_memory_write_policy_symbol(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return interp.symbols.intern("invalid");
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return interp.symbols.intern("read-only");
        case TENSOR_PAYLOAD_MAP:
        case TENSOR_PAYLOAD_CONTRACT:
            return interp.symbols.intern("planned-write-once");
        case TENSOR_PAYLOAD_VIEW:
            return interp.symbols.intern("read-only-view");
    }
}

fn Value* tensor_capture_memory_fields(Value* node, TensorVal* tensor, Interp* interp) {
    if (Value* err = nn_dict_set(node, interp, "element-count", tensor_capture_usz_value(interp, tensor.element_count, "tensor/capture: tensor element-count is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "byte-length", tensor_capture_usz_value(interp, tensor.byte_len, "tensor/capture: tensor byte-length is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "storage-offset", tensor_capture_usz_value(interp, tensor.storage_offset, "tensor/capture: storage offset is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "storage-elements", tensor_capture_usz_value(interp, tensor.storage_element_count, "tensor/capture: tensor storage element count is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "storage-bytes", tensor_capture_usz_value(interp, tensor.storage_byte_len, "tensor/capture: tensor storage byte length is too large"))) return err;
    if (Value* err = nn_dict_set(node, interp, "allocation", make_symbol(interp, tensor_capture_memory_allocation_symbol(tensor, interp)))) return err;
    if (Value* err = nn_dict_set(node, interp, "owner", make_symbol(interp, tensor_capture_memory_owner_symbol(tensor, interp)))) return err;
    if (Value* err = nn_dict_set(node, interp, "write-policy", make_symbol(interp, tensor_capture_memory_write_policy_symbol(tensor, interp)))) return err;
    return null;
}

fn Value* tensor_capture_memory_plan_bytes(Value* nodes, Interp* interp, usz* external_bytes, usz* transient_bytes) {
    if (nodes == null || nodes.tag != ARRAY || nodes.array_val == null || external_bytes == null || transient_bytes == null) return nn_oom(interp);
    *external_bytes = 0;
    *transient_bytes = 0;
    for (usz i = 0; i < nodes.array_val.length; i++) {
        Value* node = nodes.array_val.items[i];
        if (node == null || node.tag != HASHMAP) return nn_oom(interp);
        Value* allocation = nn_dict_get(node, interp, "allocation");
        Value* storage_bytes = nn_dict_get(node, interp, "storage-bytes");
        Value* byte_length = nn_dict_get(node, interp, "byte-length");
        if (allocation == null || allocation.tag != SYMBOL) continue;
        if (nn_symbol_is(allocation, interp, "external")) {
            if (storage_bytes != null && storage_bytes.tag == INT && storage_bytes.int_val >= 0) {
                usz bytes = (usz)storage_bytes.int_val;
                if (*external_bytes > (usz)long.max - bytes) return tensor_runtime_raise(interp, "tensor/capture: external-bytes is too large");
                *external_bytes += bytes;
            }
        } else if (nn_symbol_is(allocation, interp, "transient")) {
            if (byte_length != null && byte_length.tag == INT && byte_length.int_val >= 0) {
                usz bytes = (usz)byte_length.int_val;
                if (*transient_bytes > (usz)long.max - bytes) return tensor_runtime_raise(interp, "tensor/capture: transient-bytes is too large");
                *transient_bytes += bytes;
            }
        }
    }
    return null;
}

fn Value* tensor_capture_memory_reuse_groups(Value* nodes, Interp* interp, int* out_group_count) {
    if (out_group_count != null) *out_group_count = 0;
    if (nodes == null || nodes.tag != ARRAY || nodes.array_val == null) return nn_oom(interp);

    int transient_count = 0;
    usz max_bytes = 0;
    for (usz i = 0; i < nodes.array_val.length; i++) {
        Value* node = nodes.array_val.items[i];
        if (node == null || node.tag != HASHMAP) return nn_oom(interp);
        Value* allocation = nn_dict_get(node, interp, "allocation");
        Value* byte_length = nn_dict_get(node, interp, "byte-length");
        if (allocation == null || allocation.tag != SYMBOL || !nn_symbol_is(allocation, interp, "transient")) continue;
        if (byte_length == null || byte_length.tag != INT || byte_length.int_val < 0) continue;
        transient_count++;
        if ((usz)byte_length.int_val > max_bytes) max_bytes = (usz)byte_length.int_val;
    }

    Value* groups = make_array_checked(interp, transient_count > 1 ? 1 : 0, "tensor/capture: out of memory");
    if (groups == null || groups.tag == ERROR) return groups;
    if (groups.array_val == null) return nn_oom(interp);
    if (transient_count <= 1) return groups;

    Value* node_ids = make_array_checked(interp, (usz)transient_count, "tensor/capture: out of memory");
    if (node_ids == null || node_ids.tag == ERROR) return node_ids;
    if (node_ids.array_val == null) return nn_oom(interp);
    for (usz i = 0; i < nodes.array_val.length; i++) {
        Value* node = nodes.array_val.items[i];
        Value* allocation = nn_dict_get(node, interp, "allocation");
        Value* id = nn_dict_get(node, interp, "id");
        if (allocation == null || allocation.tag != SYMBOL || !nn_symbol_is(allocation, interp, "transient")) continue;
        if (id == null || id.tag != INT) return tensor_runtime_raise(interp, "tensor/capture: reuse candidate node id is missing");
        if (Value* err = tensor_capture_array_push(node_ids, id, interp)) return err;
    }

    Value* group = nn_make_dict(interp, 8);
    if (group == null || group.tag == ERROR) return group;
    if (Value* err = nn_dict_set_symbol(group, interp, "kind", "transient-reuse-group")) return err;
    if (Value* err = nn_dict_set_symbol(group, interp, "policy", "single-slot-sequential")) return err;
    if (Value* err = nn_dict_set(group, interp, "nodes", node_ids)) return err;
    if (Value* err = nn_dict_set(group, interp, "candidate-count", tensor_capture_usz_value(interp, (usz)transient_count, "tensor/capture: reuse candidate-count is too large"))) return err;
    if (Value* err = nn_dict_set(group, interp, "slot-bytes", tensor_capture_usz_value(interp, max_bytes, "tensor/capture: reuse slot-bytes is too large"))) return err;
    if (Value* err = nn_dict_set(group, interp, "allocates", make_bool_value(interp, false))) return err;
    if (Value* err = nn_dict_set(group, interp, "retains-handles", make_bool_value(interp, false))) return err;
    if (Value* err = tensor_capture_array_push(groups, group, interp)) return err;
    if (out_group_count != null) *out_group_count = 1;
    return groups;
}

fn Value* tensor_capture_memory_plan(Value* nodes, usz external_bytes, usz transient_bytes, Interp* interp) {
    int reuse_group_count = 0;
    Value* reuse_groups = tensor_capture_memory_reuse_groups(nodes, interp, &reuse_group_count);
    if (reuse_groups == null || reuse_groups.tag == ERROR) return reuse_groups;

    Value* plan = nn_make_dict(interp, 16);
    if (plan == null || plan.tag == ERROR) return plan;
    if (Value* err = nn_dict_set_symbol(plan, interp, "kind", "tensor-memory-plan")) return err;
    if (Value* err = nn_dict_set(plan, interp, "version", make_int(interp, 2))) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "policy", "metadata-only")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "reuse-policy", reuse_group_count > 0 ? "single-slot-sequential" : "none")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "runtime-ownership", "tensor-payload-owned")) return err;
    if (Value* err = nn_dict_set(plan, interp, "allocates", make_bool_value(interp, false))) return err;
    if (Value* err = nn_dict_set(plan, interp, "retains-handles", make_bool_value(interp, false))) return err;
    if (Value* err = nn_dict_set(plan, interp, "executes-reuse", make_bool_value(interp, false))) return err;
    if (Value* err = nn_dict_set(plan, interp, "node-memory", nodes)) return err;
    if (Value* err = nn_dict_set(plan, interp, "reuse-group-count", tensor_capture_usz_value(interp, (usz)reuse_group_count, "tensor/capture: reuse-group-count is too large"))) return err;
    if (Value* err = nn_dict_set(plan, interp, "reuse-groups", reuse_groups)) return err;
    if (Value* err = nn_dict_set(plan, interp, "external-bytes", tensor_capture_usz_value(interp, external_bytes, "tensor/capture: external-bytes is too large"))) return err;
    if (Value* err = nn_dict_set(plan, interp, "transient-bytes", tensor_capture_usz_value(interp, transient_bytes, "tensor/capture: transient-bytes is too large"))) return err;
    return plan;
}
