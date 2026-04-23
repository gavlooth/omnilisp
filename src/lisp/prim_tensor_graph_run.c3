module lisp;

import std::core::mem;
import main;

fn bool tensor_graph_run_binary_op(Value* operation, Interp* interp, uint* out_op) {
    if (operation == null || out_op == null) return false;
    if (nn_symbol_is(operation, interp, "add")) { *out_op = 0; return true; }
    if (nn_symbol_is(operation, interp, "sub")) { *out_op = 1; return true; }
    if (nn_symbol_is(operation, interp, "mul")) { *out_op = 2; return true; }
    if (nn_symbol_is(operation, interp, "div")) { *out_op = 3; return true; }
    if (nn_symbol_is(operation, interp, "min")) { *out_op = 4; return true; }
    if (nn_symbol_is(operation, interp, "max")) { *out_op = 5; return true; }
    return false;
}

fn bool tensor_graph_run_unary_op(Value* operation, Interp* interp, uint* out_op) {
    if (operation == null || out_op == null) return false;
    if (nn_symbol_is(operation, interp, "abs")) { *out_op = 0; return true; }
    if (nn_symbol_is(operation, interp, "neg")) { *out_op = 1; return true; }
    if (nn_symbol_is(operation, interp, "sqrt")) { *out_op = 2; return true; }
    if (nn_symbol_is(operation, interp, "identity")) { *out_op = 3; return true; }
    if (nn_symbol_is(operation, interp, "zero")) { *out_op = 4; return true; }
    if (nn_symbol_is(operation, interp, "sin")) { *out_op = 5; return true; }
    if (nn_symbol_is(operation, interp, "cos")) { *out_op = 6; return true; }
    if (nn_symbol_is(operation, interp, "tan")) { *out_op = 7; return true; }
    if (nn_symbol_is(operation, interp, "asin")) { *out_op = 8; return true; }
    if (nn_symbol_is(operation, interp, "acos")) { *out_op = 9; return true; }
    if (nn_symbol_is(operation, interp, "atan")) { *out_op = 10; return true; }
    if (nn_symbol_is(operation, interp, "sinh")) { *out_op = 11; return true; }
    if (nn_symbol_is(operation, interp, "cosh")) { *out_op = 12; return true; }
    if (nn_symbol_is(operation, interp, "tanh")) { *out_op = 13; return true; }
    if (nn_symbol_is(operation, interp, "exp")) { *out_op = 14; return true; }
    if (nn_symbol_is(operation, interp, "log")) { *out_op = 15; return true; }
    if (nn_symbol_is(operation, interp, "log10")) { *out_op = 16; return true; }
    if (nn_symbol_is(operation, interp, "normal-cdf")) { *out_op = 19; return true; }
    if (nn_symbol_is(operation, interp, "normal-quantile")) { *out_op = 20; return true; }
    return false;
}

fn Value* tensor_graph_run_source_node(Value* node, Value*[] results, Interp* interp) {
    Value* id_value = nn_dict_get(node, interp, "id");
    Value* source = nn_dict_get(node, interp, "source-value");
    if (id_value == null || id_value.tag != INT || id_value.int_val < 0 || (usz)id_value.int_val >= results.len) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: source node id is invalid", null);
    }
    if (!is_tensor(source) || source.tensor_val == null || source.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
        source.tensor_val.device != TENSOR_DEVICE_VULKAN || !tensor_has_valid_device_storage(source.tensor_val, TENSOR_DEVICE_VULKAN)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: source node does not contain executable Vulkan Float32 tensor storage", null);
    }
    results[(usz)id_value.int_val] = source;
    return null;
}

fn bool tensor_graph_run_vulkan_readable_float32(TensorVal* tensor) {
    if (tensor == null || tensor.dtype != TENSOR_DTYPE_FLOAT32 || tensor.device != TENSOR_DEVICE_VULKAN) return false;
    if (tensor.kind == TENSOR_PAYLOAD_CONCRETE) return tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_VULKAN);
    if (tensor.kind != TENSOR_PAYLOAD_VIEW || !tensor.is_view) return false;
    if (tensor.rank > 0 && (tensor.shape == null || tensor.strides == null)) return false;
    if (tensor.byte_len > 0 && tensor.device_handle == null) return false;
    if (tensor.storage_offset != 0 || tensor.storage_element_count < tensor.element_count) return false;
    if (tensor.storage_byte_len < tensor.byte_len) return false;
    return true;
}

fn Value* tensor_graph_run_map_node(Value* node, Value*[] results, Interp* interp) {
    Value* id_value = nn_dict_get(node, interp, "id");
    Value* input_ids = nn_dict_get(node, interp, "inputs");
    Value* arity_value = nn_dict_get(node, interp, "arity");
    if (id_value == null || id_value.tag != INT || id_value.int_val < 0 || (usz)id_value.int_val >= results.len ||
        input_ids == null || input_ids.tag != ARRAY || input_ids.array_val == null ||
        arity_value == null || arity_value.tag != INT) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: map node metadata is invalid", null);
    }
    if (input_ids.array_val.length == 0 || input_ids.array_val.length > 2) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: map node input count is invalid", null);
    }

    Value* first_id = input_ids.array_val.items[0];
    if (first_id == null || first_id.tag != INT || first_id.int_val < 0 || (usz)first_id.int_val >= results.len) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: map node input id is invalid", null);
    }
    Value* left_value = results[(usz)first_id.int_val];
    if (!is_tensor(left_value) || left_value.tensor_val == null || left_value.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
        left_value.tensor_val.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/run: map inputs must be Vulkan Float32 tensors", null);
    }

    uint op = 0;
    if (arity_value.int_val == 1) {
        if (!tensor_graph_run_unary_op(nn_dict_get(node, interp, "operation"), interp, &op)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/run: unsupported unary map operation", null);
        }
        usz[1] scalar_shape = { 0 };
        usz[] shape_slice = left_value.tensor_val.rank == 0 ? scalar_shape[0:0] : left_value.tensor_val.shape[0:left_value.tensor_val.rank];
        Value* out = tensor_vulkan_map_unary_value(
            left_value.tensor_val,
            op,
            shape_slice,
            interp,
            "tensor/run: failed to allocate Vulkan tensor metadata",
            "tensor/run: failed to allocate Vulkan tensor value",
            "tensor/run: unsupported Vulkan Float32 unary map graph",
            "tensor/run: Vulkan tensor backing storage is missing",
            "tensor/run: Vulkan unary map graph failed"
        );
        if (out == null || out.tag == ERROR) return out;
        results[(usz)id_value.int_val] = out;
        return null;
    }

    if (arity_value.int_val != 2 || !tensor_graph_run_binary_op(nn_dict_get(node, interp, "operation"), interp, &op)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/run: unsupported binary map operation", null);
    }

    TensorVal* left_tensor = left_value.tensor_val;
    TensorVal* right_tensor = null;
    float scalar = 0.0f;
    uint mode = 2;
    Value* right_scalar = nn_dict_get(node, interp, "right-scalar");
    Value* left_scalar = nn_dict_get(node, interp, "left-scalar");
    if (right_scalar != null && left_scalar != null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: binary map node cannot contain both left-scalar and right-scalar", null);
    }
    if (right_scalar != null) {
        if (input_ids.array_val.length != 1) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: right-scalar binary map node must contain exactly one tensor input", null);
        }
        if (!tensor_value_to_float32(right_scalar, &scalar)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/run: right scalar is not representable as Float32", null);
        }
        mode = 0;
    } else if (left_scalar != null) {
        if (input_ids.array_val.length != 1) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: left-scalar binary map node must contain exactly one tensor input", null);
        }
        if (!tensor_value_to_float32(left_scalar, &scalar)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/run: left scalar is not representable as Float32", null);
        }
        right_tensor = left_tensor;
        left_tensor = null;
        mode = 1;
    } else {
        if (input_ids.array_val.length != 2) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: binary map node is missing tensor input", null);
        }
        Value* second_id = input_ids.array_val.items[1];
        if (second_id == null || second_id.tag != INT || second_id.int_val < 0 || (usz)second_id.int_val >= results.len) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: map node second input id is invalid", null);
        }
        Value* right_value = results[(usz)second_id.int_val];
        if (!is_tensor(right_value) || right_value.tensor_val == null || right_value.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
            right_value.tensor_val.device != TENSOR_DEVICE_VULKAN) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/run: map inputs must be Vulkan Float32 tensors", null);
        }
        right_tensor = right_value.tensor_val;
        mode = 2;
    }

    TensorVal* shape_tensor = left_tensor != null ? left_tensor : right_tensor;
    if (shape_tensor == null || !tensor_graph_run_vulkan_readable_float32(shape_tensor) ||
        (left_tensor != null && !tensor_graph_run_vulkan_readable_float32(left_tensor)) ||
        (right_tensor != null && !tensor_graph_run_vulkan_readable_float32(right_tensor))) {
        return tensor_runtime_raise(interp, "tensor/run: Vulkan tensor backing storage is missing");
    }
    void* out_device = null;
    int status = omni_tensor_backend_vulkan_map_f32(
        left_tensor != null ? left_tensor.device_handle : null,
        right_tensor != null ? right_tensor.device_handle : null,
        shape_tensor.byte_len,
        shape_tensor.element_count,
        scalar,
        mode,
        op,
        shape_tensor.rank,
        left_tensor != null ? left_tensor.rank : 0,
        right_tensor != null ? right_tensor.rank : 0,
        shape_tensor.shape,
        shape_tensor.strides,
        left_tensor != null ? left_tensor.shape : null,
        left_tensor != null ? left_tensor.strides : null,
        right_tensor != null ? right_tensor.shape : null,
        right_tensor != null ? right_tensor.strides : null,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "tensor/run: Vulkan binary map graph failed");
    }

    Value* out = ml_optimizer_vulkan_tensor_value(shape_tensor, out_device, interp, "tensor/run: failed to allocate Vulkan output tensor metadata", "tensor/run: failed to allocate Vulkan output tensor value");
    if (out == null || out.tag == ERROR) return out;
    results[(usz)id_value.int_val] = out;
    return null;
}

fn bool tensor_graph_run_node_id_is(Value* node, Interp* interp, usz expected) {
    Value* id_value = nn_dict_get(node, interp, "id");
    return id_value != null && id_value.tag == INT && id_value.int_val >= 0 && (usz)id_value.int_val == expected;
}

fn bool tensor_graph_run_single_input_is(Value* node, Interp* interp, usz expected) {
    Value* input_ids = nn_dict_get(node, interp, "inputs");
    if (input_ids == null || input_ids.tag != ARRAY || input_ids.array_val == null || input_ids.array_val.length != 1) return false;
    Value* input_id = input_ids.array_val.items[0];
    return input_id != null && input_id.tag == INT && input_id.int_val >= 0 && (usz)input_id.int_val == expected;
}

fn bool tensor_graph_run_scalar_map_parts(Value* node, Interp* interp, uint* out_mode, float* out_scalar, uint* out_op) {
    if (node == null || node.tag != HASHMAP || out_mode == null || out_scalar == null || out_op == null) return false;
    if (!nn_symbol_is(nn_dict_get(node, interp, "kind"), interp, "tensor-map")) return false;
    Value* arity = nn_dict_get(node, interp, "arity");
    if (arity == null || arity.tag != INT || arity.int_val != 2) return false;
    if (!tensor_graph_run_binary_op(nn_dict_get(node, interp, "operation"), interp, out_op)) return false;
    Value* right_scalar = nn_dict_get(node, interp, "right-scalar");
    Value* left_scalar = nn_dict_get(node, interp, "left-scalar");
    if (right_scalar != null && left_scalar == null) {
        *out_mode = 0;
        return tensor_value_to_float32(right_scalar, out_scalar);
    }
    if (left_scalar != null && right_scalar == null) {
        *out_mode = 1;
        return tensor_value_to_float32(left_scalar, out_scalar);
    }
    return false;
}

fn bool tensor_graph_run_int_field_is(Value* node, Interp* interp, char[] key, usz expected) {
    Value* value = nn_dict_get(node, interp, key);
    return value != null && value.tag == INT && value.int_val >= 0 && (usz)value.int_val == expected;
}

fn bool tensor_graph_run_command_batch_matches_scalar_chain(Value* graph, Value*[] nodes, uint* ops, Interp* interp) {
    if (nodes.len < 3 || ops == null) return false;
    Value* batches = nn_dict_get(graph, interp, "command-batches");
    if (batches == null || batches.tag != ARRAY || batches.array_val == null || batches.array_val.length != 1) return false;
    Value* batch = batches.array_val.items[0];
    if (batch == null || batch.tag != HASHMAP) return false;
    if (!nn_symbol_is(nn_dict_get(batch, interp, "execution"), interp, "command-buffer-candidate")) return false;
    if (!nn_symbol_is(nn_dict_get(batch, interp, "barrier-policy"), interp, "shader-write-to-shader-read-between-dispatches")) return false;

    usz op_count = nodes.len - 1;
    if (!tensor_graph_run_int_field_is(batch, interp, "launch-count", op_count) ||
        !tensor_graph_run_int_field_is(batch, interp, "dispatch-count", op_count) ||
        !tensor_graph_run_int_field_is(batch, interp, "barrier-count", op_count - 1)) {
        return false;
    }

    Value* batch_nodes = nn_dict_get(batch, interp, "nodes");
    Value* dispatches = nn_dict_get(batch, interp, "dispatches");
    Value* depends_on = nn_dict_get(batch, interp, "depends-on");
    if (batch_nodes == null || batch_nodes.tag != ARRAY || batch_nodes.array_val == null || batch_nodes.array_val.length != op_count ||
        dispatches == null || dispatches.tag != ARRAY || dispatches.array_val == null || dispatches.array_val.length != op_count ||
        depends_on == null || depends_on.tag != ARRAY || depends_on.array_val == null || depends_on.array_val.length != 1) {
        return false;
    }
    Value* dep = depends_on.array_val.items[0];
    if (dep == null || dep.tag != INT || dep.int_val != 0) return false;

    for (usz i = 0; i < op_count; i++) {
        Value* batch_node = batch_nodes.array_val.items[i];
        if (batch_node == null || batch_node.tag != INT || batch_node.int_val < 0 || (usz)batch_node.int_val != i + 1) return false;
        Value* dispatch = dispatches.array_val.items[i];
        if (dispatch == null || dispatch.tag != HASHMAP) return false;
        uint dispatch_op = 0;
        if (!tensor_graph_run_int_field_is(dispatch, interp, "node", i + 1) ||
            !tensor_graph_run_binary_op(nn_dict_get(dispatch, interp, "operation"), interp, &dispatch_op) ||
            dispatch_op != ops[i]) {
            return false;
        }
    }
    return true;
}

fn bool tensor_graph_run_tensors_same_dense_shape(TensorVal* left, TensorVal* right) {
    if (left == null || right == null || left.rank != right.rank ||
        left.element_count != right.element_count || left.byte_len != right.byte_len ||
        !tensor_dense_row_major(left) || !tensor_dense_row_major(right)) {
        return false;
    }
    for (usz i = 0; i < left.rank; i++) {
        if (left.shape[i] != right.shape[i] || left.strides[i] != right.strides[i]) return false;
    }
    return true;
}

fn bool tensor_graph_run_binary_tensor_map_op(Value* node, Interp* interp, uint* out_op) {
    if (node == null || node.tag != HASHMAP || out_op == null) return false;
    if (!nn_symbol_is(nn_dict_get(node, interp, "kind"), interp, "tensor-map")) return false;
    Value* arity = nn_dict_get(node, interp, "arity");
    Value* input_ids = nn_dict_get(node, interp, "inputs");
    if (arity == null || arity.tag != INT || arity.int_val != 2 ||
        input_ids == null || input_ids.tag != ARRAY || input_ids.array_val == null || input_ids.array_val.length != 2) {
        return false;
    }
    if (nn_dict_get(node, interp, "left-scalar") != null || nn_dict_get(node, interp, "right-scalar") != null) return false;
    return tensor_graph_run_binary_op(nn_dict_get(node, interp, "operation"), interp, out_op);
}

fn bool tensor_graph_run_command_batch_matches_tensor_scalar_chain(Value* graph, Value*[] nodes, uint* ops, Interp* interp) {
    if (nodes.len < 4 || ops == null) return false;
    Value* batches = nn_dict_get(graph, interp, "command-batches");
    if (batches == null || batches.tag != ARRAY || batches.array_val == null || batches.array_val.length != 1) return false;
    Value* batch = batches.array_val.items[0];
    if (batch == null || batch.tag != HASHMAP) return false;
    if (!nn_symbol_is(nn_dict_get(batch, interp, "execution"), interp, "command-buffer-candidate")) return false;
    if (!nn_symbol_is(nn_dict_get(batch, interp, "barrier-policy"), interp, "shader-write-to-shader-read-between-dispatches")) return false;

    usz op_count = nodes.len - 2;
    if (!tensor_graph_run_int_field_is(batch, interp, "launch-count", op_count) ||
        !tensor_graph_run_int_field_is(batch, interp, "dispatch-count", op_count) ||
        !tensor_graph_run_int_field_is(batch, interp, "barrier-count", op_count - 1)) {
        return false;
    }

    Value* batch_nodes = nn_dict_get(batch, interp, "nodes");
    Value* dispatches = nn_dict_get(batch, interp, "dispatches");
    Value* depends_on = nn_dict_get(batch, interp, "depends-on");
    if (batch_nodes == null || batch_nodes.tag != ARRAY || batch_nodes.array_val == null || batch_nodes.array_val.length != op_count ||
        dispatches == null || dispatches.tag != ARRAY || dispatches.array_val == null || dispatches.array_val.length != op_count ||
        depends_on == null || depends_on.tag != ARRAY || depends_on.array_val == null || depends_on.array_val.length != 2) {
        return false;
    }
    Value* left_dep = depends_on.array_val.items[0];
    Value* right_dep = depends_on.array_val.items[1];
    if (left_dep == null || right_dep == null || left_dep.tag != INT || right_dep.tag != INT || left_dep.int_val != 0 || right_dep.int_val != 1) return false;

    for (usz i = 0; i < op_count; i++) {
        usz expected_node = i + 2;
        Value* batch_node = batch_nodes.array_val.items[i];
        if (batch_node == null || batch_node.tag != INT || batch_node.int_val < 0 || (usz)batch_node.int_val != expected_node) return false;
        Value* dispatch = dispatches.array_val.items[i];
        if (dispatch == null || dispatch.tag != HASHMAP) return false;
        uint dispatch_op = 0;
        if (!tensor_graph_run_int_field_is(dispatch, interp, "node", expected_node) ||
            !tensor_graph_run_binary_op(nn_dict_get(dispatch, interp, "operation"), interp, &dispatch_op) ||
            dispatch_op != ops[i]) {
            return false;
        }
    }
    return true;
}

fn Value* tensor_graph_run_usz_array(Value* value, Interp* interp, char[] label, usz** out_items, usz* out_count) {
    if (out_items == null || out_count == null) return nn_oom(interp);
    *out_items = null;
    *out_count = 0;
    if (value == null || value.tag != ARRAY || value.array_val == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", label, null);
    }
    *out_count = value.array_val.length;
    if (value.array_val.length == 0) return null;
    usz* items = mem::new_array(usz, value.array_val.length);
    for (usz i = 0; i < value.array_val.length; i++) {
        Value* item = value.array_val.items[i];
        if (item == null || item.tag != INT || item.int_val < 0) {
            mem::free(items);
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", label, null);
        }
        items[i] = (usz)item.int_val;
    }
    *out_items = items;
    return null;
}

fn bool tensor_graph_run_axes_valid(usz* axes, usz axis_count, usz rank) {
    if (axis_count > 0 && axes == null) return false;
    for (usz i = 0; i < axis_count; i++) {
        if (axes[i] >= rank) return false;
        for (usz j = i + 1; j < axis_count; j++) {
            if (axes[i] == axes[j]) return false;
        }
    }
    return true;
}

fn Value* tensor_graph_run_tensor_result(TensorVal* tensor, Interp* interp, char[] value_label) {
    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, value_label);
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_graph_run_contract_node(Value* node, Value*[] results, Interp* interp) {
    Value* id_value = nn_dict_get(node, interp, "id");
    Value* input_ids = nn_dict_get(node, interp, "inputs");
    if (id_value == null || id_value.tag != INT || id_value.int_val < 0 || (usz)id_value.int_val >= results.len ||
        input_ids == null || input_ids.tag != ARRAY || input_ids.array_val == null || input_ids.array_val.length != 2) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: contract node metadata is invalid", null);
    }
    Value* left_id = input_ids.array_val.items[0];
    Value* right_id = input_ids.array_val.items[1];
    if (left_id == null || left_id.tag != INT || left_id.int_val < 0 || (usz)left_id.int_val >= results.len ||
        right_id == null || right_id.tag != INT || right_id.int_val < 0 || (usz)right_id.int_val >= results.len) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: contract node input id is invalid", null);
    }

    Value* left_value = results[(usz)left_id.int_val];
    Value* right_value = results[(usz)right_id.int_val];
    if (!is_tensor(left_value) || !is_tensor(right_value) ||
        left_value.tensor_val == null || right_value.tensor_val == null ||
        left_value.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
        right_value.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
        left_value.tensor_val.device != TENSOR_DEVICE_VULKAN ||
        right_value.tensor_val.device != TENSOR_DEVICE_VULKAN ||
        !tensor_has_valid_device_storage(left_value.tensor_val, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(right_value.tensor_val, TENSOR_DEVICE_VULKAN)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/run: contract inputs must be Vulkan Float32 tensors", null);
    }

    usz* left_axes = null;
    usz left_axis_count = 0;
    if (Value* err = tensor_graph_run_usz_array(nn_dict_get(node, interp, "left-axes"), interp, "tensor/run: contract left axes are invalid", &left_axes, &left_axis_count)) return err;
    defer if (left_axes != null) mem::free(left_axes);
    usz* right_axes = null;
    usz right_axis_count = 0;
    if (Value* err = tensor_graph_run_usz_array(nn_dict_get(node, interp, "right-axes"), interp, "tensor/run: contract right axes are invalid", &right_axes, &right_axis_count)) return err;
    defer if (right_axes != null) mem::free(right_axes);
    if (left_axis_count != right_axis_count) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: contract axis counts do not match", null);
    }
    if (!tensor_graph_run_axes_valid(left_axes, left_axis_count, left_value.tensor_val.rank) ||
        !tensor_graph_run_axes_valid(right_axes, right_axis_count, right_value.tensor_val.rank)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: contract axes are out of range or duplicated", null);
    }

    usz* shape = null;
    usz rank = 0;
    if (Value* err = tensor_graph_run_usz_array(nn_dict_get(node, interp, "shape"), interp, "tensor/run: contract output shape is invalid", &shape, &rank)) return err;
    defer if (shape != null) mem::free(shape);
    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = rank == 0 ? scalar_shape[0:0] : shape[0:rank];
    TensorVal* tensor = tensor_alloc_payload_base_checked(TENSOR_DTYPE_FLOAT32, shape_slice, false, false, "tensor/run: failed to allocate Vulkan contract tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "tensor/run: failed to allocate Vulkan contract tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_contract_f32(
        left_value.tensor_val.device_handle,
        right_value.tensor_val.device_handle,
        left_value.tensor_val.rank,
        right_value.tensor_val.rank,
        left_axis_count,
        left_axes,
        right_axes,
        tensor.element_count,
        tensor.rank,
        left_value.tensor_val.shape,
        left_value.tensor_val.strides,
        right_value.tensor_val.shape,
        right_value.tensor_val.strides,
        tensor.shape,
        tensor.strides,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "tensor/run: Vulkan contract graph failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* out = tensor_graph_run_tensor_result(tensor, interp, "tensor/run: failed to allocate Vulkan contract tensor value");
    if (out == null || out.tag == ERROR) return out;
    results[(usz)id_value.int_val] = out;
    return null;
}

fn Value* tensor_graph_run_view_node(Value* node, Value*[] results, Interp* interp) {
    Value* id_value = nn_dict_get(node, interp, "id");
    Value* input_ids = nn_dict_get(node, interp, "inputs");
    if (id_value == null || id_value.tag != INT || id_value.int_val < 0 || (usz)id_value.int_val >= results.len ||
        input_ids == null || input_ids.tag != ARRAY || input_ids.array_val == null || input_ids.array_val.length != 1 ||
        !nn_symbol_is(nn_dict_get(node, interp, "operation"), interp, "transpose-view")) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: view node metadata is invalid", null);
    }
    Value* source_id = input_ids.array_val.items[0];
    if (source_id == null || source_id.tag != INT || source_id.int_val < 0 || (usz)source_id.int_val >= results.len) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: view node input id is invalid", null);
    }
    Value* source = results[(usz)source_id.int_val];
    if (!is_tensor(source) || source.tensor_val == null || source.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
        source.tensor_val.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/run: view input must be a Vulkan Float32 tensor", null);
    }
    Value* out = matrix_transpose_view_value(source, source.tensor_val, interp, "tensor/run: failed to replay transpose view");
    if (out == null || out.tag == ERROR) return out;
    results[(usz)id_value.int_val] = out;
    return null;
}

<*
@require interp != null
*>
fn Value* prim_tensor_run(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "tensor/run", "1 argument: tensor-graph", args.len);
    Value* graph = args[0];
    if (graph.tag != HASHMAP || graph.hashmap_val == null) {
        return primitive_arg_type_raise(interp, "tensor/run", 0, "tensor/capture graph Dictionary", graph);
    }
    if (!nn_symbol_is(nn_dict_get(graph, interp, "kind"), interp, "tensor-graph")) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: expected tensor-graph capture dictionary", null);
    }
    if (!nn_symbol_is(nn_dict_get(graph, interp, "backend"), interp, "vulkan") ||
        !nn_symbol_is(nn_dict_get(graph, interp, "dtype"), interp, "Float32")) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/run: currently supports captured Vulkan Float32 tensor graphs", null);
    }

    Value* schedule = nn_dict_get(graph, interp, "schedule");
    Value* nodes = nn_dict_get(graph, interp, "nodes");
    Value* command_batches = nn_dict_get(graph, interp, "command-batches");
    if (schedule == null || schedule.tag != ARRAY || nodes == null || nodes.tag != ARRAY ||
        nodes.array_val == null || command_batches == null || command_batches.tag != ARRAY) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: capture dictionary is missing schedule, nodes, or command-batches", null);
    }

    Value* output_id = nn_dict_get(graph, interp, "output");
    if (output_id == null || output_id.tag != INT || output_id.int_val < 0 || (usz)output_id.int_val >= nodes.array_val.length) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: capture output id is invalid", null);
    }

    Value*[] results = mem::new_array(Value*, nodes.array_val.length);
    defer mem::free(results.ptr);
    for (usz i = 0; i < results.len; i++) results[i] = null;

    Value* chain_result = tensor_graph_run_try_contract_scalar_map_chain(graph, nodes.array_val.items[0:nodes.array_val.length], (usz)output_id.int_val, interp);
    if (chain_result != null) return chain_result;
    chain_result = tensor_graph_run_try_view_tensor_scalar_map_chain(graph, nodes.array_val.items[0:nodes.array_val.length], (usz)output_id.int_val, interp);
    if (chain_result != null) return chain_result;
    chain_result = tensor_graph_run_try_view_scalar_map_chain(graph, nodes.array_val.items[0:nodes.array_val.length], (usz)output_id.int_val, interp);
    if (chain_result != null) return chain_result;
    chain_result = tensor_graph_run_try_tensor_scalar_map_chain(graph, nodes.array_val.items[0:nodes.array_val.length], (usz)output_id.int_val, interp);
    if (chain_result != null) return chain_result;
    chain_result = tensor_graph_run_try_scalar_map_chain(graph, nodes.array_val.items[0:nodes.array_val.length], (usz)output_id.int_val, interp);
    if (chain_result != null) return chain_result;

    for (usz i = 0; i < nodes.array_val.length; i++) {
        Value* node = nodes.array_val.items[i];
        if (node == null || node.tag != HASHMAP) return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: graph node is invalid", null);
        if (nn_symbol_is(nn_dict_get(node, interp, "kind"), interp, "tensor-source")) {
            if (Value* err = tensor_graph_run_source_node(node, results, interp)) return err;
        } else if (nn_symbol_is(nn_dict_get(node, interp, "kind"), interp, "tensor-map")) {
            if (Value* err = tensor_graph_run_map_node(node, results, interp)) return err;
        } else if (nn_symbol_is(nn_dict_get(node, interp, "kind"), interp, "tensor-contract")) {
            if (Value* err = tensor_graph_run_contract_node(node, results, interp)) return err;
        } else if (nn_symbol_is(nn_dict_get(node, interp, "kind"), interp, "tensor-view")) {
            if (Value* err = tensor_graph_run_view_node(node, results, interp)) return err;
        } else {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "tensor/run: unsupported graph node kind", null);
        }
    }

    Value* out = results[(usz)output_id.int_val];
    if (!is_tensor(out) || out.tensor_val == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "tensor/run: graph output was not produced", null);
    }
    return out;
}
