module lisp;

import std::core::mem;

fn bool tensor_graph_run_shape_element_count(usz* shape, usz rank, usz* out_count) {
    if (out_count == null || (rank > 0 && shape == null)) return false;
    usz count = 1;
    for (usz i = 0; i < rank; i++) {
        if (!tensor_checked_mul_usz(count, shape[i], &count)) return false;
    }
    *out_count = count;
    return true;
}

fn Value* tensor_graph_run_try_scalar_map_chain(Value* graph, Value*[] nodes, usz output_index, Interp* interp) {
    if (nodes.len < 3 || output_index + 1 != nodes.len) return null;
    Value* source_node = nodes[0];
    if (source_node == null || source_node.tag != HASHMAP) return null;
    if (!nn_symbol_is(nn_dict_get(graph, interp, "family"), interp, "map-expression")) return null;
    if (!nn_symbol_is(nn_dict_get(source_node, interp, "kind"), interp, "tensor-source")) return null;
    if (!tensor_graph_run_node_id_is(source_node, interp, 0)) return null;

    usz op_count = nodes.len - 1;
    float* scalars = mem::new_array(float, op_count);
    defer mem::free(scalars);
    uint* modes = mem::new_array(uint, op_count);
    defer mem::free(modes);
    uint* ops = mem::new_array(uint, op_count);
    defer mem::free(ops);
    for (usz i = 0; i < op_count; i++) {
        Value* node = nodes[i + 1];
        if (node == null || node.tag != HASHMAP) return null;
        if (!tensor_graph_run_node_id_is(node, interp, i + 1) ||
            !tensor_graph_run_single_input_is(node, interp, i) ||
            !tensor_graph_run_scalar_map_parts(node, interp, &modes[i], &scalars[i], &ops[i])) {
            return null;
        }
    }
    if (!tensor_graph_run_selected_region_plan_matches(graph, interp, "scalar-map-chain") ||
        !tensor_graph_run_command_batch_matches_scalar_chain(graph, nodes, ops, interp)) return null;

    Value* source = nn_dict_get(source_node, interp, "source-value");
    if (!is_tensor(source) || source.tensor_val == null ||
        source.tensor_val.kind != TENSOR_PAYLOAD_CONCRETE ||
        source.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
        source.tensor_val.device != TENSOR_DEVICE_VULKAN ||
        !tensor_has_valid_device_storage(source.tensor_val, TENSOR_DEVICE_VULKAN)) {
        return null;
    }
    if (!tensor_dense_row_major(source.tensor_val)) return null;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_map_scalar_chain_f32(
        source.tensor_val.device_handle,
        source.tensor_val.byte_len,
        source.tensor_val.element_count,
        op_count,
        scalars,
        modes,
        ops,
        source.tensor_val.rank,
        source.tensor_val.shape,
        source.tensor_val.strides,
        &out_device
    );
    if (status == OMNI_TENSOR_VULKAN_UNAVAILABLE || status == OMNI_TENSOR_VULKAN_UNSUPPORTED || status == OMNI_TENSOR_VULKAN_INVALID) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return null;
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "tensor/run: Vulkan scalar map-chain reuse graph failed");
    }
    return ml_optimizer_vulkan_tensor_value(source.tensor_val, out_device, interp, "tensor/run: failed to allocate Vulkan scalar map-chain output tensor metadata", "tensor/run: failed to allocate Vulkan scalar map-chain output tensor value");
}

fn Value* tensor_graph_run_try_tensor_scalar_map_chain(Value* graph, Value*[] nodes, usz output_index, Interp* interp) {
    if (nodes.len < 4 || output_index + 1 != nodes.len) return null;
    Value* left_node = nodes[0];
    Value* right_node = nodes[1];
    if (left_node == null || right_node == null || left_node.tag != HASHMAP || right_node.tag != HASHMAP) return null;
    if (!nn_symbol_is(nn_dict_get(graph, interp, "family"), interp, "map-expression")) return null;
    if (!nn_symbol_is(nn_dict_get(left_node, interp, "kind"), interp, "tensor-source") ||
        !nn_symbol_is(nn_dict_get(right_node, interp, "kind"), interp, "tensor-source") ||
        !tensor_graph_run_node_id_is(left_node, interp, 0) ||
        !tensor_graph_run_node_id_is(right_node, interp, 1)) {
        return null;
    }

    usz op_count = nodes.len - 2;
    float* scalars = mem::new_array(float, op_count);
    defer mem::free(scalars);
    uint* modes = mem::new_array(uint, op_count);
    defer mem::free(modes);
    uint* ops = mem::new_array(uint, op_count);
    defer mem::free(ops);
    for (usz i = 0; i < op_count; i++) {
        scalars[i] = 0.0f;
        modes[i] = 2;
    }
    Value* first_map = nodes[2];
    if (!tensor_graph_run_binary_tensor_map_op(first_map, interp, &ops[0])) return null;
    Value* first_inputs = nn_dict_get(first_map, interp, "inputs");
    Value* first_left = first_inputs.array_val.items[0];
    Value* first_right = first_inputs.array_val.items[1];
    if (first_left == null || first_right == null || first_left.tag != INT || first_right.tag != INT ||
        first_left.int_val != 0 || first_right.int_val != 1 || !tensor_graph_run_node_id_is(first_map, interp, 2)) {
        return null;
    }
    for (usz i = 1; i < op_count; i++) {
        Value* node = nodes[i + 2];
        if (node == null || node.tag != HASHMAP) return null;
        if (!tensor_graph_run_node_id_is(node, interp, i + 2) ||
            !tensor_graph_run_single_input_is(node, interp, i + 1) ||
            !tensor_graph_run_scalar_map_parts(node, interp, &modes[i], &scalars[i], &ops[i])) {
            return null;
        }
    }
    if (!tensor_graph_run_selected_region_plan_matches(graph, interp, "tensor-scalar-map-chain") ||
        !tensor_graph_run_command_batch_matches_tensor_scalar_chain(graph, nodes, ops, interp)) return null;

    Value* left = nn_dict_get(left_node, interp, "source-value");
    Value* right = nn_dict_get(right_node, interp, "source-value");
    if (!is_tensor(left) || !is_tensor(right) || left.tensor_val == null || right.tensor_val == null ||
        left.tensor_val.kind != TENSOR_PAYLOAD_CONCRETE || right.tensor_val.kind != TENSOR_PAYLOAD_CONCRETE ||
        left.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 || right.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
        left.tensor_val.device != TENSOR_DEVICE_VULKAN || right.tensor_val.device != TENSOR_DEVICE_VULKAN ||
        !tensor_has_valid_device_storage(left.tensor_val, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(right.tensor_val, TENSOR_DEVICE_VULKAN) ||
        !tensor_graph_run_tensors_same_dense_shape(left.tensor_val, right.tensor_val)) {
        return null;
    }

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_map_tensor_scalar_chain_f32(
        left.tensor_val.device_handle,
        right.tensor_val.device_handle,
        left.tensor_val.byte_len,
        left.tensor_val.element_count,
        op_count,
        scalars,
        modes,
        ops,
        left.tensor_val.rank,
        left.tensor_val.shape,
        left.tensor_val.strides,
        &out_device
    );
    if (status == OMNI_TENSOR_VULKAN_UNAVAILABLE || status == OMNI_TENSOR_VULKAN_UNSUPPORTED || status == OMNI_TENSOR_VULKAN_INVALID) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return null;
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "tensor/run: Vulkan tensor/scalar map-chain reuse graph failed");
    }
    return ml_optimizer_vulkan_tensor_value(left.tensor_val, out_device, interp, "tensor/run: failed to allocate Vulkan tensor/scalar map-chain output tensor metadata", "tensor/run: failed to allocate Vulkan tensor/scalar map-chain output tensor value");
}

fn bool tensor_graph_run_command_batch_matches_view_scalar_chain(Value* graph, Value*[] nodes, uint* ops, Interp* interp) {
    if (nodes.len < 3 || ops == null) return false;
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
        depends_on == null || depends_on.tag != ARRAY || depends_on.array_val == null || depends_on.array_val.length != 1) {
        return false;
    }
    Value* dep = depends_on.array_val.items[0];
    if (dep == null || dep.tag != INT || dep.int_val != 1) return false;

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

fn bool tensor_graph_run_direct_transpose_view_matches_source(Value* view_node, TensorVal* source, usz** out_shape, usz** out_strides, Interp* interp) {
    if (view_node == null || source == null || out_shape == null || out_strides == null) return false;
    *out_shape = null;
    *out_strides = null;
    if (!nn_symbol_is(nn_dict_get(view_node, interp, "kind"), interp, "tensor-view") ||
        !nn_symbol_is(nn_dict_get(view_node, interp, "operation"), interp, "transpose-view") ||
        !tensor_graph_run_node_id_is(view_node, interp, 1) ||
        !tensor_graph_run_single_input_is(view_node, interp, 0)) {
        return false;
    }
    if (source.rank != 2 || source.shape == null || source.strides == null || !tensor_dense_row_major(source)) return false;
    Value* storage_offset = nn_dict_get(view_node, interp, "storage-offset");
    if (storage_offset == null || storage_offset.tag != INT || storage_offset.int_val != 0) return false;

    usz* shape = null;
    usz rank = 0;
    if (Value* err = tensor_graph_run_usz_array(nn_dict_get(view_node, interp, "shape"), interp, "tensor/run: view shape is invalid", &shape, &rank)) return false;
    if (rank != 2 || shape[0] != source.shape[1] || shape[1] != source.shape[0]) {
        if (shape != null) mem::free(shape);
        return false;
    }
    usz* strides = null;
    usz stride_rank = 0;
    if (Value* err = tensor_graph_run_usz_array(nn_dict_get(view_node, interp, "strides"), interp, "tensor/run: view strides are invalid", &strides, &stride_rank)) {
        mem::free(shape);
        return false;
    }
    if (stride_rank != 2 || strides[0] != source.strides[1] || strides[1] != source.strides[0]) {
        mem::free(shape);
        mem::free(strides);
        return false;
    }
    *out_shape = shape;
    *out_strides = strides;
    return true;
}

fn Value* tensor_graph_run_try_view_scalar_map_chain(Value* graph, Value*[] nodes, usz output_index, Interp* interp) {
    if (nodes.len < 3 || output_index + 1 != nodes.len) return null;
    Value* source_node = nodes[0];
    Value* view_node = nodes[1];
    if (source_node == null || view_node == null || source_node.tag != HASHMAP || view_node.tag != HASHMAP) return null;
    if (!nn_symbol_is(nn_dict_get(graph, interp, "family"), interp, "map-expression")) return null;
    if (!nn_symbol_is(nn_dict_get(source_node, interp, "kind"), interp, "tensor-source") ||
        !tensor_graph_run_node_id_is(source_node, interp, 0)) {
        return null;
    }

    usz op_count = nodes.len - 2;
    float* scalars = mem::new_array(float, op_count);
    defer mem::free(scalars);
    uint* modes = mem::new_array(uint, op_count);
    defer mem::free(modes);
    uint* ops = mem::new_array(uint, op_count);
    defer mem::free(ops);
    for (usz i = 0; i < op_count; i++) {
        Value* node = nodes[i + 2];
        if (node == null || node.tag != HASHMAP) return null;
        if (!tensor_graph_run_node_id_is(node, interp, i + 2) ||
            !tensor_graph_run_single_input_is(node, interp, i + 1) ||
            !tensor_graph_run_scalar_map_parts(node, interp, &modes[i], &scalars[i], &ops[i])) {
            return null;
        }
    }
    if (!tensor_graph_run_selected_region_plan_matches(graph, interp, "view-scalar-map-chain") ||
        !tensor_graph_run_command_batch_matches_view_scalar_chain(graph, nodes, ops, interp)) return null;

    Value* source = nn_dict_get(source_node, interp, "source-value");
    if (!is_tensor(source) || source.tensor_val == null ||
        source.tensor_val.kind != TENSOR_PAYLOAD_CONCRETE ||
        source.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
        source.tensor_val.device != TENSOR_DEVICE_VULKAN ||
        !tensor_has_valid_device_storage(source.tensor_val, TENSOR_DEVICE_VULKAN)) {
        return null;
    }

    usz* shape = null;
    usz* strides = null;
    if (!tensor_graph_run_direct_transpose_view_matches_source(view_node, source.tensor_val, &shape, &strides, interp)) return null;
    defer mem::free(shape);
    defer mem::free(strides);
    usz element_count = 0;
    if (!tensor_graph_run_shape_element_count(shape, 2, &element_count) || element_count != source.tensor_val.element_count) return null;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_map_view_scalar_chain_f32(
        source.tensor_val.device_handle,
        source.tensor_val.byte_len,
        element_count,
        op_count,
        scalars,
        modes,
        ops,
        2,
        shape,
        strides,
        &out_device
    );
    if (status == OMNI_TENSOR_VULKAN_UNAVAILABLE || status == OMNI_TENSOR_VULKAN_UNSUPPORTED || status == OMNI_TENSOR_VULKAN_INVALID) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return null;
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "tensor/run: Vulkan view/scalar map-chain reuse graph failed");
    }

    usz[] shape_slice = shape[0:2];
    TensorVal* tensor = tensor_alloc_payload_base_checked(TENSOR_DTYPE_FLOAT32, shape_slice, false, false, "tensor/run: failed to allocate Vulkan view/scalar-chain tensor metadata");
    if (tensor == null) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_runtime_raise(interp, "tensor/run: failed to allocate Vulkan view/scalar-chain tensor metadata");
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;
    return tensor_graph_run_tensor_result(tensor, interp, "tensor/run: failed to allocate Vulkan view/scalar-chain output tensor value");
}

fn bool tensor_graph_run_command_batch_matches_view_tensor_scalar_chain(Value* graph, Value*[] nodes, uint first_op, uint* ops, Interp* interp) {
    if (nodes.len < 5) return false;
    Value* batches = nn_dict_get(graph, interp, "command-batches");
    if (batches == null || batches.tag != ARRAY || batches.array_val == null || batches.array_val.length != 1) return false;
    Value* batch = batches.array_val.items[0];
    if (batch == null || batch.tag != HASHMAP) return false;
    if (!nn_symbol_is(nn_dict_get(batch, interp, "execution"), interp, "command-buffer-candidate")) return false;
    if (!nn_symbol_is(nn_dict_get(batch, interp, "barrier-policy"), interp, "shader-write-to-shader-read-between-dispatches")) return false;

    usz scalar_op_count = nodes.len - 4;
    usz dispatch_count = scalar_op_count + 1;
    if (!tensor_graph_run_int_field_is(batch, interp, "launch-count", dispatch_count) ||
        !tensor_graph_run_int_field_is(batch, interp, "dispatch-count", dispatch_count) ||
        !tensor_graph_run_int_field_is(batch, interp, "barrier-count", dispatch_count - 1)) {
        return false;
    }

    Value* batch_nodes = nn_dict_get(batch, interp, "nodes");
    Value* dispatches = nn_dict_get(batch, interp, "dispatches");
    Value* depends_on = nn_dict_get(batch, interp, "depends-on");
    if (batch_nodes == null || batch_nodes.tag != ARRAY || batch_nodes.array_val == null || batch_nodes.array_val.length != dispatch_count ||
        dispatches == null || dispatches.tag != ARRAY || dispatches.array_val == null || dispatches.array_val.length != dispatch_count ||
        depends_on == null || depends_on.tag != ARRAY || depends_on.array_val == null || depends_on.array_val.length != 2) {
        return false;
    }
    Value* view_dep = depends_on.array_val.items[0];
    Value* right_dep = depends_on.array_val.items[1];
    if (view_dep == null || right_dep == null || view_dep.tag != INT || right_dep.tag != INT || view_dep.int_val != 1 || right_dep.int_val != 2) return false;

    for (usz i = 0; i < dispatch_count; i++) {
        usz expected_node = i + 3;
        Value* batch_node = batch_nodes.array_val.items[i];
        if (batch_node == null || batch_node.tag != INT || batch_node.int_val < 0 || (usz)batch_node.int_val != expected_node) return false;
        Value* dispatch = dispatches.array_val.items[i];
        if (dispatch == null || dispatch.tag != HASHMAP) return false;
        uint dispatch_op = 0;
        if (!tensor_graph_run_int_field_is(dispatch, interp, "node", expected_node) ||
            !tensor_graph_run_binary_op(nn_dict_get(dispatch, interp, "operation"), interp, &dispatch_op)) {
            return false;
        }
        if (i == 0) {
            if (dispatch_op != first_op) return false;
        } else if (ops == null || dispatch_op != ops[i - 1]) {
            return false;
        }
    }
    return true;
}

fn bool tensor_graph_run_dense_tensor_matches_shape(TensorVal* tensor, usz* shape, usz rank, usz element_count) {
    if (tensor == null || shape == null || tensor.rank != rank || tensor.element_count != element_count ||
        tensor.dtype != TENSOR_DTYPE_FLOAT32 || tensor.device != TENSOR_DEVICE_VULKAN ||
        tensor.kind != TENSOR_PAYLOAD_CONCRETE || !tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_VULKAN) ||
        !tensor_dense_row_major(tensor)) {
        return false;
    }
    for (usz i = 0; i < rank; i++) {
        if (tensor.shape[i] != shape[i]) return false;
    }
    return true;
}

fn Value* tensor_graph_run_try_view_tensor_scalar_map_chain(Value* graph, Value*[] nodes, usz output_index, Interp* interp) {
    if (nodes.len < 5 || output_index + 1 != nodes.len) return null;
    Value* source_node = nodes[0];
    Value* view_node = nodes[1];
    Value* right_node = nodes[2];
    Value* first_map = nodes[3];
    if (source_node == null || view_node == null || right_node == null || first_map == null ||
        source_node.tag != HASHMAP || view_node.tag != HASHMAP || right_node.tag != HASHMAP || first_map.tag != HASHMAP) return null;
    if (!nn_symbol_is(nn_dict_get(graph, interp, "family"), interp, "map-expression")) return null;
    if (!nn_symbol_is(nn_dict_get(source_node, interp, "kind"), interp, "tensor-source") ||
        !nn_symbol_is(nn_dict_get(right_node, interp, "kind"), interp, "tensor-source") ||
        !tensor_graph_run_node_id_is(source_node, interp, 0) ||
        !tensor_graph_run_node_id_is(right_node, interp, 2) ||
        !tensor_graph_run_node_id_is(first_map, interp, 3)) {
        return null;
    }

    uint first_op = 0;
    if (!tensor_graph_run_binary_tensor_map_op(first_map, interp, &first_op)) return null;
    Value* first_inputs = nn_dict_get(first_map, interp, "inputs");
    Value* first_left = first_inputs.array_val.items[0];
    Value* first_right = first_inputs.array_val.items[1];
    if (first_left == null || first_right == null || first_left.tag != INT || first_right.tag != INT ||
        first_left.int_val != 1 || first_right.int_val != 2) {
        return null;
    }

    usz scalar_op_count = nodes.len - 4;
    float* scalars = mem::new_array(float, scalar_op_count);
    defer mem::free(scalars);
    uint* modes = mem::new_array(uint, scalar_op_count);
    defer mem::free(modes);
    uint* ops = mem::new_array(uint, scalar_op_count);
    defer mem::free(ops);
    for (usz i = 0; i < scalar_op_count; i++) {
        Value* node = nodes[i + 4];
        if (node == null || node.tag != HASHMAP) return null;
        if (!tensor_graph_run_node_id_is(node, interp, i + 4) ||
            !tensor_graph_run_single_input_is(node, interp, i + 3) ||
            !tensor_graph_run_scalar_map_parts(node, interp, &modes[i], &scalars[i], &ops[i])) {
            return null;
        }
    }
    if (!tensor_graph_run_selected_region_plan_matches(graph, interp, "view-tensor-scalar-map-chain") ||
        !tensor_graph_run_command_batch_matches_view_tensor_scalar_chain(graph, nodes, first_op, ops, interp)) return null;

    Value* source = nn_dict_get(source_node, interp, "source-value");
    Value* right = nn_dict_get(right_node, interp, "source-value");
    if (!is_tensor(source) || !is_tensor(right) || source.tensor_val == null || right.tensor_val == null ||
        source.tensor_val.kind != TENSOR_PAYLOAD_CONCRETE ||
        source.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
        source.tensor_val.device != TENSOR_DEVICE_VULKAN ||
        !tensor_has_valid_device_storage(source.tensor_val, TENSOR_DEVICE_VULKAN)) {
        return null;
    }

    usz* shape = null;
    usz* view_strides = null;
    if (!tensor_graph_run_direct_transpose_view_matches_source(view_node, source.tensor_val, &shape, &view_strides, interp)) return null;
    defer mem::free(shape);
    defer mem::free(view_strides);
    usz element_count = 0;
    if (!tensor_graph_run_shape_element_count(shape, 2, &element_count) || element_count != source.tensor_val.element_count ||
        !tensor_graph_run_dense_tensor_matches_shape(right.tensor_val, shape, 2, element_count)) {
        return null;
    }

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_map_view_tensor_scalar_chain_f32(
        source.tensor_val.device_handle,
        right.tensor_val.device_handle,
        source.tensor_val.byte_len,
        right.tensor_val.byte_len,
        element_count,
        first_op,
        scalar_op_count,
        scalars,
        modes,
        ops,
        2,
        shape,
        view_strides,
        right.tensor_val.strides,
        &out_device
    );
    if (status == OMNI_TENSOR_VULKAN_UNAVAILABLE || status == OMNI_TENSOR_VULKAN_UNSUPPORTED || status == OMNI_TENSOR_VULKAN_INVALID) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return null;
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "tensor/run: Vulkan view/tensor scalar-chain reuse graph failed");
    }

    usz[] shape_slice = shape[0:2];
    TensorVal* tensor = tensor_alloc_payload_base_checked(TENSOR_DTYPE_FLOAT32, shape_slice, false, false, "tensor/run: failed to allocate Vulkan view/tensor-chain tensor metadata");
    if (tensor == null) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_runtime_raise(interp, "tensor/run: failed to allocate Vulkan view/tensor-chain tensor metadata");
    }
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;
    return tensor_graph_run_tensor_result(tensor, interp, "tensor/run: failed to allocate Vulkan view/tensor-chain output tensor value");
}

fn bool tensor_graph_run_command_batch_matches_contract_scalar_chain(Value* graph, Value*[] nodes, uint* ops, Interp* interp) {
    if (nodes.len < 4 || ops == null) return false;
    Value* batches = nn_dict_get(graph, interp, "command-batches");
    if (batches == null || batches.tag != ARRAY || batches.array_val == null || batches.array_val.length != 1) return false;
    Value* batch = batches.array_val.items[0];
    if (batch == null || batch.tag != HASHMAP) return false;
    if (!nn_symbol_is(nn_dict_get(batch, interp, "execution"), interp, "command-buffer-candidate")) return false;
    if (!nn_symbol_is(nn_dict_get(batch, interp, "barrier-policy"), interp, "shader-write-to-shader-read-between-dispatches")) return false;

    usz dispatch_count = nodes.len - 2;
    if (!tensor_graph_run_int_field_is(batch, interp, "launch-count", dispatch_count) ||
        !tensor_graph_run_int_field_is(batch, interp, "dispatch-count", dispatch_count) ||
        !tensor_graph_run_int_field_is(batch, interp, "barrier-count", dispatch_count - 1)) {
        return false;
    }

    Value* batch_nodes = nn_dict_get(batch, interp, "nodes");
    Value* dispatches = nn_dict_get(batch, interp, "dispatches");
    Value* depends_on = nn_dict_get(batch, interp, "depends-on");
    if (batch_nodes == null || batch_nodes.tag != ARRAY || batch_nodes.array_val == null || batch_nodes.array_val.length != dispatch_count ||
        dispatches == null || dispatches.tag != ARRAY || dispatches.array_val == null || dispatches.array_val.length != dispatch_count ||
        depends_on == null || depends_on.tag != ARRAY || depends_on.array_val == null || depends_on.array_val.length != 2) {
        return false;
    }
    Value* left_dep = depends_on.array_val.items[0];
    Value* right_dep = depends_on.array_val.items[1];
    if (left_dep == null || right_dep == null || left_dep.tag != INT || right_dep.tag != INT || left_dep.int_val != 0 || right_dep.int_val != 1) return false;

    for (usz i = 0; i < dispatch_count; i++) {
        usz expected_node = i + 2;
        Value* batch_node = batch_nodes.array_val.items[i];
        if (batch_node == null || batch_node.tag != INT || batch_node.int_val < 0 || (usz)batch_node.int_val != expected_node) return false;
        Value* dispatch = dispatches.array_val.items[i];
        if (dispatch == null || dispatch.tag != HASHMAP || !tensor_graph_run_int_field_is(dispatch, interp, "node", expected_node)) return false;
        if (i == 0) {
            if (!nn_symbol_is(nn_dict_get(dispatch, interp, "kind"), interp, "tensor-contract")) return false;
        } else {
            uint dispatch_op = 0;
            if (!tensor_graph_run_binary_op(nn_dict_get(dispatch, interp, "operation"), interp, &dispatch_op) ||
                dispatch_op != ops[i - 1]) {
                return false;
            }
        }
    }
    return true;
}

fn Value* tensor_graph_run_try_contract_scalar_map_chain(Value* graph, Value*[] nodes, usz output_index, Interp* interp) {
    if (nodes.len < 4 || output_index + 1 != nodes.len) return null;
    Value* left_node = nodes[0];
    Value* right_node = nodes[1];
    Value* contract_node = nodes[2];
    if (left_node == null || right_node == null || contract_node == null ||
        left_node.tag != HASHMAP || right_node.tag != HASHMAP || contract_node.tag != HASHMAP) return null;
    if (!nn_symbol_is(nn_dict_get(graph, interp, "family"), interp, "map-expression")) return null;
    if (!nn_symbol_is(nn_dict_get(left_node, interp, "kind"), interp, "tensor-source") ||
        !nn_symbol_is(nn_dict_get(right_node, interp, "kind"), interp, "tensor-source") ||
        !nn_symbol_is(nn_dict_get(contract_node, interp, "kind"), interp, "tensor-contract") ||
        !tensor_graph_run_node_id_is(left_node, interp, 0) ||
        !tensor_graph_run_node_id_is(right_node, interp, 1) ||
        !tensor_graph_run_node_id_is(contract_node, interp, 2)) {
        return null;
    }
    Value* contract_inputs = nn_dict_get(contract_node, interp, "inputs");
    if (contract_inputs == null || contract_inputs.tag != ARRAY || contract_inputs.array_val == null || contract_inputs.array_val.length != 2) return null;
    Value* contract_left = contract_inputs.array_val.items[0];
    Value* contract_right = contract_inputs.array_val.items[1];
    if (contract_left == null || contract_right == null || contract_left.tag != INT || contract_right.tag != INT ||
        contract_left.int_val != 0 || contract_right.int_val != 1) return null;

    usz scalar_op_count = nodes.len - 3;
    float* scalars = mem::new_array(float, scalar_op_count);
    defer mem::free(scalars);
    uint* modes = mem::new_array(uint, scalar_op_count);
    defer mem::free(modes);
    uint* ops = mem::new_array(uint, scalar_op_count);
    defer mem::free(ops);
    for (usz i = 0; i < scalar_op_count; i++) {
        Value* node = nodes[i + 3];
        if (node == null || node.tag != HASHMAP) return null;
        if (!tensor_graph_run_node_id_is(node, interp, i + 3) ||
            !tensor_graph_run_single_input_is(node, interp, i + 2) ||
            !tensor_graph_run_scalar_map_parts(node, interp, &modes[i], &scalars[i], &ops[i])) {
            return null;
        }
    }
    if (!tensor_graph_run_selected_region_plan_matches(graph, interp, "contract-scalar-map-chain") ||
        !tensor_graph_run_command_batch_matches_contract_scalar_chain(graph, nodes, ops, interp)) return null;

    Value* left = nn_dict_get(left_node, interp, "source-value");
    Value* right = nn_dict_get(right_node, interp, "source-value");
    if (!is_tensor(left) || !is_tensor(right) || left.tensor_val == null || right.tensor_val == null ||
        left.tensor_val.kind != TENSOR_PAYLOAD_CONCRETE || right.tensor_val.kind != TENSOR_PAYLOAD_CONCRETE ||
        left.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 || right.tensor_val.dtype != TENSOR_DTYPE_FLOAT32 ||
        left.tensor_val.device != TENSOR_DEVICE_VULKAN || right.tensor_val.device != TENSOR_DEVICE_VULKAN ||
        !tensor_has_valid_device_storage(left.tensor_val, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(right.tensor_val, TENSOR_DEVICE_VULKAN) ||
        !tensor_dense_row_major(left.tensor_val) || !tensor_dense_row_major(right.tensor_val)) {
        return null;
    }

    usz* left_axes = null;
    usz left_axis_count = 0;
    if (Value* err = tensor_graph_run_usz_array(nn_dict_get(contract_node, interp, "left-axes"), interp, "tensor/run: contract left axes are invalid", &left_axes, &left_axis_count)) return null;
    defer if (left_axes != null) mem::free(left_axes);
    usz* right_axes = null;
    usz right_axis_count = 0;
    if (Value* err = tensor_graph_run_usz_array(nn_dict_get(contract_node, interp, "right-axes"), interp, "tensor/run: contract right axes are invalid", &right_axes, &right_axis_count)) return null;
    defer if (right_axes != null) mem::free(right_axes);
    if (left_axis_count != right_axis_count) return null;

    usz* shape = null;
    usz rank = 0;
    if (Value* err = tensor_graph_run_usz_array(nn_dict_get(contract_node, interp, "shape"), interp, "tensor/run: contract output shape is invalid", &shape, &rank)) return null;
    defer if (shape != null) mem::free(shape);
    usz element_count = 0;
    if (!tensor_graph_run_shape_element_count(shape, rank, &element_count)) return null;
    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = rank == 0 ? scalar_shape[0:0] : shape[0:rank];
    TensorVal* tensor = tensor_alloc_payload_base_checked(TENSOR_DTYPE_FLOAT32, shape_slice, false, false, "tensor/run: failed to allocate Vulkan contract/scalar-chain tensor metadata");
    if (tensor == null) return null;
    defer if (tensor != null) tensor_free_payload(tensor);
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_contract_scalar_chain_f32(
        left.tensor_val.device_handle,
        right.tensor_val.device_handle,
        left.tensor_val.rank,
        right.tensor_val.rank,
        left_axis_count,
        left_axes,
        right_axes,
        element_count,
        rank,
        left.tensor_val.shape,
        left.tensor_val.strides,
        right.tensor_val.shape,
        right.tensor_val.strides,
        tensor.shape,
        tensor.strides,
        scalar_op_count,
        scalars,
        modes,
        ops,
        &out_device
    );
    if (status == OMNI_TENSOR_VULKAN_UNAVAILABLE || status == OMNI_TENSOR_VULKAN_UNSUPPORTED || status == OMNI_TENSOR_VULKAN_INVALID) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return null;
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "tensor/run: Vulkan contract/scalar-chain reuse graph failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;
    TensorVal* result_tensor = tensor;
    tensor = null;
    return tensor_graph_run_tensor_result(result_tensor, interp, "tensor/run: failed to allocate Vulkan contract/scalar-chain output tensor value");
}
