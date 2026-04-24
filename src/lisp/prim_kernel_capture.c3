module lisp;

fn Value* kernel_capture_symbol(Interp* interp, char[] name) {
    SymbolId sym = interp.symbols.intern(name);
    if (sym == INVALID_SYMBOL_ID) return nn_oom(interp);
    return make_symbol(interp, sym);
}

fn Value* kernel_capture_tensor_shape(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return tensor_runtime_raise(interp, "kernel/capture: tensor metadata is missing");
    Value* out = make_array_checked(interp, tensor.rank, "kernel/capture: out of memory");
    if (out == null || out.tag == ERROR) return out;
    if (out.array_val == null) return nn_oom(interp);
    for (usz i = 0; i < tensor.rank; i++) {
        if (tensor.shape[i] > (usz)long.max) return kernel_run_invalid(interp, "kernel/capture: tensor shape is too large");
        out.array_val.items[i] = make_int(interp, (long)tensor.shape[i]);
        if (out.array_val.items[i] == null) return nn_oom(interp);
        out.array_val.length++;
    }
    return out;
}

fn Value* kernel_capture_descriptor_names(Value* descriptors, Interp* interp, char[] label) {
    if (descriptors == null || descriptors.tag != ARRAY || descriptors.array_val == null) return kernel_run_invalid(interp, label);
    Value* out = make_array_checked(interp, descriptors.array_val.length, "kernel/capture: out of memory");
    if (out == null || out.tag == ERROR) return out;
    if (out.array_val == null) return nn_oom(interp);
    for (usz i = 0; i < descriptors.array_val.length; i++) {
        Value* name = kernel_descriptor_name(descriptors.array_val.items[i], interp, label);
        if (name == null || name.tag == ERROR) return name;
        out.array_val.items[i] = name;
        out.array_val.length++;
    }
    return out;
}

fn Value* kernel_capture_invalidation_key(Value* operation, Value* shape, Interp* interp) {
    Value* key = make_array_checked(interp, 4, "kernel/capture: out of memory");
    if (key == null || key.tag == ERROR) return key;
    if (key.array_val == null) return nn_oom(interp);
    key.array_val.items[0] = kernel_capture_symbol(interp, "vulkan");
    key.array_val.items[1] = operation;
    key.array_val.items[2] = kernel_capture_symbol(interp, "Float32");
    key.array_val.items[3] = shape;
    for (usz i = 0; i < 4; i++) {
        if (key.array_val.items[i] == null || key.array_val.items[i].tag == ERROR) return key.array_val.items[i];
    }
    key.array_val.length = 4;
    return key;
}

fn Value* kernel_capture_node(
    Value* operation,
    char[] family,
    Value* input_names,
    Value* output_names,
    Value* shape,
    Value* push,
    Interp* interp
) {
    Value* node = nn_make_dict(interp, 16);
    if (node == null || node.tag == ERROR) return node;
    if (Value* err = nn_dict_set_symbol(node, interp, "kind", "kernel-node")) return err;
    if (Value* err = nn_dict_set(node, interp, "id", make_int(interp, 0))) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set(node, interp, "operation", operation)) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "family", family)) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "device", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(node, interp, "execution", "direct-helper")) return err;
    if (Value* err = nn_dict_set(node, interp, "inputs", input_names)) return err;
    if (Value* err = nn_dict_set(node, interp, "outputs", output_names)) return err;
    if (Value* err = nn_dict_set(node, interp, "shape", shape)) return err;
    if (Value* err = nn_dict_set(node, interp, "push", push)) return err;
    return node;
}

fn Value* kernel_capture_plan(
    Value* kernel,
    Value* operation,
    char[] family,
    Value* input_descriptors,
    Value* output_descriptors,
    TensorVal* shape_tensor,
    Value* push,
    Interp* interp
) {
    Value* input_names = kernel_capture_descriptor_names(input_descriptors, interp, "kernel/capture: input descriptor name is invalid");
    if (input_names == null || input_names.tag == ERROR) return input_names;
    Value* output_names = kernel_capture_descriptor_names(output_descriptors, interp, "kernel/capture: output descriptor name is invalid");
    if (output_names == null || output_names.tag == ERROR) return output_names;
    Value* shape = kernel_capture_tensor_shape(shape_tensor, interp);
    if (shape == null || shape.tag == ERROR) return shape;
    Value* key = kernel_capture_invalidation_key(operation, shape, interp);
    if (key == null || key.tag == ERROR) return key;
    Value* node = kernel_capture_node(operation, family, input_names, output_names, shape, push, interp);
    if (node == null || node.tag == ERROR) return node;
    Value* nodes = make_array_checked(interp, 1, "kernel/capture: out of memory");
    if (nodes == null || nodes.tag == ERROR) return nodes;
    if (nodes.array_val == null) return nn_oom(interp);
    nodes.array_val.items[0] = node;
    nodes.array_val.length = 1;

    Value* plan = nn_make_dict(interp, 24);
    if (plan == null || plan.tag == ERROR) return plan;
    if (Value* err = nn_dict_set_symbol(plan, interp, "kind", "kernel-graph")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "backend", "vulkan")) return err;
    if (Value* err = nn_dict_set(plan, interp, "operation", operation)) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "family", family)) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "dtype", "Float32")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "device", "vulkan")) return err;
    if (Value* err = nn_dict_set_symbol(plan, interp, "execution", "direct-helper")) return err;
    if (Value* err = nn_dict_set(plan, interp, "node-count", make_int(interp, 1))) return err;
    if (Value* err = nn_dict_set(plan, interp, "nodes", nodes)) return err;
    if (Value* err = nn_dict_set(plan, interp, "inputs", input_names)) return err;
    if (Value* err = nn_dict_set(plan, interp, "outputs", output_names)) return err;
    if (Value* err = nn_dict_set(plan, interp, "shape", shape)) return err;
    if (Value* err = nn_dict_set(plan, interp, "push", push)) return err;
    if (Value* err = nn_dict_set(plan, interp, "invalidation-key", key)) return err;
    if (Value* err = nn_dict_set(plan, interp, "kernel", kernel)) return err;
    return plan;
}

fn Value* kernel_capture_scale_f32(Value* kernel, Value* inputs, Value* push, Interp* interp) {
    Value* input_descriptors = nn_dict_get(kernel, interp, "inputs");
    Value* output_descriptors = nn_dict_get(kernel, interp, "outputs");
    if (input_descriptors == null || input_descriptors.tag != ARRAY || input_descriptors.array_val == null || input_descriptors.array_val.length != 1) {
        return kernel_run_unsupported(interp, "kernel/capture: scale-f32 expects exactly one input descriptor");
    }
    if (output_descriptors == null || output_descriptors.tag != ARRAY || output_descriptors.array_val == null || output_descriptors.array_val.length != 1) {
        return kernel_run_unsupported(interp, "kernel/capture: scale-f32 expects exactly one output descriptor");
    }
    Value* input_descriptor = input_descriptors.array_val.items[0];
    Value* output_descriptor = output_descriptors.array_val.items[0];
    if (!kernel_dtype_is(input_descriptor, interp, "Float32") || !kernel_dtype_is(output_descriptor, interp, "Float32")) {
        return kernel_run_unsupported(interp, "kernel/capture: scale-f32 currently supports Float32 inputs and outputs");
    }
    if (push.hashmap_val.count != 1 || nn_dict_get(push, interp, "scale") == null) {
        return kernel_run_invalid(interp, "kernel/capture: scale-f32 requires exactly one scale push constant");
    }
    Value* input_value = kernel_input_by_descriptor(inputs, input_descriptor, interp, "kernel/capture: scale-f32 input descriptor name is invalid", "kernel/capture: scale-f32 input is missing");
    if (input_value == null || input_value.tag == ERROR) return input_value;
    TensorVal* input_tensor = null;
    Value* materialized = null;
    Value* err = kernel_resolve_vulkan_f32_input(input_value, &input_tensor, &materialized, interp, "kernel/capture: scale-f32 tensor backing storage is missing");
    if (err != null) return err;
    defer if (materialized != null && materialized != input_value) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);
    if (!kernel_shape_matches_tensor(nn_dict_get(input_descriptor, interp, "shape"), input_tensor) ||
        !kernel_shape_matches_tensor(nn_dict_get(output_descriptor, interp, "shape"), input_tensor)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/capture: scale-f32 descriptor shape does not match input tensor", null);
    }
    return kernel_capture_plan(kernel, nn_dict_get(kernel, interp, "operation"), "scale-f32", input_descriptors, output_descriptors, input_tensor, push, interp);
}

<*
@require interp != null
*>
fn Value* prim_kernel_capture(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 3) return primitive_arity_raise(interp, "kernel/capture", "3 arguments: kernel inputs push", args.len);
    if (Value* err = kernel_validate_spec(args[0], interp)) return err;
    if (!kernel_is_value(args[0], interp)) return kernel_invalid(interp, "kernel", "Kernel value", args[0]);
    if (args[1].tag != HASHMAP || args[1].hashmap_val == null) {
        return primitive_arg_type_raise(interp, "kernel/capture", 1, "Dictionary of input tensors", args[1]);
    }
    if (args[2].tag != HASHMAP || args[2].hashmap_val == null) {
        return primitive_arg_type_raise(interp, "kernel/capture", 2, "Dictionary of push constants", args[2]);
    }
    if (!kernel_symbol_is(nn_dict_get(args[0], interp, "backend"), interp, "vulkan")) {
        return kernel_run_unsupported(interp, "kernel/capture: only Vulkan Kernel capture is currently supported");
    }

    Value* operation = nn_dict_get(args[0], interp, "operation");
    if (kernel_symbol_is(operation, interp, "scale-f32")) return kernel_capture_scale_f32(args[0], args[1], args[2], interp);

    uint op = 0;
    uint mode = 0;
    Value* input_descriptor = null;
    Value* output_descriptor = null;
    if (kernel_unary_f32_operation(operation, interp, &op)) {
        if (args[2].hashmap_val.count != 0) return kernel_run_invalid(interp, "kernel/capture: unary Float32 kernels do not accept push constants");
        if (Value* err = kernel_run_validate_unary_f32_spec(args[0], interp, &input_descriptor, &output_descriptor)) return err;
        Value* input_value = kernel_input_by_descriptor(args[1], input_descriptor, interp, "kernel/capture: unary Float32 input descriptor name is invalid", "kernel/capture: unary Float32 input is missing");
        if (input_value == null || input_value.tag == ERROR) return input_value;
        TensorVal* input_tensor = null;
        Value* materialized = null;
        Value* err = kernel_resolve_vulkan_f32_input(input_value, &input_tensor, &materialized, interp, "kernel/capture: unary Float32 tensor backing storage is missing");
        if (err != null) return err;
        defer if (materialized != null && materialized != input_value) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);
        if (!kernel_shape_matches_tensor(nn_dict_get(input_descriptor, interp, "shape"), input_tensor) ||
            !kernel_shape_matches_tensor(nn_dict_get(output_descriptor, interp, "shape"), input_tensor)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/capture: unary Float32 descriptor shape does not match input tensor", null);
        }
        return kernel_capture_plan(args[0], operation, "unary-f32", nn_dict_get(args[0], interp, "inputs"), nn_dict_get(args[0], interp, "outputs"), input_tensor, args[2], interp);
    }
    if (kernel_scalar_f32_operation(operation, interp, &op, &mode)) {
        if (Value* err = kernel_run_validate_scalar_f32_spec(args[0], interp, &input_descriptor, &output_descriptor)) return err;
        if (args[2].hashmap_val.count != 1 || nn_dict_get(args[2], interp, "scalar") == null) return kernel_run_invalid(interp, "kernel/capture: scalar Float32 kernels require exactly one scalar push constant");
        Value* input_value = kernel_input_by_descriptor(args[1], input_descriptor, interp, "kernel/capture: scalar Float32 input descriptor name is invalid", "kernel/capture: scalar Float32 input is missing");
        if (input_value == null || input_value.tag == ERROR) return input_value;
        TensorVal* input_tensor = null;
        Value* materialized = null;
        Value* err = kernel_resolve_vulkan_f32_input(input_value, &input_tensor, &materialized, interp, "kernel/capture: scalar Float32 tensor backing storage is missing");
        if (err != null) return err;
        defer if (materialized != null && materialized != input_value) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);
        if (!kernel_shape_matches_tensor(nn_dict_get(input_descriptor, interp, "shape"), input_tensor) ||
            !kernel_shape_matches_tensor(nn_dict_get(output_descriptor, interp, "shape"), input_tensor)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/capture: scalar Float32 descriptor shape does not match input tensor", null);
        }
        return kernel_capture_plan(args[0], operation, "scalar-f32", nn_dict_get(args[0], interp, "inputs"), nn_dict_get(args[0], interp, "outputs"), input_tensor, args[2], interp);
    }
    if (kernel_binary_f32_operation(operation, interp, &op)) {
        if (args[2].hashmap_val.count != 0) return kernel_run_invalid(interp, "kernel/capture: binary Float32 kernels do not accept push constants");
        Value* left_descriptor = null;
        Value* right_descriptor = null;
        if (Value* err = kernel_run_validate_binary_f32_spec(args[0], interp, &left_descriptor, &right_descriptor, &output_descriptor)) return err;
        Value* left_value = kernel_input_by_descriptor(args[1], left_descriptor, interp, "kernel/capture: binary Float32 left input descriptor name is invalid", "kernel/capture: binary Float32 left input is missing");
        if (left_value == null || left_value.tag == ERROR) return left_value;
        Value* right_value = kernel_input_by_descriptor(args[1], right_descriptor, interp, "kernel/capture: binary Float32 right input descriptor name is invalid", "kernel/capture: binary Float32 right input is missing");
        if (right_value == null || right_value.tag == ERROR) return right_value;
        TensorVal* left_tensor = null;
        TensorVal* right_tensor = null;
        Value* left_materialized = null;
        Value* right_materialized = null;
        Value* err = kernel_resolve_vulkan_f32_input(left_value, &left_tensor, &left_materialized, interp, "kernel/capture: binary Float32 left tensor backing storage is missing");
        if (err != null) return err;
        defer if (left_materialized != null && left_materialized != left_value) boundary_cleanup_materialized_value(null, left_materialized, interp.current_scope);
        err = kernel_resolve_vulkan_f32_input(right_value, &right_tensor, &right_materialized, interp, "kernel/capture: binary Float32 right tensor backing storage is missing");
        if (err != null) return err;
        defer if (right_materialized != null && right_materialized != right_value) boundary_cleanup_materialized_value(null, right_materialized, interp.current_scope);
        if (!ml_sgd_tensor_shapes_match(left_tensor, right_tensor)) return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/capture: binary Float32 input tensor shapes must match", null);
        if (!kernel_shape_matches_tensor(nn_dict_get(left_descriptor, interp, "shape"), left_tensor) ||
            !kernel_shape_matches_tensor(nn_dict_get(right_descriptor, interp, "shape"), right_tensor) ||
            !kernel_shape_matches_tensor(nn_dict_get(output_descriptor, interp, "shape"), left_tensor)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/capture: binary Float32 descriptor shape does not match input tensors", null);
        }
        return kernel_capture_plan(args[0], operation, "binary-f32", nn_dict_get(args[0], interp, "inputs"), nn_dict_get(args[0], interp, "outputs"), left_tensor, args[2], interp);
    }
    return kernel_run_unsupported(interp, "kernel/capture: unsupported Kernel operation for Vulkan backend");
}
