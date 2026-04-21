module lisp;

fn bool kernel_unary_f32_operation(Value* operation, Interp* interp, uint* out_op) {
    if (operation == null || out_op == null) return false;
    if (nn_symbol_is(operation, interp, "abs-f32")) {
        *out_op = 0;
        return true;
    }
    if (nn_symbol_is(operation, interp, "neg-f32")) {
        *out_op = 1;
        return true;
    }
    if (nn_symbol_is(operation, interp, "sqrt-f32")) {
        *out_op = 2;
        return true;
    }
    if (nn_symbol_is(operation, interp, "identity-f32")) {
        *out_op = 3;
        return true;
    }
    if (nn_symbol_is(operation, interp, "zero-f32")) {
        *out_op = 4;
        return true;
    }
    if (nn_symbol_is(operation, interp, "sin-f32")) {
        *out_op = 5;
        return true;
    }
    if (nn_symbol_is(operation, interp, "cos-f32")) {
        *out_op = 6;
        return true;
    }
    if (nn_symbol_is(operation, interp, "tan-f32")) {
        *out_op = 7;
        return true;
    }
    if (nn_symbol_is(operation, interp, "asin-f32")) {
        *out_op = 8;
        return true;
    }
    if (nn_symbol_is(operation, interp, "acos-f32")) {
        *out_op = 9;
        return true;
    }
    if (nn_symbol_is(operation, interp, "atan-f32")) {
        *out_op = 10;
        return true;
    }
    if (nn_symbol_is(operation, interp, "sinh-f32")) {
        *out_op = 11;
        return true;
    }
    if (nn_symbol_is(operation, interp, "cosh-f32")) {
        *out_op = 12;
        return true;
    }
    if (nn_symbol_is(operation, interp, "tanh-f32")) {
        *out_op = 13;
        return true;
    }
    if (nn_symbol_is(operation, interp, "exp-f32")) {
        *out_op = 14;
        return true;
    }
    if (nn_symbol_is(operation, interp, "log-f32")) {
        *out_op = 15;
        return true;
    }
    if (nn_symbol_is(operation, interp, "log10-f32")) {
        *out_op = 16;
        return true;
    }
    if (nn_symbol_is(operation, interp, "normal-cdf-f32")) {
        *out_op = 19;
        return true;
    }
    return false;
}

fn Value* kernel_run_validate_unary_f32_spec(Value* kernel, Interp* interp, Value** out_input_descriptor, Value** out_output_descriptor) {
    Value* inputs = nn_dict_get(kernel, interp, "inputs");
    Value* outputs = nn_dict_get(kernel, interp, "outputs");
    if (inputs == null || inputs.tag != ARRAY || inputs.array_val == null || inputs.array_val.length != 1) {
        return kernel_run_unsupported(interp, "kernel/run: unary Float32 kernels expect exactly one input descriptor");
    }
    if (outputs == null || outputs.tag != ARRAY || outputs.array_val == null || outputs.array_val.length != 1) {
        return kernel_run_unsupported(interp, "kernel/run: unary Float32 kernels expect exactly one output descriptor");
    }
    Value* input_descriptor = inputs.array_val.items[0];
    Value* output_descriptor = outputs.array_val.items[0];
    if (!kernel_dtype_is(input_descriptor, interp, "Float32") || !kernel_dtype_is(output_descriptor, interp, "Float32")) {
        return kernel_run_unsupported(interp, "kernel/run: unary kernels currently support Float32 inputs and outputs");
    }
    Value* push_spec = nn_dict_get(kernel, interp, "push");
    if (push_spec != null && push_spec.tag != NIL && (push_spec.tag != HASHMAP || push_spec.hashmap_val == null || push_spec.hashmap_val.count != 0)) {
        return kernel_run_invalid(interp, "kernel/run: unary Float32 kernels do not accept push constants");
    }
    *out_input_descriptor = input_descriptor;
    *out_output_descriptor = output_descriptor;
    return null;
}

fn Value* kernel_run_unary_f32(Value* kernel, Value* inputs, Value* push, Interp* interp, uint op) {
    if (push.hashmap_val.count != 0) return kernel_run_invalid(interp, "kernel/run: unary Float32 kernels do not accept push constants");

    Value* input_descriptor = null;
    Value* output_descriptor = null;
    if (Value* err = kernel_run_validate_unary_f32_spec(kernel, interp, &input_descriptor, &output_descriptor)) return err;

    Value* input_value = kernel_input_by_descriptor(inputs, input_descriptor, interp, "kernel/run: unary Float32 input descriptor name is invalid", "kernel/run: unary Float32 input is missing");
    if (input_value == null || input_value.tag == ERROR) return input_value;
    Value* output_name = kernel_descriptor_name(output_descriptor, interp, "kernel/run: unary Float32 output descriptor name is invalid");
    if (output_name == null || output_name.tag == ERROR) return output_name;

    TensorVal* input_tensor = null;
    Value* materialized = null;
    Value* err = kernel_resolve_vulkan_f32_input(input_value, &input_tensor, &materialized, interp, "kernel/run: unary Float32 tensor backing storage is missing");
    if (err != null) return err;
    defer if (materialized != null && materialized != input_value) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);

    if (!kernel_shape_matches_tensor(nn_dict_get(input_descriptor, interp, "shape"), input_tensor) ||
        !kernel_shape_matches_tensor(nn_dict_get(output_descriptor, interp, "shape"), input_tensor)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/run: unary Float32 descriptor shape does not match input tensor", null);
    }

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_map_unary_f32(
        input_tensor.device_handle,
        input_tensor.byte_len,
        input_tensor.element_count,
        op,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "kernel/run: Vulkan unary Float32 kernel failed");
    }

    Value* output_value = ml_optimizer_vulkan_tensor_value(input_tensor, out_device, interp, "kernel/run: failed to allocate Vulkan output tensor metadata", "kernel/run: failed to allocate Vulkan output tensor value");
    if (output_value == null || output_value.tag == ERROR) return output_value;
    return kernel_output_dict(output_name, output_value, interp);
}
