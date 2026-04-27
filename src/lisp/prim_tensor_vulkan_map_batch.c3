module lisp;

import main;

fn bool tensor_map_scalar_f32_mode(TensorVal* source, uint* out_mode, float* out_scalar) {
    if (source == null || source.kind != TENSOR_PAYLOAD_MAP || !source.map_is_binary || out_mode == null || out_scalar == null) return false;
    if (source.map_left != null && source.map_right == null) {
        *out_mode = 0;
        return tensor_double_to_float32(source.map_right_scalar, out_scalar);
    }
    if (source.map_left == null && source.map_right != null) {
        *out_mode = 1;
        return tensor_double_to_float32(source.map_left_scalar, out_scalar);
    }
    return false;
}

fn Value* tensor_map_chain2_input(TensorVal* source, Interp* interp, TensorVal** out_input) {
    if (out_input == null) return tensor_runtime_raise(interp, "map: Vulkan chain input is missing");
    *out_input = null;
    if (source == null || source.kind != TENSOR_PAYLOAD_MAP) return null;
    Value* input_value = source.map_left != null ? source.map_left : source.map_right;
    if (!is_tensor(input_value) || input_value.tensor_val == null) return null;
    TensorVal* input = input_value.tensor_val;
    if (input.kind != TENSOR_PAYLOAD_CONCRETE || input.device != TENSOR_DEVICE_VULKAN || input.dtype != TENSOR_DTYPE_FLOAT32) return null;
    if (!tensor_has_valid_device_storage(input, TENSOR_DEVICE_VULKAN)) return tensor_runtime_raise(interp, "map: Vulkan tensor backing storage is missing");
    if (!tensor_dense_row_major(input)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan chain batching currently requires dense row-major Float32 storage", null);
    }
    if (!tensor_map_operand_matches_output(input, source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    *out_input = input;
    return null;
}

fn Value* tensor_map_try_vulkan_chain2_scalar_value(TensorVal* source, uint second_op, Interp* interp) {
    if (source == null || source.kind != TENSOR_PAYLOAD_MAP || source.dtype != TENSOR_DTYPE_FLOAT32) return null;
    if ((source.map_left != null && tensor_expr_has_non_vulkan_device(source.map_left)) ||
        (source.map_is_binary && source.map_right != null && tensor_expr_has_non_vulkan_device(source.map_right))) {
        return null;
    }

    uint second_mode = 0;
    float second_scalar = 0.0f;
    if (!tensor_map_scalar_f32_mode(source, &second_mode, &second_scalar)) return null;

    Value* inner_value = source.map_left != null ? source.map_left : source.map_right;
    if (!is_tensor(inner_value) || inner_value.tensor_val == null) return null;
    TensorVal* inner = inner_value.tensor_val;
    if (inner.kind != TENSOR_PAYLOAD_MAP || inner.dtype != TENSOR_DTYPE_FLOAT32) return null;

    uint first_op = 0;
    if (!tensor_map_callable_vulkan_op(inner.map_func, &first_op)) return null;
    uint first_mode = 0;
    float first_scalar = 0.0f;
    if (!tensor_map_scalar_f32_mode(inner, &first_mode, &first_scalar)) return null;

    TensorVal* input = null;
    Value* input_err = tensor_map_chain2_input(inner, interp, &input);
    if (input_err != null) return input_err;
    if (input == null) return null;
    if (!tensor_map_operand_matches_output(input, source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, shape_slice, false, false, "map: failed to allocate Vulkan tensor metadata");
    if (result_tensor == null) return tensor_runtime_raise(interp, "map: failed to allocate Vulkan tensor metadata");
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_map_chain2_scalar_f32(
        input.device_handle,
        input.byte_len,
        input.element_count,
        first_scalar,
        first_mode,
        first_op,
        second_scalar,
        second_mode,
        second_op,
        input.rank,
        input.shape,
        input.strides,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_vulkan_status_raise(interp, status, "map: Vulkan command-buffer batch failed");
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, "map: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
        return make_runtime_out_of_memory_error(interp, "Tensor: failed to register result destructor");
    }
    return result;
}
