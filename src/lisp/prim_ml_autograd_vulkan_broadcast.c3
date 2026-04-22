module lisp;

import std::core::mem;
import main;

fn Value* ml_grad_vulkan_alias_with_shape(Interp* interp, TensorVal* source, TensorVal* shape_source) {
    if (source == null || shape_source == null ||
        source.device != TENSOR_DEVICE_VULKAN ||
        shape_source.device != TENSOR_DEVICE_VULKAN ||
        source.dtype != shape_source.dtype ||
        source.element_count != shape_source.element_count ||
        source.byte_len != shape_source.byte_len ||
        !tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN) ||
        !tensor_dense_row_major(source) ||
        !tensor_dense_row_major(shape_source) ||
        source.device_handle == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan broadcast gradient shape alias is invalid", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = shape_source.rank == 0 ? scalar_shape[0:0] : shape_source.shape[0:shape_source.rank];
    TensorVal* tensor = tensor_alloc_payload_base_checked(source.dtype, shape_slice, false, false, "ml/grad: failed to allocate Vulkan broadcast gradient metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "ml/grad: failed to allocate Vulkan broadcast gradient metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;
    tensor.device_handle = omni_tensor_backend_vulkan_retain(source.device_handle);
    if (tensor.device_handle == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/grad: failed to retain Vulkan broadcast gradient storage");
    }
    tensor.device_finalizer = &tensor_vulkan_device_finalizer;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/grad: failed to allocate Vulkan broadcast gradient value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* ml_grad_vulkan_reduce_broadcast_to_child(Interp* interp, Value* upstream_value, TensorVal* child, TensorVal* output) {
    if (upstream_value == null || upstream_value.tag != TENSOR || upstream_value.tensor_val == null ||
        child == null || output == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan broadcast gradient state is missing", null);
    }
    TensorVal* upstream = upstream_value.tensor_val;
    if (tensor_same_shape(upstream, child)) return upstream_value;
    if (upstream.device != TENSOR_DEVICE_VULKAN ||
        child.device != TENSOR_DEVICE_VULKAN ||
        output.device != TENSOR_DEVICE_VULKAN ||
        (upstream.dtype != TENSOR_DTYPE_FLOAT32 && upstream.dtype != TENSOR_DTYPE_DOUBLE) ||
        (child.dtype != TENSOR_DTYPE_FLOAT32 && child.dtype != TENSOR_DTYPE_DOUBLE) ||
        upstream.dtype != child.dtype ||
        child.rank > output.rank ||
        !tensor_dense_row_major(upstream) ||
        !tensor_dense_row_major(child) ||
        !tensor_dense_row_major(output) ||
        !tensor_map_operand_matches_shape(child, output.shape[0:output.rank])) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan map backward does not support this broadcast gradient", null);
    }

    usz* axes = null;
    usz* reduced_shape = null;
    if (output.rank > 0) {
        axes = (usz*)mem::malloc(usz.sizeof * output.rank);
        reduced_shape = (usz*)mem::malloc(usz.sizeof * output.rank);
        if (axes == null || reduced_shape == null) {
            if (axes != null) mem::free(axes);
            if (reduced_shape != null) mem::free(reduced_shape);
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/grad: failed to allocate Vulkan broadcast reduction metadata", null);
        }
    }
    defer if (axes != null) mem::free(axes);
    defer if (reduced_shape != null) mem::free(reduced_shape);

    usz axis_delta = output.rank - child.rank;
    usz axis_count = 0;
    usz reduced_rank = 0;
    for (usz out_axis = 0; out_axis < output.rank; out_axis++) {
        bool reduce_axis = out_axis < axis_delta;
        if (!reduce_axis) {
            usz child_axis = out_axis - axis_delta;
            reduce_axis = child.shape[child_axis] == 1 && output.shape[out_axis] != 1;
        }
        if (reduce_axis) {
            axes[axis_count++] = out_axis;
        } else {
            reduced_shape[reduced_rank++] = output.shape[out_axis];
        }
    }
    if (axis_count == 0) return upstream_value;

    usz[1] scalar_shape = { 0 };
    usz[] reduced_slice = reduced_rank == 0 ? scalar_shape[0:0] : reduced_shape[0:reduced_rank];
    Value* reduced_value = ml_reduction_eval_vulkan(
        upstream,
        reduced_slice,
        axes,
        axis_count,
        ML_REDUCTION_SUM,
        interp,
        "ml/grad: failed to allocate Vulkan broadcast gradient metadata",
        "ml/grad: Vulkan broadcast gradient reduction failed"
    );
    if (reduced_value == null || reduced_value.tag == ERROR) return reduced_value;
    TensorVal* reduced = reduced_value.tensor_val;
    return tensor_same_shape(reduced, child) ? reduced_value : ml_grad_vulkan_alias_with_shape(interp, reduced, child);
}
