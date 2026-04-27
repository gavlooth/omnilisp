module lisp;

import std::core::mem;
import main;

struct MlConv1dSpec {
    usz batch;
    usz in_channels;
    usz width;
    usz out_channels;
    usz kernel_in_channels;
    usz kernel_width;
    usz stride;
    usz padding;
    usz dilation;
    usz groups;
    usz in_channels_per_group;
    usz out_channels_per_group;
    usz out_width;
}

fn bool ml_conv_supported_dtype(TensorDType dtype) @inline {
    return dtype == TENSOR_DTYPE_DOUBLE || dtype == TENSOR_DTYPE_FLOAT32;
}

fn Value* ml_conv_parse_positive(Value* value, char[] label, usz* out, Interp* interp) {
    long parsed = 0;
    if (!exact_integer_value_to_i64(value, &parsed)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", label, null);
    }
    if (parsed <= 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", label, null);
    }
    *out = (usz)parsed;
    return null;
}

fn Value* ml_conv_parse_nonnegative(Value* value, char[] label, usz* out, Interp* interp) {
    long parsed = 0;
    if (!exact_integer_value_to_i64(value, &parsed)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", label, null);
    }
    if (parsed < 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", label, null);
    }
    *out = (usz)parsed;
    return null;
}

fn Value* ml_conv1d_validate(
    TensorVal* input,
    TensorVal* kernel,
    usz stride,
    usz padding,
    usz dilation,
    usz groups,
    MlConv1dSpec* out,
    Interp* interp
) {
    if (input == null || kernel == null || out == null) {
        return tensor_runtime_raise(interp, "ml/conv1d: tensor backing storage is missing");
    }
    *out = {};
    if (input.dtype != kernel.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/conv1d: tensor dtype mismatch", null);
    }
    if (!ml_conv_supported_dtype(input.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/conv1d: currently supports Float64 and Float32 tensors", null);
    }
    if (input.rank != 3 || kernel.rank != 3) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/conv1d: expected input [batch channels width] and kernel [out-channels in-channels-per-group kernel-width]", null);
    }

    usz batch = input.shape[0];
    usz in_channels = input.shape[1];
    usz width = input.shape[2];
    usz out_channels = kernel.shape[0];
    usz kernel_in_channels = kernel.shape[1];
    usz kernel_width = kernel.shape[2];
    if (batch == 0 || in_channels == 0 || width == 0 || out_channels == 0 || kernel_width == 0 || groups == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/conv1d: convolution dimensions must be non-empty", null);
    }
    if (in_channels % groups != 0 || out_channels % groups != 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/conv1d: channels must be divisible by groups", null);
    }

    usz in_channels_per_group = in_channels / groups;
    if (kernel_in_channels != in_channels_per_group) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/conv1d: kernel input channels must equal input channels per group", null);
    }

    usz kernel_tail = 0;
    if (!tensor_checked_mul_usz(kernel_width - 1, dilation, &kernel_tail)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/conv1d: effective kernel width is too large", null);
    }
    usz effective_kernel = kernel_tail + 1;
    if (effective_kernel < kernel_tail) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/conv1d: effective kernel width is too large", null);
    }
    usz padded_extra = 0;
    if (!tensor_checked_mul_usz(padding, 2, &padded_extra)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/conv1d: padded width is too large", null);
    }
    usz padded_width = width + padded_extra;
    if (padded_width < width) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/conv1d: padded width is too large", null);
    }
    if (padded_width < effective_kernel) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/conv1d: effective kernel does not fit padded input", null);
    }

    out.batch = batch;
    out.in_channels = in_channels;
    out.width = width;
    out.out_channels = out_channels;
    out.kernel_in_channels = kernel_in_channels;
    out.kernel_width = kernel_width;
    out.stride = stride;
    out.padding = padding;
    out.dilation = dilation;
    out.groups = groups;
    out.in_channels_per_group = in_channels_per_group;
    out.out_channels_per_group = out_channels / groups;
    out.out_width = ((padded_width - effective_kernel) / stride) + 1;
    return null;
}

fn void ml_conv1d_eval_f64(TensorVal* input, TensorVal* kernel, TensorVal* out, MlConv1dSpec* spec) {
    double* input_data = (double*)input.data;
    double* kernel_data = (double*)kernel.data;
    double* out_data = (double*)out.data;
    for (usz batch = 0; batch < spec.batch; batch++) {
        for (usz out_channel = 0; out_channel < spec.out_channels; out_channel++) {
            usz group = out_channel / spec.out_channels_per_group;
            usz input_channel_base = group * spec.in_channels_per_group;
            for (usz out_pos = 0; out_pos < spec.out_width; out_pos++) {
                double sum = 0.0;
                for (usz group_channel = 0; group_channel < spec.in_channels_per_group; group_channel++) {
                    usz input_channel = input_channel_base + group_channel;
                    for (usz kernel_pos = 0; kernel_pos < spec.kernel_width; kernel_pos++) {
                        usz source_base = out_pos * spec.stride + kernel_pos * spec.dilation;
                        if (source_base < spec.padding) continue;
                        usz input_pos = source_base - spec.padding;
                        if (input_pos >= spec.width) continue;
                        usz input_index = (batch * spec.in_channels + input_channel) * spec.width + input_pos;
                        usz kernel_index = (out_channel * spec.kernel_in_channels + group_channel) * spec.kernel_width + kernel_pos;
                        sum += input_data[input_index] * kernel_data[kernel_index];
                    }
                }
                usz out_index = (batch * spec.out_channels + out_channel) * spec.out_width + out_pos;
                out_data[out_index] = sum;
            }
        }
    }
}

fn void ml_conv1d_eval_f32(TensorVal* input, TensorVal* kernel, TensorVal* out, MlConv1dSpec* spec) {
    float* input_data = (float*)input.data;
    float* kernel_data = (float*)kernel.data;
    float* out_data = (float*)out.data;
    for (usz batch = 0; batch < spec.batch; batch++) {
        for (usz out_channel = 0; out_channel < spec.out_channels; out_channel++) {
            usz group = out_channel / spec.out_channels_per_group;
            usz input_channel_base = group * spec.in_channels_per_group;
            for (usz out_pos = 0; out_pos < spec.out_width; out_pos++) {
                float sum = 0.0f;
                for (usz group_channel = 0; group_channel < spec.in_channels_per_group; group_channel++) {
                    usz input_channel = input_channel_base + group_channel;
                    for (usz kernel_pos = 0; kernel_pos < spec.kernel_width; kernel_pos++) {
                        usz source_base = out_pos * spec.stride + kernel_pos * spec.dilation;
                        if (source_base < spec.padding) continue;
                        usz input_pos = source_base - spec.padding;
                        if (input_pos >= spec.width) continue;
                        usz input_index = (batch * spec.in_channels + input_channel) * spec.width + input_pos;
                        usz kernel_index = (out_channel * spec.kernel_in_channels + group_channel) * spec.kernel_width + kernel_pos;
                        sum += input_data[input_index] * kernel_data[kernel_index];
                    }
                }
                usz out_index = (batch * spec.out_channels + out_channel) * spec.out_width + out_pos;
                out_data[out_index] = sum;
            }
        }
    }
}

fn Value* ml_conv1d_vulkan_result(TensorVal* input, TensorVal* kernel, MlConv1dSpec* spec, usz[] result_shape, Interp* interp) {
    if (input.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/conv1d: Vulkan currently supports Float32 tensors", null);
    }
    if (!tensor_has_valid_device_storage(input, TENSOR_DEVICE_VULKAN) || !tensor_has_valid_device_storage(kernel, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/conv1d: Vulkan tensor backing storage is missing");
    }

    TensorVal* tensor = tensor_alloc_payload_base_checked(input.dtype, result_shape, false, false, "ml/conv1d: failed to allocate Vulkan result tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "ml/conv1d: failed to allocate Vulkan result tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_ml_conv1d_f32(
        input.device_handle,
        input.byte_len,
        kernel.device_handle,
        kernel.byte_len,
        spec.batch,
        spec.in_channels,
        spec.width,
        spec.out_channels,
        spec.kernel_width,
        spec.stride,
        spec.padding,
        spec.dilation,
        spec.groups,
        spec.out_width,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "ml/conv1d: Vulkan convolution kernel failed");
    }
    if (out_device == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/conv1d: Vulkan convolution kernel did not return result storage");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = &tensor_vulkan_device_finalizer;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/conv1d: failed to allocate Vulkan result tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
        return make_runtime_out_of_memory_error(interp, "Tensor: failed to register result destructor");
    }
    return result;
}

fn Value* ml_conv1d_try_vulkan(Value* input_value, Value* kernel_value, usz stride, usz padding, usz dilation, usz groups, Interp* interp) {
    bool touches_vulkan = tensor_expr_has_device(input_value, TENSOR_DEVICE_VULKAN) ||
        tensor_expr_has_device(kernel_value, TENSOR_DEVICE_VULKAN);
    if (!touches_vulkan) return null;

    TensorVal* input = null;
    TensorVal* kernel = null;
    Value* resolved_input_value = null;
    Value* resolved_kernel_value = null;
    Value* err = tensor_expr_resolve_vulkan_or_migrate(input_value, &input, &resolved_input_value, interp, "ml/conv1d: input tensor metadata is missing");
    if (err != null) return err;
    defer if (resolved_input_value != null && resolved_input_value != input_value) {
        boundary_cleanup_materialized_value(null, resolved_input_value, interp.current_scope);
    }
    err = tensor_expr_resolve_vulkan_or_migrate(kernel_value, &kernel, &resolved_kernel_value, interp, "ml/conv1d: kernel tensor metadata is missing");
    if (err != null) return err;
    defer if (resolved_kernel_value != null && resolved_kernel_value != kernel_value) {
        boundary_cleanup_materialized_value(null, resolved_kernel_value, interp.current_scope);
    }

    if (input == null || kernel == null) return tensor_runtime_raise(interp, "ml/conv1d: tensor metadata is missing");
    if (!tensor_dense_row_major(input) || !tensor_dense_row_major(kernel)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/conv1d: Vulkan currently supports dense row-major tensors", null);
    }

    MlConv1dSpec spec = {};
    err = ml_conv1d_validate(input, kernel, stride, padding, dilation, groups, &spec, interp);
    if (err != null) return err;

    usz[3] result_shape = { spec.batch, spec.out_channels, spec.out_width };
    return ml_conv1d_vulkan_result(input, kernel, &spec, result_shape[0:3], interp);
}

<*
@require interp != null
*>
fn Value* prim_ml_conv1d(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 6) return primitive_arity_raise(interp, "ml/conv1d", "6 arguments: input kernel stride padding dilation groups", args.len);
    if (!is_tensor(args[0]) || !is_tensor(args[1])) {
        return tensor_type_raise(interp, "ml/conv1d: expected Tensor input and Tensor kernel");
    }

    usz stride = 0;
    usz padding = 0;
    usz dilation = 0;
    usz groups = 0;
    Value* err = ml_conv_parse_positive(args[2], "ml/conv1d: stride must be a positive integer", &stride, interp);
    if (err != null) return err;
    err = ml_conv_parse_nonnegative(args[3], "ml/conv1d: padding must be a non-negative integer", &padding, interp);
    if (err != null) return err;
    err = ml_conv_parse_positive(args[4], "ml/conv1d: dilation must be a positive integer", &dilation, interp);
    if (err != null) return err;
    err = ml_conv_parse_positive(args[5], "ml/conv1d: groups must be a positive integer", &groups, interp);
    if (err != null) return err;

    Value* vulkan_result = ml_conv1d_try_vulkan(args[0], args[1], stride, padding, dilation, groups, interp);
    if (vulkan_result != null) return vulkan_result;

    TensorVal* input = null;
    TensorVal* kernel = null;
    Value* input_value = null;
    Value* kernel_value = null;
    err = tensor_expr_resolve_concrete_cpu(
        args[0],
        &input,
        &input_value,
        interp,
        "ml/conv1d: input tensor backing storage is missing",
        "ml/conv1d: backend kernel is not implemented for non-CPU tensors"
    );
    if (err != null) return err;
    defer if (input_value != args[0]) {
        boundary_cleanup_materialized_value(null, input_value, interp.current_scope);
    }
    err = tensor_expr_resolve_concrete_cpu(
        args[1],
        &kernel,
        &kernel_value,
        interp,
        "ml/conv1d: kernel tensor backing storage is missing",
        "ml/conv1d: backend kernel is not implemented for non-CPU tensors"
    );
    if (err != null) return err;
    defer if (kernel_value != args[1]) {
        boundary_cleanup_materialized_value(null, kernel_value, interp.current_scope);
    }

    if (!tensor_has_valid_storage(input) || !tensor_has_valid_storage(kernel)) {
        return tensor_runtime_raise(interp, "ml/conv1d: tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(input) || !tensor_dense_row_major(kernel)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/conv1d: currently supports dense row-major tensors", null);
    }

    MlConv1dSpec spec = {};
    err = ml_conv1d_validate(input, kernel, stride, padding, dilation, groups, &spec, interp);
    if (err != null) return err;

    usz[3] result_shape = { spec.batch, spec.out_channels, spec.out_width };
    Value* out_value = make_tensor_uninitialized_checked(interp, input.dtype, result_shape[0:3], "ml/conv1d: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/conv1d: result tensor backing storage is missing");
    }

    if (input.dtype == TENSOR_DTYPE_DOUBLE) {
        ml_conv1d_eval_f64(input, kernel, out_value.tensor_val, &spec);
    } else {
        ml_conv1d_eval_f32(input, kernel, out_value.tensor_val, &spec);
    }
    keep_out = true;
    return out_value;
}
