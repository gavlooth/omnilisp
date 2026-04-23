module lisp;

import main;

struct MlConv2dSpec {
    usz batch;
    usz in_channels;
    usz height;
    usz width;
    usz out_channels;
    usz kernel_in_channels;
    usz kernel_height;
    usz kernel_width;
    usz stride_height;
    usz stride_width;
    usz padding_height;
    usz padding_width;
    usz dilation_height;
    usz dilation_width;
    usz groups;
    usz in_channels_per_group;
    usz out_channels_per_group;
    usz out_height;
    usz out_width;
}

fn Value* ml_conv2d_axis_extent(
    usz input_size,
    usz kernel_size,
    usz stride,
    usz padding,
    usz dilation,
    char[] axis_label,
    usz* out_extent,
    Interp* interp
) {
    usz kernel_tail = 0;
    if (!tensor_checked_mul_usz(kernel_size - 1, dilation, &kernel_tail)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", axis_label, null);
    }
    usz effective_kernel = kernel_tail + 1;
    if (effective_kernel < kernel_tail) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", axis_label, null);
    }
    usz padded_extra = 0;
    if (!tensor_checked_mul_usz(padding, 2, &padded_extra)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", axis_label, null);
    }
    usz padded_size = input_size + padded_extra;
    if (padded_size < input_size) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", axis_label, null);
    }
    if (padded_size < effective_kernel) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/conv2d: effective kernel does not fit padded input", null);
    }
    *out_extent = ((padded_size - effective_kernel) / stride) + 1;
    return null;
}

fn Value* ml_conv2d_validate(
    TensorVal* input,
    TensorVal* kernel,
    usz stride_height,
    usz stride_width,
    usz padding_height,
    usz padding_width,
    usz dilation_height,
    usz dilation_width,
    usz groups,
    MlConv2dSpec* out,
    Interp* interp
) {
    if (input == null || kernel == null || out == null) {
        return tensor_runtime_raise(interp, "ml/conv2d: tensor backing storage is missing");
    }
    *out = {};
    if (input.dtype != kernel.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/conv2d: tensor dtype mismatch", null);
    }
    if (!ml_conv_supported_dtype(input.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/conv2d: currently supports Float64 and Float32 tensors", null);
    }
    if (input.rank != 4 || kernel.rank != 4) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/conv2d: expected input [batch channels height width] and kernel [out-channels in-channels-per-group kernel-height kernel-width]", null);
    }

    usz batch = input.shape[0];
    usz in_channels = input.shape[1];
    usz height = input.shape[2];
    usz width = input.shape[3];
    usz out_channels = kernel.shape[0];
    usz kernel_in_channels = kernel.shape[1];
    usz kernel_height = kernel.shape[2];
    usz kernel_width = kernel.shape[3];
    if (batch == 0 || in_channels == 0 || height == 0 || width == 0 ||
        out_channels == 0 || kernel_height == 0 || kernel_width == 0 || groups == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/conv2d: convolution dimensions must be non-empty", null);
    }
    if (in_channels % groups != 0 || out_channels % groups != 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/conv2d: channels must be divisible by groups", null);
    }

    usz in_channels_per_group = in_channels / groups;
    if (kernel_in_channels != in_channels_per_group) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/conv2d: kernel input channels must equal input channels per group", null);
    }
    usz out_height = 0;
    Value* err = ml_conv2d_axis_extent(
        height,
        kernel_height,
        stride_height,
        padding_height,
        dilation_height,
        "ml/conv2d: effective kernel height or padded height is too large",
        &out_height,
        interp
    );
    if (err != null) return err;
    usz out_width = 0;
    err = ml_conv2d_axis_extent(
        width,
        kernel_width,
        stride_width,
        padding_width,
        dilation_width,
        "ml/conv2d: effective kernel width or padded width is too large",
        &out_width,
        interp
    );
    if (err != null) return err;

    out.batch = batch;
    out.in_channels = in_channels;
    out.height = height;
    out.width = width;
    out.out_channels = out_channels;
    out.kernel_in_channels = kernel_in_channels;
    out.kernel_height = kernel_height;
    out.kernel_width = kernel_width;
    out.stride_height = stride_height;
    out.stride_width = stride_width;
    out.padding_height = padding_height;
    out.padding_width = padding_width;
    out.dilation_height = dilation_height;
    out.dilation_width = dilation_width;
    out.groups = groups;
    out.in_channels_per_group = in_channels_per_group;
    out.out_channels_per_group = out_channels / groups;
    out.out_height = out_height;
    out.out_width = out_width;
    return null;
}

fn void ml_conv2d_eval_f64(TensorVal* input, TensorVal* kernel, TensorVal* out, MlConv2dSpec* spec) {
    double* input_data = (double*)input.data;
    double* kernel_data = (double*)kernel.data;
    double* out_data = (double*)out.data;
    for (usz batch = 0; batch < spec.batch; batch++) {
        for (usz out_channel = 0; out_channel < spec.out_channels; out_channel++) {
            usz group = out_channel / spec.out_channels_per_group;
            usz input_channel_base = group * spec.in_channels_per_group;
            for (usz out_y = 0; out_y < spec.out_height; out_y++) {
                for (usz out_x = 0; out_x < spec.out_width; out_x++) {
                    double sum = 0.0;
                    for (usz group_channel = 0; group_channel < spec.in_channels_per_group; group_channel++) {
                        usz input_channel = input_channel_base + group_channel;
                        for (usz kernel_y = 0; kernel_y < spec.kernel_height; kernel_y++) {
                            usz source_y_base = out_y * spec.stride_height + kernel_y * spec.dilation_height;
                            if (source_y_base < spec.padding_height) continue;
                            usz input_y = source_y_base - spec.padding_height;
                            if (input_y >= spec.height) continue;
                            for (usz kernel_x = 0; kernel_x < spec.kernel_width; kernel_x++) {
                                usz source_x_base = out_x * spec.stride_width + kernel_x * spec.dilation_width;
                                if (source_x_base < spec.padding_width) continue;
                                usz input_x = source_x_base - spec.padding_width;
                                if (input_x >= spec.width) continue;
                                usz input_index = ((batch * spec.in_channels + input_channel) * spec.height + input_y) * spec.width + input_x;
                                usz kernel_index = ((out_channel * spec.kernel_in_channels + group_channel) * spec.kernel_height + kernel_y) * spec.kernel_width + kernel_x;
                                sum += input_data[input_index] * kernel_data[kernel_index];
                            }
                        }
                    }
                    usz out_index = ((batch * spec.out_channels + out_channel) * spec.out_height + out_y) * spec.out_width + out_x;
                    out_data[out_index] = sum;
                }
            }
        }
    }
}

fn void ml_conv2d_eval_f32(TensorVal* input, TensorVal* kernel, TensorVal* out, MlConv2dSpec* spec) {
    float* input_data = (float*)input.data;
    float* kernel_data = (float*)kernel.data;
    float* out_data = (float*)out.data;
    for (usz batch = 0; batch < spec.batch; batch++) {
        for (usz out_channel = 0; out_channel < spec.out_channels; out_channel++) {
            usz group = out_channel / spec.out_channels_per_group;
            usz input_channel_base = group * spec.in_channels_per_group;
            for (usz out_y = 0; out_y < spec.out_height; out_y++) {
                for (usz out_x = 0; out_x < spec.out_width; out_x++) {
                    float sum = 0.0f;
                    for (usz group_channel = 0; group_channel < spec.in_channels_per_group; group_channel++) {
                        usz input_channel = input_channel_base + group_channel;
                        for (usz kernel_y = 0; kernel_y < spec.kernel_height; kernel_y++) {
                            usz source_y_base = out_y * spec.stride_height + kernel_y * spec.dilation_height;
                            if (source_y_base < spec.padding_height) continue;
                            usz input_y = source_y_base - spec.padding_height;
                            if (input_y >= spec.height) continue;
                            for (usz kernel_x = 0; kernel_x < spec.kernel_width; kernel_x++) {
                                usz source_x_base = out_x * spec.stride_width + kernel_x * spec.dilation_width;
                                if (source_x_base < spec.padding_width) continue;
                                usz input_x = source_x_base - spec.padding_width;
                                if (input_x >= spec.width) continue;
                                usz input_index = ((batch * spec.in_channels + input_channel) * spec.height + input_y) * spec.width + input_x;
                                usz kernel_index = ((out_channel * spec.kernel_in_channels + group_channel) * spec.kernel_height + kernel_y) * spec.kernel_width + kernel_x;
                                sum += input_data[input_index] * kernel_data[kernel_index];
                            }
                        }
                    }
                    usz out_index = ((batch * spec.out_channels + out_channel) * spec.out_height + out_y) * spec.out_width + out_x;
                    out_data[out_index] = sum;
                }
            }
        }
    }
}

fn Value* ml_conv2d_vulkan_result(TensorVal* input, TensorVal* kernel, MlConv2dSpec* spec, usz[] result_shape, Interp* interp) {
    if (input.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/conv2d: Vulkan currently supports Float32 tensors", null);
    }
    if (!tensor_has_valid_device_storage(input, TENSOR_DEVICE_VULKAN) || !tensor_has_valid_device_storage(kernel, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/conv2d: Vulkan tensor backing storage is missing");
    }

    TensorVal* tensor = tensor_alloc_payload_base_checked(input.dtype, result_shape, false, false, "ml/conv2d: failed to allocate Vulkan result tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "ml/conv2d: failed to allocate Vulkan result tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_ml_conv2d_f32(
        input.device_handle,
        input.byte_len,
        kernel.device_handle,
        kernel.byte_len,
        spec.batch,
        spec.in_channels,
        spec.height,
        spec.width,
        spec.out_channels,
        spec.kernel_height,
        spec.kernel_width,
        spec.stride_height,
        spec.stride_width,
        spec.padding_height,
        spec.padding_width,
        spec.dilation_height,
        spec.dilation_width,
        spec.groups,
        spec.out_height,
        spec.out_width,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "ml/conv2d: Vulkan convolution kernel failed");
    }
    if (out_device == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/conv2d: Vulkan convolution kernel did not return result storage");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = &tensor_vulkan_device_finalizer;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/conv2d: failed to allocate Vulkan result tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* ml_conv2d_try_vulkan(Value* input_value, Value* kernel_value, usz stride_height, usz stride_width, usz padding_height, usz padding_width, usz dilation_height, usz dilation_width, usz groups, Interp* interp) {
    bool touches_vulkan = tensor_expr_has_device(input_value, TENSOR_DEVICE_VULKAN) ||
        tensor_expr_has_device(kernel_value, TENSOR_DEVICE_VULKAN);
    if (!touches_vulkan) return null;

    TensorVal* input = null;
    TensorVal* kernel = null;
    Value* resolved_input_value = null;
    Value* resolved_kernel_value = null;
    Value* err = tensor_expr_resolve_vulkan_or_migrate(input_value, &input, &resolved_input_value, interp, "ml/conv2d: input tensor metadata is missing");
    if (err != null) return err;
    defer if (resolved_input_value != null && resolved_input_value != input_value) {
        boundary_cleanup_materialized_value(null, resolved_input_value, interp.current_scope);
    }
    err = tensor_expr_resolve_vulkan_or_migrate(kernel_value, &kernel, &resolved_kernel_value, interp, "ml/conv2d: kernel tensor metadata is missing");
    if (err != null) return err;
    defer if (resolved_kernel_value != null && resolved_kernel_value != kernel_value) {
        boundary_cleanup_materialized_value(null, resolved_kernel_value, interp.current_scope);
    }

    if (input == null || kernel == null) return tensor_runtime_raise(interp, "ml/conv2d: tensor metadata is missing");
    if (!tensor_dense_row_major(input) || !tensor_dense_row_major(kernel)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/conv2d: Vulkan currently supports dense row-major tensors", null);
    }

    MlConv2dSpec spec = {};
    err = ml_conv2d_validate(input, kernel, stride_height, stride_width, padding_height, padding_width, dilation_height, dilation_width, groups, &spec, interp);
    if (err != null) return err;

    usz[4] result_shape = { spec.batch, spec.out_channels, spec.out_height, spec.out_width };
    return ml_conv2d_vulkan_result(input, kernel, &spec, result_shape[0:4], interp);
}

<*
@require interp != null
*>
fn Value* prim_ml_conv2d(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 9) {
        return primitive_arity_raise(interp, "ml/conv2d", "9 arguments: input kernel stride-height stride-width padding-height padding-width dilation-height dilation-width groups", args.len);
    }
    if (!is_tensor(args[0]) || !is_tensor(args[1])) {
        return tensor_type_raise(interp, "ml/conv2d: expected Tensor input and Tensor kernel");
    }

    usz stride_height = 0;
    usz stride_width = 0;
    usz padding_height = 0;
    usz padding_width = 0;
    usz dilation_height = 0;
    usz dilation_width = 0;
    usz groups = 0;
    Value* err = ml_conv_parse_positive(args[2], "ml/conv2d: stride-height must be a positive integer", &stride_height, interp);
    if (err != null) return err;
    err = ml_conv_parse_positive(args[3], "ml/conv2d: stride-width must be a positive integer", &stride_width, interp);
    if (err != null) return err;
    err = ml_conv_parse_nonnegative(args[4], "ml/conv2d: padding-height must be a non-negative integer", &padding_height, interp);
    if (err != null) return err;
    err = ml_conv_parse_nonnegative(args[5], "ml/conv2d: padding-width must be a non-negative integer", &padding_width, interp);
    if (err != null) return err;
    err = ml_conv_parse_positive(args[6], "ml/conv2d: dilation-height must be a positive integer", &dilation_height, interp);
    if (err != null) return err;
    err = ml_conv_parse_positive(args[7], "ml/conv2d: dilation-width must be a positive integer", &dilation_width, interp);
    if (err != null) return err;
    err = ml_conv_parse_positive(args[8], "ml/conv2d: groups must be a positive integer", &groups, interp);
    if (err != null) return err;

    Value* vulkan_result = ml_conv2d_try_vulkan(args[0], args[1], stride_height, stride_width, padding_height, padding_width, dilation_height, dilation_width, groups, interp);
    if (vulkan_result != null) return vulkan_result;

    TensorVal* input = null;
    TensorVal* kernel = null;
    Value* input_value = null;
    Value* kernel_value = null;
    err = tensor_expr_resolve_concrete_cpu(args[0], &input, &input_value, interp, "ml/conv2d: input tensor backing storage is missing", "ml/conv2d: backend kernel is not implemented for non-CPU tensors");
    if (err != null) return err;
    defer if (input_value != args[0]) {
        boundary_cleanup_materialized_value(null, input_value, interp.current_scope);
    }
    err = tensor_expr_resolve_concrete_cpu(args[1], &kernel, &kernel_value, interp, "ml/conv2d: kernel tensor backing storage is missing", "ml/conv2d: backend kernel is not implemented for non-CPU tensors");
    if (err != null) return err;
    defer if (kernel_value != args[1]) {
        boundary_cleanup_materialized_value(null, kernel_value, interp.current_scope);
    }

    if (!tensor_has_valid_storage(input) || !tensor_has_valid_storage(kernel)) {
        return tensor_runtime_raise(interp, "ml/conv2d: tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(input) || !tensor_dense_row_major(kernel)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/conv2d: currently supports dense row-major tensors", null);
    }

    MlConv2dSpec spec = {};
    err = ml_conv2d_validate(input, kernel, stride_height, stride_width, padding_height, padding_width, dilation_height, dilation_width, groups, &spec, interp);
    if (err != null) return err;

    usz[4] result_shape = { spec.batch, spec.out_channels, spec.out_height, spec.out_width };
    Value* out_value = make_tensor_uninitialized_checked(interp, input.dtype, result_shape[0:4], "ml/conv2d: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/conv2d: result tensor backing storage is missing");
    }

    if (input.dtype == TENSOR_DTYPE_DOUBLE) {
        ml_conv2d_eval_f64(input, kernel, out_value.tensor_val, &spec);
    } else {
        ml_conv2d_eval_f32(input, kernel, out_value.tensor_val, &spec);
    }
    keep_out = true;
    return out_value;
}
