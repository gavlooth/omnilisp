module lisp;

import main;

struct MlPool2dSpec {
    usz batch;
    usz channels;
    usz height;
    usz width;
    usz kernel_height;
    usz kernel_width;
    usz stride_height;
    usz stride_width;
    usz padding_height;
    usz padding_width;
    usz out_height;
    usz out_width;
}

fn Value* ml_pool2d_axis_extent(
    usz input_size,
    usz kernel_size,
    usz stride,
    usz padding,
    char[] overflow_label,
    char[] padding_label,
    usz* out_extent,
    Interp* interp
) {
    usz padded_extra = 0;
    if (!tensor_checked_mul_usz(padding, 2, &padded_extra)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", overflow_label, null);
    }
    usz padded_size = input_size + padded_extra;
    if (padded_size < input_size) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", overflow_label, null);
    }
    if (padded_size < kernel_size) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml pool2d: pooling window does not fit padded input", null);
    }
    if (padding >= kernel_size) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", padding_label, null);
    }
    *out_extent = ((padded_size - kernel_size) / stride) + 1;
    return null;
}

fn Value* ml_pool2d_validate(
    TensorVal* input,
    usz kernel_height,
    usz kernel_width,
    usz stride_height,
    usz stride_width,
    usz padding_height,
    usz padding_width,
    MlPool2dSpec* out,
    Interp* interp
) {
    if (input == null || out == null) {
        return tensor_runtime_raise(interp, "ml pool2d: tensor backing storage is missing");
    }
    *out = {};
    if (!ml_conv_supported_dtype(input.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml pool2d: currently supports Float64 and Float32 tensors", null);
    }
    if (input.rank != 4) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml pool2d: expected input [batch channels height width]", null);
    }

    usz batch = input.shape[0];
    usz channels = input.shape[1];
    usz height = input.shape[2];
    usz width = input.shape[3];
    if (batch == 0 || channels == 0 || height == 0 || width == 0 || kernel_height == 0 || kernel_width == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml pool2d: pooling dimensions must be non-empty", null);
    }

    usz out_height = 0;
    Value* err = ml_pool2d_axis_extent(
        height,
        kernel_height,
        stride_height,
        padding_height,
        "ml pool2d: padded height is too large",
        "ml pool2d: padding-height must be smaller than window height",
        &out_height,
        interp
    );
    if (err != null) return err;
    usz out_width = 0;
    err = ml_pool2d_axis_extent(
        width,
        kernel_width,
        stride_width,
        padding_width,
        "ml pool2d: padded width is too large",
        "ml pool2d: padding-width must be smaller than window width",
        &out_width,
        interp
    );
    if (err != null) return err;

    out.batch = batch;
    out.channels = channels;
    out.height = height;
    out.width = width;
    out.kernel_height = kernel_height;
    out.kernel_width = kernel_width;
    out.stride_height = stride_height;
    out.stride_width = stride_width;
    out.padding_height = padding_height;
    out.padding_width = padding_width;
    out.out_height = out_height;
    out.out_width = out_width;
    return null;
}

fn void ml_pool2d_eval_f64(TensorVal* input, TensorVal* out, MlPool2dSpec* spec, bool average) {
    double* input_data = (double*)input.data;
    double* out_data = (double*)out.data;
    for (usz batch = 0; batch < spec.batch; batch++) {
        for (usz channel = 0; channel < spec.channels; channel++) {
            for (usz out_y = 0; out_y < spec.out_height; out_y++) {
                for (usz out_x = 0; out_x < spec.out_width; out_x++) {
                    double acc = 0.0;
                    usz valid_count = 0;
                    bool have_max = false;
                    for (usz kernel_y = 0; kernel_y < spec.kernel_height; kernel_y++) {
                        usz source_y_base = out_y * spec.stride_height + kernel_y;
                        if (source_y_base < spec.padding_height) continue;
                        usz input_y = source_y_base - spec.padding_height;
                        if (input_y >= spec.height) continue;
                        for (usz kernel_x = 0; kernel_x < spec.kernel_width; kernel_x++) {
                            usz source_x_base = out_x * spec.stride_width + kernel_x;
                            if (source_x_base < spec.padding_width) continue;
                            usz input_x = source_x_base - spec.padding_width;
                            if (input_x >= spec.width) continue;
                            usz input_index = ((batch * spec.channels + channel) * spec.height + input_y) * spec.width + input_x;
                            double value = input_data[input_index];
                            if (average) {
                                acc += value;
                                valid_count++;
                            } else if (!have_max || value > acc) {
                                acc = value;
                                have_max = true;
                            }
                        }
                    }
                    if (average) acc = valid_count == 0 ? 0.0 : acc / (double)valid_count;
                    usz out_index = ((batch * spec.channels + channel) * spec.out_height + out_y) * spec.out_width + out_x;
                    out_data[out_index] = acc;
                }
            }
        }
    }
}

fn void ml_pool2d_eval_f32(TensorVal* input, TensorVal* out, MlPool2dSpec* spec, bool average) {
    float* input_data = (float*)input.data;
    float* out_data = (float*)out.data;
    for (usz batch = 0; batch < spec.batch; batch++) {
        for (usz channel = 0; channel < spec.channels; channel++) {
            for (usz out_y = 0; out_y < spec.out_height; out_y++) {
                for (usz out_x = 0; out_x < spec.out_width; out_x++) {
                    float acc = 0.0f;
                    usz valid_count = 0;
                    bool have_max = false;
                    for (usz kernel_y = 0; kernel_y < spec.kernel_height; kernel_y++) {
                        usz source_y_base = out_y * spec.stride_height + kernel_y;
                        if (source_y_base < spec.padding_height) continue;
                        usz input_y = source_y_base - spec.padding_height;
                        if (input_y >= spec.height) continue;
                        for (usz kernel_x = 0; kernel_x < spec.kernel_width; kernel_x++) {
                            usz source_x_base = out_x * spec.stride_width + kernel_x;
                            if (source_x_base < spec.padding_width) continue;
                            usz input_x = source_x_base - spec.padding_width;
                            if (input_x >= spec.width) continue;
                            usz input_index = ((batch * spec.channels + channel) * spec.height + input_y) * spec.width + input_x;
                            float value = input_data[input_index];
                            if (average) {
                                acc += value;
                                valid_count++;
                            } else if (!have_max || value > acc) {
                                acc = value;
                                have_max = true;
                            }
                        }
                    }
                    if (average) acc = valid_count == 0 ? 0.0f : acc / (float)valid_count;
                    usz out_index = ((batch * spec.channels + channel) * spec.out_height + out_y) * spec.out_width + out_x;
                    out_data[out_index] = acc;
                }
            }
        }
    }
}

fn Value* ml_pool2d_vulkan_result(TensorVal* input, MlPool2dSpec* spec, usz[] result_shape, bool average, Interp* interp) {
    if (input.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml pool2d: Vulkan currently supports Float32 tensors", null);
    }
    if (!tensor_has_valid_device_storage(input, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml pool2d: Vulkan tensor backing storage is missing");
    }

    TensorVal* tensor = tensor_alloc_payload_base_checked(input.dtype, result_shape, false, false, "ml pool2d: failed to allocate Vulkan result tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "ml pool2d: failed to allocate Vulkan result tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_ml_pool2d_f32(
        input.device_handle,
        input.byte_len,
        spec.batch,
        spec.channels,
        spec.height,
        spec.width,
        spec.kernel_height,
        spec.kernel_width,
        spec.stride_height,
        spec.stride_width,
        spec.padding_height,
        spec.padding_width,
        spec.out_height,
        spec.out_width,
        average ? 1 : 0,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "ml pool2d: Vulkan pooling kernel failed");
    }
    if (out_device == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml pool2d: Vulkan pooling kernel did not return result storage");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = &tensor_vulkan_device_finalizer;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml pool2d: failed to allocate Vulkan result tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* ml_pool2d_try_vulkan(Value* input_value, usz kernel_height, usz kernel_width, usz stride_height, usz stride_width, usz padding_height, usz padding_width, bool average, Interp* interp) {
    if (!tensor_expr_has_device(input_value, TENSOR_DEVICE_VULKAN)) return null;

    TensorVal* input = null;
    Value* resolved_input_value = null;
    Value* err = tensor_expr_resolve_vulkan_or_migrate(input_value, &input, &resolved_input_value, interp, "ml pool2d: input tensor metadata is missing");
    if (err != null) return err;
    defer if (resolved_input_value != null && resolved_input_value != input_value) {
        boundary_cleanup_materialized_value(null, resolved_input_value, interp.current_scope);
    }

    if (input == null) return tensor_runtime_raise(interp, "ml pool2d: tensor metadata is missing");
    if (input.device != TENSOR_DEVICE_VULKAN) {
        return null;
    }
    if (!tensor_dense_row_major(input)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml pool2d: Vulkan currently supports dense row-major tensors", null);
    }

    MlPool2dSpec spec = {};
    err = ml_pool2d_validate(input, kernel_height, kernel_width, stride_height, stride_width, padding_height, padding_width, &spec, interp);
    if (err != null) return err;

    usz[4] result_shape = { spec.batch, spec.channels, spec.out_height, spec.out_width };
    return ml_pool2d_vulkan_result(input, &spec, result_shape[0:4], average, interp);
}

fn Value* prim_ml_pool2d_common(Value*[] args, Env* env, Interp* interp, bool average, char[] name) {
    if (args.len != 7) {
        return primitive_arity_raise(interp, name, "7 arguments: input window-height window-width stride-height stride-width padding-height padding-width", args.len);
    }
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "ml pool2d: expected Tensor input");

    usz kernel_height = 0;
    usz kernel_width = 0;
    usz stride_height = 0;
    usz stride_width = 0;
    usz padding_height = 0;
    usz padding_width = 0;
    Value* err = ml_conv_parse_positive(args[1], "ml pool2d: window-height must be a positive integer", &kernel_height, interp);
    if (err != null) return err;
    err = ml_conv_parse_positive(args[2], "ml pool2d: window-width must be a positive integer", &kernel_width, interp);
    if (err != null) return err;
    err = ml_conv_parse_positive(args[3], "ml pool2d: stride-height must be a positive integer", &stride_height, interp);
    if (err != null) return err;
    err = ml_conv_parse_positive(args[4], "ml pool2d: stride-width must be a positive integer", &stride_width, interp);
    if (err != null) return err;
    err = ml_conv_parse_nonnegative(args[5], "ml pool2d: padding-height must be a non-negative integer", &padding_height, interp);
    if (err != null) return err;
    err = ml_conv_parse_nonnegative(args[6], "ml pool2d: padding-width must be a non-negative integer", &padding_width, interp);
    if (err != null) return err;

    Value* vulkan_result = ml_pool2d_try_vulkan(args[0], kernel_height, kernel_width, stride_height, stride_width, padding_height, padding_width, average, interp);
    if (vulkan_result != null) return vulkan_result;

    TensorVal* input = null;
    Value* input_value = null;
    err = tensor_expr_resolve_concrete_cpu(args[0], &input, &input_value, interp, "ml pool2d: input tensor backing storage is missing", "ml pool2d: backend kernel is not implemented for non-CPU tensors");
    if (err != null) return err;
    defer if (input_value != args[0]) {
        boundary_cleanup_materialized_value(null, input_value, interp.current_scope);
    }

    if (!tensor_has_valid_storage(input)) return tensor_runtime_raise(interp, "ml pool2d: tensor backing storage is missing");
    if (!tensor_dense_row_major(input)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml pool2d: currently supports dense row-major tensors", null);
    }

    MlPool2dSpec spec = {};
    err = ml_pool2d_validate(input, kernel_height, kernel_width, stride_height, stride_width, padding_height, padding_width, &spec, interp);
    if (err != null) return err;

    usz[4] result_shape = { spec.batch, spec.channels, spec.out_height, spec.out_width };
    Value* out_value = make_tensor_uninitialized_checked(interp, input.dtype, result_shape[0:4], "ml pool2d: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml pool2d: result tensor backing storage is missing");
    }

    if (input.dtype == TENSOR_DTYPE_DOUBLE) {
        ml_pool2d_eval_f64(input, out_value.tensor_val, &spec, average);
    } else {
        ml_pool2d_eval_f32(input, out_value.tensor_val, &spec, average);
    }
    keep_out = true;
    (void)env;
    return out_value;
}

<*
@require interp != null
*>
fn Value* prim_ml_max_pool2d(Value*[] args, Env* env, Interp* interp) {
    return prim_ml_pool2d_common(args, env, interp, false, "ml/max-pool2d");
}

<*
@require interp != null
*>
fn Value* prim_ml_avg_pool2d(Value*[] args, Env* env, Interp* interp) {
    return prim_ml_pool2d_common(args, env, interp, true, "ml/avg-pool2d");
}
