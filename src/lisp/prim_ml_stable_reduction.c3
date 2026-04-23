module lisp;

import std::core::mem;
import main;

extern fn double ml_c_exp(double x) @extern("exp");
extern fn double ml_c_log(double x) @extern("log");

fn Value* ml_stable_exp_stats_cpu(
    TensorVal* source,
    usz* axes,
    usz axis_count,
    usz output_count,
    Interp* interp,
    char[] index_label,
    double** out_maxes,
    double** out_exp_sums
) {
    if (source == null || axes == null || out_maxes == null || out_exp_sums == null) {
        return tensor_runtime_raise(interp, index_label);
    }
    *out_maxes = null;
    *out_exp_sums = null;

    usz bytes = 0;
    if (!tensor_checked_mul_usz(output_count, double.sizeof, &bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml stable reduction: accumulator is too large", null);
    }
    double* maxes = (double*)mem::malloc(bytes);
    if (maxes == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml stable reduction: failed to allocate max accumulator", null);
    }
    bool keep_maxes = false;
    defer if (!keep_maxes) mem::free(maxes);

    double* exp_sums = (double*)mem::malloc(bytes);
    if (exp_sums == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml stable reduction: failed to allocate exp accumulator", null);
    }
    bool keep_exp_sums = false;
    defer if (!keep_exp_sums) mem::free(exp_sums);

    usz seen_bytes = 0;
    if (!tensor_checked_mul_usz(output_count, bool.sizeof, &seen_bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml stable reduction: accumulator flags are too large", null);
    }
    bool* seen = (bool*)mem::malloc(seen_bytes);
    if (seen == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml stable reduction: failed to allocate accumulator flags", null);
    }
    defer mem::free(seen);

    for (usz i = 0; i < output_count; i++) {
        maxes[i] = 0.0;
        exp_sums[i] = 0.0;
        seen[i] = false;
    }

    for (usz i = 0; i < source.element_count; i++) {
        usz result_index = 0;
        double value = 0.0;
        if (!ml_reduction_result_index(source, i, axes, axis_count, &result_index) ||
            result_index >= output_count ||
            !ml_reduction_read(source, i, &value)) {
            return tensor_runtime_raise(interp, index_label);
        }
        if (!seen[result_index] || value > maxes[result_index]) {
            maxes[result_index] = value;
            seen[result_index] = true;
        }
    }

    for (usz i = 0; i < source.element_count; i++) {
        usz result_index = 0;
        double value = 0.0;
        if (!ml_reduction_result_index(source, i, axes, axis_count, &result_index) ||
            result_index >= output_count ||
            !ml_reduction_read(source, i, &value)) {
            return tensor_runtime_raise(interp, index_label);
        }
        exp_sums[result_index] += ml_c_exp(value - maxes[result_index]);
    }

    keep_maxes = true;
    keep_exp_sums = true;
    *out_maxes = maxes;
    *out_exp_sums = exp_sums;
    return null;
}

<*
@require interp != null
*>
fn Value* prim_ml_logsumexp(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return primitive_arity_raise(interp, "ml/logsumexp", "2 arguments: input axes", args.len);
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "ml/logsumexp: expected Tensor input");

    TensorVal* source = null;
    Value* source_value = null;
    Value* err = tensor_expr_resolve_concrete_any_device(
        args[0],
        &source,
        &source_value,
        interp,
        "ml/logsumexp: tensor backing storage is missing"
    );
    if (err != null) return err;
    bool cleanup_source = source_value != args[0];
    defer if (cleanup_source) {
        boundary_cleanup_materialized_value(null, source_value, interp.current_scope);
    }

    if (source.device != TENSOR_DEVICE_CPU && source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/logsumexp: backend kernel is not implemented for this tensor placement", null);
    }
    if (source.device == TENSOR_DEVICE_CPU && !tensor_has_valid_storage(source)) {
        return tensor_runtime_raise(interp, "ml/logsumexp: tensor backing storage is missing");
    }
    if (source.device == TENSOR_DEVICE_VULKAN && !tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/logsumexp: Vulkan tensor backing storage is missing");
    }
    if (!ml_reduction_supported_dtype(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/logsumexp: currently supports Float64 and Float32 tensors", null);
    }
    if (source.device == TENSOR_DEVICE_VULKAN && source.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/logsumexp: Vulkan currently supports Float32 tensors", null);
    }

    usz* axes = null;
    usz axis_count = 0;
    err = ml_reduction_parse_axes(
        args[1],
        source,
        interp,
        "ml/logsumexp: axes must be an integer, array, or proper list of integers",
        "ml/logsumexp: axis out of range",
        "ml/logsumexp: duplicate reduction axis",
        "ml/logsumexp: expected at least one reduction axis",
        &axes,
        &axis_count
    );
    if (err != null) return err;
    defer mem::free(axes);

    usz* result_shape = null;
    usz result_rank = 0;
    usz reduction_count = 0;
    err = ml_reduction_result_shape(
        source,
        axes,
        axis_count,
        interp,
        "ml/logsumexp: result shape is too large",
        "ml/logsumexp: cannot reduce an empty axis",
        &result_shape,
        &result_rank,
        &reduction_count
    );
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        return ml_reduction_eval_vulkan(source, shape_slice, axes, axis_count, ML_REDUCTION_LOGSUMEXP, interp, "ml/logsumexp: failed to allocate Vulkan result tensor metadata", "ml/logsumexp: failed to allocate Vulkan result tensor value");
    }

    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "ml/logsumexp: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/logsumexp: result tensor backing storage is missing");
    }
    if (out_value.tensor_val.element_count == 0) {
        keep_out = true;
        return out_value;
    }

    double* maxes = null;
    double* exp_sums = null;
    err = ml_stable_exp_stats_cpu(
        source,
        axes,
        axis_count,
        out_value.tensor_val.element_count,
        interp,
        "ml/logsumexp: tensor index offset is invalid",
        &maxes,
        &exp_sums
    );
    if (err != null) return err;
    defer mem::free(maxes);
    defer mem::free(exp_sums);

    for (usz i = 0; i < out_value.tensor_val.element_count; i++) {
        exp_sums[i] = maxes[i] + ml_c_log(exp_sums[i]);
    }
    err = ml_reduction_write_result(out_value.tensor_val, exp_sums, interp, "ml/logsumexp: Float32 result is not representable");
    if (err != null) return err;
    keep_out = true;
    return out_value;
}

fn Value* ml_softmax_eval_vulkan(
    TensorVal* source,
    usz axis,
    Interp* interp
) {
    if (source == null) return tensor_runtime_raise(interp, "ml/softmax: Vulkan tensor metadata is missing");
    if (source.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/softmax: Vulkan currently supports Float32 tensors", null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/softmax: Vulkan tensor backing storage is missing");
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    TensorVal* tensor = tensor_alloc_payload_base_checked(source.dtype, shape_slice, false, false, "ml/softmax: failed to allocate Vulkan result tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "ml/softmax: failed to allocate Vulkan result tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_ml_softmax_f32(
        source.device_handle,
        source.byte_len,
        source.element_count,
        source.rank,
        axis,
        source.shape,
        source.strides,
        tensor.strides,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "ml/softmax: Vulkan softmax kernel failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/softmax: failed to allocate Vulkan result tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

<*
@require interp != null
*>
fn Value* prim_ml_softmax(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return primitive_arity_raise(interp, "ml/softmax", "2 arguments: input axis", args.len);
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "ml/softmax: expected Tensor input");

    TensorVal* source = null;
    Value* source_value = null;
    Value* err = tensor_expr_resolve_concrete_any_device(
        args[0],
        &source,
        &source_value,
        interp,
        "ml/softmax: tensor backing storage is missing"
    );
    if (err != null) return err;
    bool cleanup_source = source_value != args[0];
    defer if (cleanup_source) {
        boundary_cleanup_materialized_value(null, source_value, interp.current_scope);
    }

    if (source.device != TENSOR_DEVICE_CPU && source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/softmax: backend kernel is not implemented for this tensor placement", null);
    }
    if (source.device == TENSOR_DEVICE_CPU && !tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "ml/softmax: tensor backing storage is missing");
    if (source.device == TENSOR_DEVICE_VULKAN && !tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/softmax: Vulkan tensor backing storage is missing");
    }
    if (!ml_reduction_supported_dtype(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/softmax: currently supports Float64 and Float32 tensors", null);
    }
    if (source.device == TENSOR_DEVICE_VULKAN && source.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/softmax: Vulkan currently supports Float32 tensors", null);
    }

    usz axis = 0;
    err = ml_reduction_parse_axis_value(
        args[1],
        source,
        interp,
        "ml/softmax: axis must be an integer",
        "ml/softmax: axis out of range",
        "ml/softmax: duplicate reduction axis",
        null,
        0,
        &axis
    );
    if (err != null) return err;

    usz[1] axes = { axis };
    usz* result_shape = null;
    usz result_rank = 0;
    usz reduction_count = 0;
    err = ml_reduction_result_shape(
        source,
        axes[..],
        1,
        interp,
        "ml/softmax: result shape is too large",
        "ml/softmax: cannot reduce an empty axis",
        &result_shape,
        &result_rank,
        &reduction_count
    );
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        return ml_softmax_eval_vulkan(source, axis, interp);
    }

    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "ml/softmax: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/softmax: result tensor backing storage is missing");
    }
    if (out_value.tensor_val.element_count == 0) {
        keep_out = true;
        return out_value;
    }

    usz slice_count = result_rank == 0 ? (usz)1 : out_value.tensor_val.element_count / reduction_count;
    double* maxes = null;
    double* exp_sums = null;
    err = ml_stable_exp_stats_cpu(
        source,
        axes[..],
        1,
        slice_count,
        interp,
        "ml/softmax: tensor index offset is invalid",
        &maxes,
        &exp_sums
    );
    if (err != null) return err;
    defer mem::free(maxes);
    defer mem::free(exp_sums);

    usz bytes = 0;
    if (!tensor_checked_mul_usz(out_value.tensor_val.element_count, double.sizeof, &bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/softmax: output buffer is too large", null);
    }
    double* softmax_values = (double*)mem::malloc(bytes);
    if (softmax_values == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/softmax: failed to allocate output buffer", null);
    }
    defer mem::free(softmax_values);

    for (usz i = 0; i < source.element_count; i++) {
        usz result_index = 0;
        double value = 0.0;
        if (!ml_reduction_result_index(source, i, axes[..], 1, &result_index) ||
            !ml_reduction_read(source, i, &value) ||
            result_index >= slice_count) {
            return tensor_runtime_raise(interp, "ml/softmax: tensor index offset is invalid");
        }
        softmax_values[i] = ml_c_exp(value - maxes[result_index]) / exp_sums[result_index];
    }

    err = ml_reduction_write_result(out_value.tensor_val, softmax_values, interp, "ml/softmax: Float32 result is not representable");
    if (err != null) return err;
    keep_out = true;
    return out_value;
}

<*
@require interp != null
*>
fn Value* prim_ml_mean_squared_error(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return primitive_arity_raise(interp, "ml/mean-squared-error", "2 arguments: predictions targets", args.len);
    if (!is_tensor(args[0]) || !is_tensor(args[1])) {
        return tensor_type_raise(interp, "ml/mean-squared-error: expected Tensor predictions and Tensor targets");
    }

    bool touches_vulkan = tensor_expr_has_device(args[0], TENSOR_DEVICE_VULKAN) ||
        tensor_expr_has_device(args[1], TENSOR_DEVICE_VULKAN);

    TensorVal* predictions = null;
    Value* predictions_value = null;
    Value* err = touches_vulkan
        ? tensor_expr_resolve_vulkan_or_migrate(args[0], &predictions, &predictions_value, interp, "ml/mean-squared-error: predictions tensor backing storage is missing")
        : tensor_expr_resolve_concrete_any_device(args[0], &predictions, &predictions_value, interp, "ml/mean-squared-error: predictions tensor backing storage is missing");
    if (err != null) return err;
    bool cleanup_predictions = predictions_value != args[0];
    defer if (cleanup_predictions) {
        boundary_cleanup_materialized_value(null, predictions_value, interp.current_scope);
    }

    TensorVal* targets = null;
    Value* targets_value = null;
    err = touches_vulkan
        ? tensor_expr_resolve_vulkan_or_migrate(args[1], &targets, &targets_value, interp, "ml/mean-squared-error: targets tensor backing storage is missing")
        : tensor_expr_resolve_concrete_any_device(args[1], &targets, &targets_value, interp, "ml/mean-squared-error: targets tensor backing storage is missing");
    if (err != null) return err;
    bool cleanup_targets = targets_value != args[1];
    defer if (cleanup_targets) {
        boundary_cleanup_materialized_value(null, targets_value, interp.current_scope);
    }

    if (predictions.device != targets.device) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/mean-squared-error: predictions and targets must be on the same device", null);
    }
    if (predictions.device != TENSOR_DEVICE_CPU && predictions.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/mean-squared-error: backend kernel is not implemented for this tensor placement", null);
    }
    if (predictions.device == TENSOR_DEVICE_CPU && (!tensor_has_valid_storage(predictions) || !tensor_has_valid_storage(targets))) {
        return tensor_runtime_raise(interp, "ml/mean-squared-error: tensor backing storage is missing");
    }
    if (!ml_reduction_supported_dtype(predictions.dtype) || !ml_reduction_supported_dtype(targets.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/mean-squared-error: currently supports Float64 and Float32 tensors", null);
    }
    if (predictions.dtype != targets.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/mean-squared-error: tensor dtype mismatch", null);
    }
    if (predictions.element_count != targets.element_count || !tensor_same_shape(predictions, targets)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/mean-squared-error: predictions and targets must have the same shape", null);
    }
    if (predictions.element_count == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/mean-squared-error: cannot reduce empty tensors", null);
    }
    if (predictions.device == TENSOR_DEVICE_VULKAN) {
        return ml_mean_squared_error_eval_vulkan(predictions, targets, interp);
    }

    double total = 0.0;
    for (usz i = 0; i < predictions.element_count; i++) {
        double prediction = 0.0;
        double target = 0.0;
        if (!ml_reduction_read(predictions, i, &prediction) ||
            !ml_reduction_read(targets, i, &target)) {
            return tensor_runtime_raise(interp, "ml/mean-squared-error: tensor index offset is invalid");
        }
        double diff = prediction - target;
        total += diff * diff;
    }

    double[1] result = { total / (double)predictions.element_count };
    usz[1] scalar_shape = { 0 };
    Value* out_value = make_tensor_uninitialized_checked(interp, predictions.dtype, scalar_shape[0:0], "ml/mean-squared-error: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/mean-squared-error: result tensor backing storage is missing");
    }

    err = ml_reduction_write_result(out_value.tensor_val, result[..], interp, "ml/mean-squared-error: Float32 result is not representable");
    if (err != null) return err;
    keep_out = true;
    return out_value;
}

<*
@require interp != null
*>
fn Value* prim_ml_cross_entropy(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 3) return primitive_arity_raise(interp, "ml/cross-entropy", "3 arguments: logits targets axis", args.len);
    if (!is_tensor(args[0]) || !is_tensor(args[1])) {
        return tensor_type_raise(interp, "ml/cross-entropy: expected Tensor logits and Tensor targets");
    }

    bool touches_vulkan = tensor_expr_has_device(args[0], TENSOR_DEVICE_VULKAN) ||
        tensor_expr_has_device(args[1], TENSOR_DEVICE_VULKAN);

    TensorVal* logits = null;
    Value* logits_value = null;
    Value* err = touches_vulkan
        ? tensor_expr_resolve_vulkan_or_migrate(args[0], &logits, &logits_value, interp, "ml/cross-entropy: logits tensor backing storage is missing")
        : tensor_expr_resolve_concrete_any_device(args[0], &logits, &logits_value, interp, "ml/cross-entropy: logits tensor backing storage is missing");
    if (err != null) return err;
    bool cleanup_logits = logits_value != args[0];
    defer if (cleanup_logits) {
        boundary_cleanup_materialized_value(null, logits_value, interp.current_scope);
    }

    TensorVal* targets = null;
    Value* targets_value = null;
    err = touches_vulkan
        ? tensor_expr_resolve_vulkan_or_migrate(args[1], &targets, &targets_value, interp, "ml/cross-entropy: targets tensor backing storage is missing")
        : tensor_expr_resolve_concrete_any_device(args[1], &targets, &targets_value, interp, "ml/cross-entropy: targets tensor backing storage is missing");
    if (err != null) return err;
    bool cleanup_targets = targets_value != args[1];
    defer if (cleanup_targets) {
        boundary_cleanup_materialized_value(null, targets_value, interp.current_scope);
    }

    if (logits.device != targets.device) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/cross-entropy: logits and targets must be on the same device", null);
    }
    if (logits.device != TENSOR_DEVICE_CPU && logits.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/cross-entropy: backend kernel is not implemented for this tensor placement", null);
    }
    if (logits.device == TENSOR_DEVICE_CPU && (!tensor_has_valid_storage(logits) || !tensor_has_valid_storage(targets))) {
        return tensor_runtime_raise(interp, "ml/cross-entropy: tensor backing storage is missing");
    }
    if (!ml_reduction_supported_dtype(logits.dtype) || !ml_reduction_supported_dtype(targets.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/cross-entropy: currently supports Float64 and Float32 tensors", null);
    }
    if (logits.dtype != targets.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/cross-entropy: tensor dtype mismatch", null);
    }
    if (logits.element_count != targets.element_count || !tensor_same_shape(logits, targets)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/cross-entropy: logits and targets must have the same shape", null);
    }

    usz axis = 0;
    err = ml_reduction_parse_axis_value(
        args[2],
        logits,
        interp,
        "ml/cross-entropy: axis must be an integer",
        "ml/cross-entropy: axis out of range",
        "ml/cross-entropy: duplicate reduction axis",
        null,
        0,
        &axis
    );
    if (err != null) return err;

    usz[1] axes = { axis };
    usz* result_shape = null;
    usz result_rank = 0;
    usz reduction_count = 0;
    err = ml_reduction_result_shape(
        logits,
        axes[..],
        1,
        interp,
        "ml/cross-entropy: result shape is too large",
        "ml/cross-entropy: cannot reduce an empty class axis",
        &result_shape,
        &result_rank,
        &reduction_count
    );
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    if (logits.element_count == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/cross-entropy: cannot reduce empty tensors", null);
    }
    usz slice_count = result_rank == 0 ? (usz)1 : logits.element_count / reduction_count;
    if (slice_count == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/cross-entropy: cannot reduce empty tensors", null);
    }
    if (logits.device == TENSOR_DEVICE_VULKAN) {
        return ml_cross_entropy_eval_vulkan(logits, targets, axis, interp);
    }

    double* maxes = null;
    double* exp_sums = null;
    err = ml_stable_exp_stats_cpu(
        logits,
        axes[..],
        1,
        slice_count,
        interp,
        "ml/cross-entropy: tensor index offset is invalid",
        &maxes,
        &exp_sums
    );
    if (err != null) return err;
    defer mem::free(maxes);
    defer mem::free(exp_sums);

    usz bytes = 0;
    if (!tensor_checked_mul_usz(slice_count, double.sizeof, &bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/cross-entropy: accumulator is too large", null);
    }
    double* losses = (double*)mem::malloc(bytes);
    if (losses == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/cross-entropy: failed to allocate loss accumulator", null);
    }
    defer mem::free(losses);
    double* target_sums = (double*)mem::malloc(bytes);
    if (target_sums == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/cross-entropy: failed to allocate target accumulator", null);
    }
    defer mem::free(target_sums);
    for (usz i = 0; i < slice_count; i++) {
        losses[i] = 0.0;
        target_sums[i] = 0.0;
    }

    double tolerance = logits.dtype == TENSOR_DTYPE_FLOAT32 ? 0.0001 : 0.000000001;
    for (usz i = 0; i < logits.element_count; i++) {
        usz result_index = 0;
        double logit = 0.0;
        double target = 0.0;
        if (!ml_reduction_result_index(logits, i, axes[..], 1, &result_index) ||
            !ml_reduction_read(logits, i, &logit) ||
            !ml_reduction_read(targets, i, &target) ||
            result_index >= slice_count) {
            return tensor_runtime_raise(interp, "ml/cross-entropy: tensor index offset is invalid");
        }
        if (!conversion_double_is_finite(logit) ||
            !conversion_double_is_finite(target) ||
            target < -tolerance) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/cross-entropy: targets must be probability or one-hot tensors", null);
        }
        double log_prob = logit - maxes[result_index] - ml_c_log(exp_sums[result_index]);
        losses[result_index] += -target * log_prob;
        target_sums[result_index] += target;
    }

    double total = 0.0;
    for (usz i = 0; i < slice_count; i++) {
        if (tensor_c_fabs(target_sums[i] - 1.0) > tolerance) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/cross-entropy: target probabilities must sum to one along the class axis", null);
        }
        total += losses[i];
    }

    double[1] result = { total / (double)slice_count };
    usz[1] scalar_shape = { 0 };
    Value* out_value = make_tensor_uninitialized_checked(interp, logits.dtype, scalar_shape[0:0], "ml/cross-entropy: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/cross-entropy: result tensor backing storage is missing");
    }

    err = ml_reduction_write_result(out_value.tensor_val, result[..], interp, "ml/cross-entropy: Float32 result is not representable");
    if (err != null) return err;
    keep_out = true;
    return out_value;
}
