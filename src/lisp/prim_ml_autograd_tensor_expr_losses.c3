module lisp;

import std::core::mem;

fn Value* ml_grad_tensor_mse(Value* spec, Interp* interp) {
    Value* prediction_arg = ml_grad_required_tensor(spec, interp, "prediction");
    if (prediction_arg == null || prediction_arg.tag == ERROR) return prediction_arg;
    Value* targets_arg = ml_grad_required_tensor(spec, interp, "targets");
    if (targets_arg == null || targets_arg.tag == ERROR) return targets_arg;
    Value* wrt_arg = ml_grad_required_tensor(spec, interp, "wrt");
    if (wrt_arg == null || wrt_arg.tag == ERROR) return wrt_arg;
    Value* vulkan_result = ml_grad_tensor_mse_try_vulkan(prediction_arg, targets_arg, wrt_arg, interp);
    if (vulkan_result != null) return vulkan_result;
    Value* err = ml_grad_expr_reject_device_backward(prediction_arg, targets_arg, wrt_arg, interp);
    if (err != null) return err;
    err = ml_grad_expr_validate_wrt_leaf(prediction_arg, wrt_arg, interp);
    if (err != null) return err;

    TensorVal* prediction = null;
    err = ml_grad_expr_resolve_dense_cpu(prediction_arg, &prediction, interp, "ml/grad: prediction tensor backing storage is missing");
    if (err != null) return err;
    TensorVal* targets = null;
    err = ml_grad_expr_resolve_dense_cpu(targets_arg, &targets, interp, "ml/grad: target tensor backing storage is missing");
    if (err != null) return err;
    TensorVal* wrt = null;
    err = ml_grad_expr_resolve_dense_cpu(wrt_arg, &wrt, interp, "ml/grad: wrt tensor backing storage is missing");
    if (err != null) return err;

    if (prediction.dtype != targets.dtype || prediction.dtype != wrt.dtype ||
        (prediction.dtype != TENSOR_DTYPE_DOUBLE && prediction.dtype != TENSOR_DTYPE_FLOAT32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/grad: Tensor expression gradients require matching Float64 or Float32 tensors", null);
    }
    if (prediction.element_count == 0 || !tensor_same_shape(prediction, targets)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: prediction and targets must have the same non-empty shape", null);
    }

    double* upstream = null;
    err = ml_grad_expr_buffer(interp, prediction.element_count, &upstream, "ml/grad: failed to allocate Tensor expression upstream");
    if (err != null) return err;
    defer mem::free(upstream);
    double* wrt_grad = null;
    err = ml_grad_expr_buffer(interp, wrt.element_count, &wrt_grad, "ml/grad: failed to allocate Tensor expression gradient");
    if (err != null) return err;
    defer mem::free(wrt_grad);

    double loss = 0.0;
    double inv_count = 1.0 / (double)prediction.element_count;
    for (usz i = 0; i < prediction.element_count; i++) {
        double y = 0.0;
        double target = 0.0;
        if (!ml_reduction_read(prediction, i, &y) || !ml_reduction_read(targets, i, &target)) {
            return tensor_runtime_raise(interp, "ml/grad: Tensor expression index is invalid");
        }
        double diff = y - target;
        loss += diff * diff;
        upstream[i] = 2.0 * diff * inv_count;
    }
    loss *= inv_count;

    err = ml_grad_expr_accumulate_to_wrt(prediction_arg, wrt_arg, upstream, wrt_grad, interp);
    if (err != null) return err;
    return ml_grad_tensor_result(interp, "tensor-mean-squared-error", prediction.dtype, loss, prediction_arg, wrt, wrt_grad, false);
}

fn Value* ml_grad_tensor_softmax_ce_try_vulkan(Value* logits_arg, Value* targets_arg, Value* wrt_arg, Value* spec, Interp* interp) {
    bool touches_vulkan = ml_grad_touches_vulkan(logits_arg) ||
        ml_grad_touches_vulkan(targets_arg) ||
        ml_grad_touches_vulkan(wrt_arg);
    if (!touches_vulkan) return null;
    if (tensor_expr_has_non_vulkan_device(logits_arg) ||
        tensor_expr_has_non_vulkan_device(wrt_arg)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward operands must remain Vulkan-placed", null);
    }

    TensorVal* logits = logits_arg.tensor_val;
    TensorVal* wrt = wrt_arg.tensor_val;
    if (logits == null || wrt == null ||
        logits.kind != TENSOR_PAYLOAD_CONCRETE ||
        wrt.kind != TENSOR_PAYLOAD_CONCRETE ||
        logits.device != TENSOR_DEVICE_VULKAN ||
        wrt.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward operands must remain Vulkan-placed", null);
    }
    if (logits.dtype != TENSOR_DTYPE_FLOAT32 || wrt.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan softmax-cross-entropy backward currently supports dense row-major Float32 tensors", null);
    }

    Value* targets_value = null;
    TensorVal* targets = null;
    Value* err = tensor_expr_resolve_vulkan_or_migrate(targets_arg, &targets, &targets_value, interp, "ml/grad: Vulkan softmax-CE backward target tensor backing storage is missing");
    if (err != null) return err;
    bool cleanup_targets = targets_value != targets_arg;
    defer if (cleanup_targets) {
        boundary_cleanup_materialized_value(null, targets_value, interp.current_scope);
    }
    if (targets == null || targets.kind != TENSOR_PAYLOAD_CONCRETE || targets.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward target must be a concrete Vulkan Tensor", null);
    }
    if (targets.dtype != TENSOR_DTYPE_FLOAT32 ||
        logits.dtype != targets.dtype || targets.dtype != wrt.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan softmax-cross-entropy backward currently supports dense row-major Float32 tensors", null);
    }
    if (!tensor_dense_row_major(logits) || !tensor_dense_row_major(targets) || !tensor_dense_row_major(wrt)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan softmax-cross-entropy backward currently supports dense row-major tensors", null);
    }
    if (logits.element_count == 0 || !tensor_same_shape(logits, targets) || !tensor_map_operand_matches_shape(wrt, logits.shape[0:logits.rank])) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: Vulkan softmax-cross-entropy backward wrt tensor must match or broadcast to logits shape", null);
    }
    if (!tensor_has_valid_device_storage(logits, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(targets, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(wrt, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/grad: Vulkan tensor backing storage is missing");
    }

    Value* axis_arg = nn_dict_get(spec, interp, "axis");
    if (axis_arg == null) return ml_grad_invalid(interp, "ml/grad: tensor-softmax-cross-entropy requires axis");
    usz axis = 0;
    err = ml_reduction_parse_axis_value(
        axis_arg,
        logits,
        interp,
        "ml/grad: axis must be an integer",
        "ml/grad: axis out of range",
        "ml/grad: duplicate reduction axis",
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
        "ml/grad: softmax result shape is too large",
        "ml/grad: cannot reduce an empty class axis",
        &result_shape,
        &result_rank,
        &reduction_count
    );
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);
    usz slice_count = result_rank == 0 ? (usz)1 : logits.element_count / reduction_count;
    if (slice_count == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: cannot reduce empty tensors", null);
    }

    Value* loss = ml_cross_entropy_eval_vulkan(logits, targets, axis, interp);
    if (loss == null || loss.tag == ERROR) return loss;
    Value* softmax = ml_softmax_eval_vulkan(logits, axis, interp);
    if (softmax == null || softmax.tag == ERROR) return softmax;

    Value* diff = ml_grad_vulkan_binary_map(
        interp,
        "-",
        softmax,
        softmax.tensor_val,
        targets_value,
        targets,
        logits.dtype,
        logits.shape[0:logits.rank]
    );
    if (diff == null || diff.tag == ERROR) return diff;
    double scale = 1.0 / (double)slice_count;
    Value* scale_value = logits.dtype == TENSOR_DTYPE_DOUBLE ? make_double(interp, scale) : make_float32(interp, (float)scale);
    if (scale_value == null) return nn_oom(interp);
    Value* grad = ml_grad_vulkan_binary_map(
        interp,
        "*",
        diff,
        diff.tensor_val,
        scale_value,
        null,
        logits.dtype,
        logits.shape[0:logits.rank]
    );
    if (grad == null || grad.tag == ERROR) return grad;

    if (!ml_grad_expr_same_tensor(logits_arg, wrt_arg)) {
        if (!tensor_concrete_has_map_provenance(logits)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan tensor-expression backward currently supports wrt equal to logits or same-shape map provenance", null);
        }
        grad = ml_grad_vulkan_map_backward_to_wrt(logits_arg, wrt_arg, grad, interp);
        if (grad == null || grad.tag == ERROR) return grad;
    }

    return ml_grad_tensor_value_result(interp, "tensor-softmax-cross-entropy", logits.dtype, "vulkan", loss, softmax, grad, true);
}

fn Value* ml_grad_tensor_softmax_cross_entropy(Value* spec, Interp* interp) {
    Value* logits_arg = ml_grad_required_tensor(spec, interp, "logits");
    if (logits_arg == null || logits_arg.tag == ERROR) return logits_arg;
    Value* targets_arg = ml_grad_required_tensor(spec, interp, "targets");
    if (targets_arg == null || targets_arg.tag == ERROR) return targets_arg;
    Value* wrt_arg = ml_grad_required_tensor(spec, interp, "wrt");
    if (wrt_arg == null || wrt_arg.tag == ERROR) return wrt_arg;
    Value* vulkan_result = ml_grad_tensor_softmax_ce_try_vulkan(logits_arg, targets_arg, wrt_arg, spec, interp);
    if (vulkan_result != null) return vulkan_result;
    Value* err = ml_grad_expr_reject_device_backward(logits_arg, targets_arg, wrt_arg, interp);
    if (err != null) return err;
    err = ml_grad_expr_validate_wrt_leaf(logits_arg, wrt_arg, interp);
    if (err != null) return err;

    TensorVal* logits = null;
    err = ml_grad_expr_resolve_dense_cpu(logits_arg, &logits, interp, "ml/grad: logits tensor backing storage is missing");
    if (err != null) return err;
    TensorVal* targets = null;
    err = ml_grad_expr_resolve_dense_cpu(targets_arg, &targets, interp, "ml/grad: target tensor backing storage is missing");
    if (err != null) return err;
    TensorVal* wrt = null;
    err = ml_grad_expr_resolve_dense_cpu(wrt_arg, &wrt, interp, "ml/grad: wrt tensor backing storage is missing");
    if (err != null) return err;

    if (logits.dtype != targets.dtype || logits.dtype != wrt.dtype ||
        (logits.dtype != TENSOR_DTYPE_DOUBLE && logits.dtype != TENSOR_DTYPE_FLOAT32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/grad: Tensor softmax gradients require matching Float64 or Float32 tensors", null);
    }
    if (logits.element_count == 0 || !tensor_same_shape(logits, targets)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: logits and targets must have the same non-empty shape", null);
    }

    Value* axis_arg = nn_dict_get(spec, interp, "axis");
    if (axis_arg == null) return ml_grad_invalid(interp, "ml/grad: tensor-softmax-cross-entropy requires axis");
    usz axis = 0;
    err = ml_reduction_parse_axis_value(
        axis_arg,
        logits,
        interp,
        "ml/grad: axis must be an integer",
        "ml/grad: axis out of range",
        "ml/grad: duplicate reduction axis",
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
        "ml/grad: softmax result shape is too large",
        "ml/grad: cannot reduce an empty class axis",
        &result_shape,
        &result_rank,
        &reduction_count
    );
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);
    usz slice_count = result_rank == 0 ? (usz)1 : logits.element_count / reduction_count;
    if (slice_count == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: cannot reduce empty tensors", null);
    }

    double* maxes = null;
    double* exp_sums = null;
    err = ml_stable_exp_stats_cpu(logits, axes[..], 1, slice_count, interp, "ml/grad: softmax tensor index is invalid", &maxes, &exp_sums);
    if (err != null) return err;
    defer mem::free(maxes);
    defer mem::free(exp_sums);
    for (usz i = 0; i < slice_count; i++) {
        if (!conversion_double_is_finite(maxes[i]) ||
            !conversion_double_is_finite(exp_sums[i]) ||
            !(exp_sums[i] > 0.0)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/grad: softmax-cross-entropy backward encountered invalid softmax denominator", null);
        }
    }

    double* softmax_values = null;
    err = ml_grad_expr_buffer(interp, logits.element_count, &softmax_values, "ml/grad: failed to allocate softmax output");
    if (err != null) return err;
    defer mem::free(softmax_values);
    double* upstream = null;
    err = ml_grad_expr_buffer(interp, logits.element_count, &upstream, "ml/grad: failed to allocate softmax upstream");
    if (err != null) return err;
    defer mem::free(upstream);
    double* target_sums = null;
    err = ml_grad_expr_buffer(interp, slice_count, &target_sums, "ml/grad: failed to allocate target accumulator");
    if (err != null) return err;
    defer mem::free(target_sums);
    double* wrt_grad = null;
    err = ml_grad_expr_buffer(interp, wrt.element_count, &wrt_grad, "ml/grad: failed to allocate Tensor expression gradient");
    if (err != null) return err;
    defer mem::free(wrt_grad);

    double tolerance = logits.dtype == TENSOR_DTYPE_FLOAT32 ? 0.0001 : 0.000000001;
    double loss = 0.0;
    for (usz i = 0; i < logits.element_count; i++) {
        usz result_index = 0;
        double logit = 0.0;
        double target = 0.0;
        if (!ml_reduction_result_index(logits, i, axes[..], 1, &result_index) ||
            !ml_reduction_read(logits, i, &logit) ||
            !ml_reduction_read(targets, i, &target) ||
            result_index >= slice_count) {
            return tensor_runtime_raise(interp, "ml/grad: softmax tensor index is invalid");
        }
        if (!conversion_double_is_finite(logit) || !conversion_double_is_finite(target) || target < -tolerance) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/grad: targets must be probability or one-hot tensors", null);
        }
        double softmax = ml_c_exp(logit - maxes[result_index]) / exp_sums[result_index];
        double log_prob = logit - maxes[result_index] - ml_c_log(exp_sums[result_index]);
        softmax_values[i] = softmax;
        upstream[i] = (softmax - target) / (double)slice_count;
        loss += -target * log_prob;
        target_sums[result_index] += target;
    }
    for (usz i = 0; i < slice_count; i++) {
        if (tensor_c_fabs(target_sums[i] - 1.0) > tolerance) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/grad: target probabilities must sum to one along the class axis", null);
        }
    }
    loss /= (double)slice_count;

    Value* output = ml_grad_make_tensor(interp, logits.dtype, logits.shape[0:logits.rank], "ml/grad: failed to allocate softmax output tensor");
    if (output == null || output.tag == ERROR) return output;
    err = ml_reduction_write_result(output.tensor_val, softmax_values, interp, "ml/grad: Float32 softmax output is not representable");
    if (err != null) return err;

    err = ml_grad_expr_accumulate_to_wrt(logits_arg, wrt_arg, upstream, wrt_grad, interp);
    if (err != null) return err;
    return ml_grad_tensor_result(interp, "tensor-softmax-cross-entropy", logits.dtype, loss, output, wrt, wrt_grad, true);
}
