module lisp;

import std::core::mem;

const double ML_GRAD_FLOAT32_MIN_NORMAL = 1.1754943508222875e-38;
const double ML_GRAD_FLOAT64_MIN_NORMAL = 2.2250738585072014e-308;

fn Value* ml_grad_expr_buffer(Interp* interp, usz count, double** out, char[] message) {
    if (out == null) return tensor_runtime_raise(interp, message);
    *out = null;
    if (count > usz.max / double.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", message, null);
    }
    double* values = count == 0 ? null : (double*)mem::malloc(count * double.sizeof);
    if (count > 0 && values == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", message, null);
    }
    for (usz i = 0; i < count; i++) values[i] = 0.0;
    *out = values;
    return null;
}

fn bool ml_grad_expr_same_tensor(Value* left, Value* right) @inline {
    if (left == null || right == null) return false;
    if (left == right) return true;
    if (!is_tensor(left) || !is_tensor(right)) return false;
    if (left.tensor_val == right.tensor_val) return true;
    TensorVal* left_tensor = left.tensor_val;
    TensorVal* right_tensor = right.tensor_val;
    if (left_tensor != null && right_tensor != null &&
        left_tensor.kind == TENSOR_PAYLOAD_CONCRETE &&
        right_tensor.kind == TENSOR_PAYLOAD_CONCRETE &&
        left_tensor.device != TENSOR_DEVICE_CPU &&
        left_tensor.device == right_tensor.device &&
        left_tensor.dtype == right_tensor.dtype &&
        left_tensor.byte_len == right_tensor.byte_len &&
        tensor_same_shape(left_tensor, right_tensor) &&
        left_tensor.device_handle != null &&
        tensor_has_valid_device_storage(left_tensor, left_tensor.device) &&
        tensor_has_valid_device_storage(right_tensor, right_tensor.device) &&
        left_tensor.device_handle == right_tensor.device_handle) {
        return true;
    }
    if (left_tensor == null || right_tensor == null ||
        left_tensor.kind != TENSOR_PAYLOAD_CONCRETE ||
        right_tensor.kind != TENSOR_PAYLOAD_CONCRETE ||
        left_tensor.device != TENSOR_DEVICE_CPU ||
        right_tensor.device != TENSOR_DEVICE_CPU ||
        left_tensor.dtype != right_tensor.dtype ||
        !tensor_same_shape(left_tensor, right_tensor)) {
        return false;
    }
    for (usz i = 0; i < left_tensor.element_count; i++) {
        double a = 0.0;
        double b = 0.0;
        if (!ml_reduction_read(left_tensor, i, &a) ||
            !ml_reduction_read(right_tensor, i, &b) ||
            a != b) {
            return false;
        }
    }
    return true;
}

fn bool ml_grad_expr_contains_value(Value* node, Value* target) {
    if (ml_grad_expr_same_tensor(node, target)) return true;
    if (!is_tensor(node) || target == null) return false;
    TensorVal* tensor = node.tensor_val;
    if (tensor == null) return false;
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return tensor_concrete_has_map_provenance(tensor) &&
                (ml_grad_expr_contains_value(tensor.map_left, target) ||
                    ml_grad_expr_contains_value(tensor.map_right, target));
        case TENSOR_PAYLOAD_MAP:
            return ml_grad_expr_contains_value(tensor.map_left, target) ||
                ml_grad_expr_contains_value(tensor.map_right, target);
        case TENSOR_PAYLOAD_CONTRACT:
            return ml_grad_expr_contains_value(tensor.contract_left, target) ||
                ml_grad_expr_contains_value(tensor.contract_right, target);
        case TENSOR_PAYLOAD_VIEW:
            return ml_grad_expr_contains_value(tensor.view_source, target);
    }
}

fn usz ml_grad_expr_matching_leaf_count(Value* node, Value* target) {
    if (!is_tensor(node) || target == null || node.tensor_val == null) return 0;
    TensorVal* tensor = node.tensor_val;
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            usz matches = ml_grad_expr_same_tensor(node, target) ? 1 : 0;
            if (tensor_concrete_has_map_provenance(tensor)) {
                matches += ml_grad_expr_matching_leaf_count(tensor.map_left, target);
                matches += ml_grad_expr_matching_leaf_count(tensor.map_right, target);
            }
            return matches;
        case TENSOR_PAYLOAD_MAP:
            return ml_grad_expr_matching_leaf_count(tensor.map_left, target) +
                ml_grad_expr_matching_leaf_count(tensor.map_right, target);
        case TENSOR_PAYLOAD_CONTRACT:
            return ml_grad_expr_matching_leaf_count(tensor.contract_left, target) +
                ml_grad_expr_matching_leaf_count(tensor.contract_right, target);
        case TENSOR_PAYLOAD_VIEW:
            return ml_grad_expr_matching_leaf_count(tensor.view_source, target);
    }
}

fn Value* ml_grad_expr_validate_wrt_leaf(Value* expression, Value* wrt, Interp* interp) {
    usz matches = ml_grad_expr_matching_leaf_count(expression, wrt);
    if (matches == 0) {
        return ml_grad_invalid(interp, "ml/grad: wrt tensor is not reachable from Tensor expression");
    }
    if (matches > 1) {
        return ml_grad_invalid(interp, "ml/grad: wrt tensor matches multiple Tensor expression leaves");
    }
    return null;
}

fn Value* ml_grad_expr_reject_device_backward(Value* primary, Value* targets, Value* wrt, Interp* interp) {
    if (ml_grad_touches_vulkan(primary) ||
        ml_grad_touches_vulkan(targets) ||
        ml_grad_touches_vulkan(wrt)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward is not supported for this expression configuration", null);
    }
    if (tensor_expr_has_non_cpu_device(primary) ||
        tensor_expr_has_non_cpu_device(targets) ||
        tensor_expr_has_non_cpu_device(wrt)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: backend kernel is not implemented for non-CPU tensors", null);
    }
    return null;
}

fn Value* ml_grad_vulkan_binary_map_direct(
    Interp* interp,
    uint op,
    Value* left_value,
    TensorVal* left_tensor,
    Value* right_value,
    TensorVal* right_tensor,
    TensorDType dtype,
    usz[] result_shape
) {
    return tensor_map_try_vulkan_direct_checked(
        left_value,
        left_tensor,
        right_value,
        right_tensor,
        true,
        dtype,
        result_shape,
        null,
        op,
        interp
    );
}

fn Value* ml_grad_lookup_map_primitive(Interp* interp, char[] name) {
    if (interp == null || interp.global_env == null) return null;
    SymbolId sym = interp.symbols.intern(name);
    if (sym == INVALID_SYMBOL_ID) return null;
    Value* func = interp.global_env.lookup(sym);
    if (func == null) return null;
    return (func.tag == PRIMITIVE || func.tag == METHOD_TABLE) ? func : null;
}

fn Value* ml_grad_vulkan_binary_map(
    Interp* interp,
    char[] op_name,
    Value* left_value,
    TensorVal* left_tensor,
    Value* right_value,
    TensorVal* right_tensor,
    TensorDType dtype,
    usz[] result_shape
) {
    Value* func = ml_grad_lookup_map_primitive(interp, op_name);
    if (func == null) return tensor_runtime_raise(interp, "ml/grad: Vulkan backward map primitive is missing");
    uint op = 0;
    if (!tensor_map_callable_vulkan_op(func, &op)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward map primitive is unsupported", null);
    }
    return tensor_map_try_vulkan_direct_checked(
        left_value,
        left_tensor,
        right_value,
        right_tensor,
        true,
        dtype,
        result_shape,
        null,
        op,
        interp
    );
}

fn Value* ml_grad_vulkan_map_scale_upstream(
    Interp* interp,
    Value* upstream_value,
    TensorVal* upstream,
    Value* other_value,
    TensorVal* other,
    double scalar,
    bool has_scalar,
    TensorDType dtype,
    usz[] result_shape
) {
    if (has_scalar) {
        Value* scalar_value = dtype == TENSOR_DTYPE_DOUBLE ? make_double(interp, scalar) : make_float32(interp, (float)scalar);
        if (scalar_value == null) return nn_oom(interp);
        return ml_grad_vulkan_binary_map(interp, "*", upstream_value, upstream, scalar_value, null, dtype, result_shape);
    }
    if (other_value == null || other == null || other.device != TENSOR_DEVICE_VULKAN || !tensor_map_operand_matches_shape(other, upstream.shape[0:upstream.rank])) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan map backward does not support broadcast-reduction gradients", null);
    }
    return ml_grad_vulkan_binary_map(interp, "*", upstream_value, upstream, other_value, other, dtype, result_shape);
}

fn Value* ml_grad_vulkan_map_comparison_mask(
    Interp* interp,
    Value* left_value,
    TensorVal* left,
    Value* right_value,
    TensorVal* right,
    uint op,
    TensorDType dtype,
    usz[] result_shape
) {
    return tensor_map_try_vulkan_direct_checked(
        left_value,
        left,
        right_value,
        right,
        true,
        dtype,
        result_shape,
        null,
        op,
        interp
    );
}

fn Value* ml_grad_vulkan_map_backward_to_wrt(Value* node, Value* wrt, Value* upstream_value, Interp* interp) {
    if (ml_grad_expr_same_tensor(node, wrt)) return upstream_value;
    if (!is_tensor(node) || node.tensor_val == null || !is_tensor(wrt) || wrt.tensor_val == null ||
        upstream_value == null || upstream_value.tag != TENSOR || upstream_value.tensor_val == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan map backward graph is incomplete", null);
    }

    TensorVal* output = node.tensor_val;
    TensorVal* upstream = upstream_value.tensor_val;
    TensorVal* wrt_tensor = wrt.tensor_val;
    if (!tensor_concrete_has_map_provenance(output)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan map backward provenance is missing", null);
    }
    if (output.device != TENSOR_DEVICE_VULKAN || upstream.device != TENSOR_DEVICE_VULKAN || wrt_tensor.device != TENSOR_DEVICE_VULKAN ||
        (output.dtype != TENSOR_DTYPE_FLOAT32 && output.dtype != TENSOR_DTYPE_DOUBLE) ||
        (upstream.dtype != TENSOR_DTYPE_FLOAT32 && upstream.dtype != TENSOR_DTYPE_DOUBLE) ||
        (wrt_tensor.dtype != TENSOR_DTYPE_FLOAT32 && wrt_tensor.dtype != TENSOR_DTYPE_DOUBLE) ||
        output.dtype != upstream.dtype || upstream.dtype != wrt_tensor.dtype ||
        !tensor_dense_row_major(output) || !tensor_dense_row_major(upstream) || !tensor_dense_row_major(wrt_tensor) ||
        !tensor_same_shape(output, upstream) || !tensor_map_operand_matches_shape(wrt_tensor, output.shape[0:output.rank])) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan map backward currently supports broadcast-compatible dense row-major Float32 and Float64 tensors", null);
    }

    usz matches = ml_grad_expr_matching_leaf_count(node, wrt);
    if (matches == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: wrt tensor is not reachable from Vulkan map expression", null);
    }
    if (matches > 1) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan map backward does not support duplicated wrt leaves", null);
    }

    uint op = 0;
    if (!tensor_map_callable_vulkan_op(output.map_func, &op) || op > 5) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: unsupported Vulkan map backward rule", null);
    }

    bool left_path = ml_grad_expr_contains_value(output.map_left, wrt);
    bool right_path = output.map_is_binary && ml_grad_expr_contains_value(output.map_right, wrt);
    if (!left_path && !right_path) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: wrt tensor is not reachable from Vulkan map expression", null);
    }
    if (left_path && right_path) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan map backward does not support duplicated wrt leaves", null);
    }

    Value* child_value = left_path ? output.map_left : output.map_right;
    TensorVal* child = child_value != null && child_value.tag == TENSOR ? child_value.tensor_val : null;
    if (child == null || child.device != TENSOR_DEVICE_VULKAN || !tensor_map_operand_matches_shape(child, output.shape[0:output.rank])) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan map backward does not support broadcast-reduction gradients", null);
    }

    Value* child_upstream = upstream_value;
    switch (op) {
        case 0:
            child_upstream = upstream_value;
            break;
        case 1:
            if (right_path) {
                child_upstream = ml_grad_vulkan_map_scale_upstream(
                    interp,
                    upstream_value,
                    upstream,
                    null,
                    null,
                    -1.0,
                    true,
                    output.dtype,
                    output.shape[0:output.rank]
                );
                if (child_upstream == null || child_upstream.tag == ERROR) return child_upstream;
            }
            break;
        case 2: {
            Value* other_value = left_path ? output.map_right : output.map_left;
            TensorVal* other = other_value != null && other_value.tag == TENSOR ? other_value.tensor_val : null;
            double scalar = left_path ? output.map_right_scalar : output.map_left_scalar;
            bool has_scalar = other == null;
            child_upstream = ml_grad_vulkan_map_scale_upstream(
                interp,
                upstream_value,
                upstream,
                other_value,
                other,
                scalar,
                has_scalar,
                output.dtype,
                output.shape[0:output.rank]
            );
            if (child_upstream == null || child_upstream.tag == ERROR) return child_upstream;
        }
            break;
        case 3: {
            Value* other_value = left_path ? output.map_right : output.map_left;
            TensorVal* other = other_value != null && other_value.tag == TENSOR ? other_value.tensor_val : null;
            if (other == null) {
                double scalar = left_path ? output.map_right_scalar : output.map_left_scalar;
                other_value = output.dtype == TENSOR_DTYPE_DOUBLE
                    ? make_double(interp, scalar)
                    : make_float32(interp, (float)scalar);
                if (other_value == null) return nn_oom(interp);
            }
            if (left_path) {
                child_upstream = ml_grad_vulkan_binary_map(
                    interp,
                    "/",
                    upstream_value,
                    upstream,
                    other_value,
                    other,
                    output.dtype,
                    output.shape[0:output.rank]
                );
            } else {
                Value* neg_output = ml_grad_vulkan_map_scale_upstream(
                    interp,
                    node,
                    output,
                    null,
                    null,
                    -1.0,
                    true,
                    output.dtype,
                    output.shape[0:output.rank]
                );
                if (neg_output == null || neg_output.tag == ERROR) return neg_output;
                Value* scale = ml_grad_vulkan_binary_map(
                    interp,
                    "/",
                    neg_output,
                    neg_output.tensor_val,
                    other_value,
                    other,
                    output.dtype,
                    output.shape[0:output.rank]
                );
                if (scale == null || scale.tag == ERROR) return scale;
                child_upstream = ml_grad_vulkan_binary_map(
                    interp,
                    "*",
                    upstream_value,
                    upstream,
                    scale,
                    scale.tensor_val,
                    output.dtype,
                    output.shape[0:output.rank]
                );
            }
            if (child_upstream == null || child_upstream.tag == ERROR) return child_upstream;
        }
            break;
        case 4:
        case 5: {
            uint mask_op = 0;
            if (op == 4) {
                mask_op = left_path ? 7 : 6;
            } else {
                mask_op = left_path ? 6 : 7;
            }
            TensorVal* mask_left = output.map_left != null && output.map_left.tag == TENSOR ? output.map_left.tensor_val : null;
            TensorVal* mask_right = output.map_right != null && output.map_right.tag == TENSOR ? output.map_right.tensor_val : null;
            Value* mask_left_value = output.map_left;
            Value* mask_right_value = output.map_right;
            if (mask_left == null) {
                mask_left_value = output.dtype == TENSOR_DTYPE_DOUBLE
                    ? make_double(interp, output.map_left_scalar)
                    : make_float32(interp, (float)output.map_left_scalar);
                if (mask_left_value == null) return nn_oom(interp);
            }
            if (mask_right == null) {
                mask_right_value = output.dtype == TENSOR_DTYPE_DOUBLE
                    ? make_double(interp, output.map_right_scalar)
                    : make_float32(interp, (float)output.map_right_scalar);
                if (mask_right_value == null) return nn_oom(interp);
            }
            Value* mask = ml_grad_vulkan_map_comparison_mask(
                interp,
                mask_left_value,
                mask_left,
                mask_right_value,
                mask_right,
                mask_op,
                output.dtype,
                output.shape[0:output.rank]
            );
            if (mask == null || mask.tag == ERROR) return mask;
            child_upstream = ml_grad_vulkan_binary_map(
                interp,
                "*",
                upstream_value,
                upstream,
                mask,
                mask.tensor_val,
                output.dtype,
                output.shape[0:output.rank]
            );
            if (child_upstream == null || child_upstream.tag == ERROR) return child_upstream;
        }
        default:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: unsupported Vulkan map backward rule", null);
    }

    child_upstream = ml_grad_vulkan_reduce_broadcast_to_child(interp, child_upstream, child, output);
    if (child_upstream == null || child_upstream.tag == ERROR) return child_upstream;
    return ml_grad_vulkan_map_backward_to_wrt(child_value, wrt, child_upstream, interp);
}

fn Value* ml_grad_tensor_mse_try_vulkan(Value* prediction_arg, Value* targets_arg, Value* wrt_arg, Interp* interp) {
    bool touches_vulkan = ml_grad_touches_vulkan(prediction_arg) ||
        ml_grad_touches_vulkan(targets_arg) ||
        ml_grad_touches_vulkan(wrt_arg);
    if (!touches_vulkan) return null;
    if (tensor_expr_has_non_vulkan_device(prediction_arg) ||
        tensor_expr_has_non_vulkan_device(wrt_arg)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward operands must remain Vulkan-placed", null);
    }

    TensorVal* prediction = prediction_arg.tensor_val;
    TensorVal* wrt = wrt_arg.tensor_val;
    if (prediction == null || wrt == null ||
        prediction.kind != TENSOR_PAYLOAD_CONCRETE ||
        wrt.kind != TENSOR_PAYLOAD_CONCRETE ||
        prediction.device != TENSOR_DEVICE_VULKAN ||
        wrt.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward operands must remain Vulkan-placed", null);
    }

    Value* targets_value = null;
    TensorVal* targets = null;
    Value* err = tensor_expr_resolve_vulkan_or_migrate(targets_arg, &targets, &targets_value, interp, "ml/grad: Vulkan MSE backward target tensor backing storage is missing");
    if (err != null) return err;
    bool cleanup_targets = targets_value != targets_arg;
    defer if (cleanup_targets) {
        boundary_cleanup_materialized_value(null, targets_value, interp.current_scope);
    }
    if (targets == null || targets.kind != TENSOR_PAYLOAD_CONCRETE || targets.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward target must be a concrete Vulkan Tensor", null);
    }
    if ((prediction.dtype != TENSOR_DTYPE_FLOAT32 && prediction.dtype != TENSOR_DTYPE_DOUBLE) ||
        (targets.dtype != TENSOR_DTYPE_FLOAT32 && targets.dtype != TENSOR_DTYPE_DOUBLE) ||
        (wrt.dtype != TENSOR_DTYPE_FLOAT32 && wrt.dtype != TENSOR_DTYPE_DOUBLE) ||
        prediction.dtype != targets.dtype || targets.dtype != wrt.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan MSE backward currently supports dense row-major Float32 and Float64 tensors", null);
    }
    if (!tensor_dense_row_major(prediction) || !tensor_dense_row_major(targets) || !tensor_dense_row_major(wrt)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan MSE backward currently supports dense row-major tensors", null);
    }
    if (prediction.element_count == 0 || !tensor_same_shape(prediction, targets) || !tensor_map_operand_matches_shape(wrt, prediction.shape[0:prediction.rank])) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: Vulkan MSE backward wrt tensor must match or broadcast to prediction shape", null);
    }
    if (!tensor_has_valid_device_storage(prediction, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(targets, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(wrt, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/grad: Vulkan tensor backing storage is missing");
    }

    Value* loss = ml_mean_squared_error_eval_vulkan(prediction, targets, interp);
    if (loss == null || loss.tag == ERROR) return loss;
    Value* diff = ml_grad_vulkan_binary_map(
        interp,
        "-",
        prediction_arg,
        prediction,
        targets_value,
        targets,
        prediction.dtype,
        prediction.shape[0:prediction.rank]
    );
    if (diff == null || diff.tag == ERROR) return diff;
    double scale = 2.0 / (double)prediction.element_count;
    Value* scale_value = prediction.dtype == TENSOR_DTYPE_DOUBLE ? make_double(interp, scale) : make_float32(interp, (float)scale);
    if (scale_value == null) return nn_oom(interp);
    Value* grad = ml_grad_vulkan_binary_map(
        interp,
        "*",
        diff,
        diff.tensor_val,
        scale_value,
        null,
        prediction.dtype,
        prediction.shape[0:prediction.rank]
    );
    if (grad == null || grad.tag == ERROR) return grad;
    if (!ml_grad_expr_same_tensor(prediction_arg, wrt_arg)) {
        if (!tensor_concrete_has_map_provenance(prediction)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan tensor-expression backward currently supports wrt equal to prediction or same-shape map provenance", null);
        }
        grad = ml_grad_vulkan_map_backward_to_wrt(prediction_arg, wrt_arg, grad, interp);
        if (grad == null || grad.tag == ERROR) return grad;
    }
    return ml_grad_tensor_value_result(interp, "tensor-mean-squared-error", prediction.dtype, "vulkan", loss, prediction_arg, grad, false);
}

fn Value* ml_grad_expr_resolve_dense_cpu(Value* value, TensorVal** out, Interp* interp, char[] label) {
    Value* resolved_value = null;
    Value* err = ml_grad_resolve_cpu(value, out, &resolved_value, interp, label);
    if (err != null) return err;
    if (out == null || *out == null || !tensor_dense_row_major(*out)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: generic Tensor gradients currently require dense row-major CPU tensors", null);
    }
    return null;
}

fn bool ml_grad_map_binary_op(Value* func, uint* out_op) {
    uint op = 0;
    if (!tensor_map_callable_vulkan_op(func, &op) || op > 5) return false;
    if (out_op != null) *out_op = op;
    return true;
}

fn bool ml_grad_callable_name_equals(Value* func, char[] expected) @inline {
    if (primitive_name_equals(func, expected)) return true;
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        return tensor_primitive_payload_name_equals(method_table_fallback_prim(func.method_table_val), expected);
    }
    return false;
}

fn Value* ml_grad_map_unary_derivative(Value* func, double x, double* out_derivative, bool* out_supported, Interp* interp) {
    if (out_derivative == null || out_supported == null) return tensor_runtime_raise(interp, "ml/grad: Tensor map derivative state is missing");
    *out_supported = false;
    if (ml_grad_callable_name_equals(func, "+")) {
        *out_derivative = 1.0;
        *out_supported = true;
        return null;
    }
    if (ml_grad_callable_name_equals(func, "-")) {
        *out_derivative = -1.0;
        *out_supported = true;
        return null;
    }
    if (ml_grad_callable_name_equals(func, "sqrt")) {
        if (!conversion_double_is_finite(x) || !(x > 0.0)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/grad: sqrt map backward requires positive finite input; signed zero is undefined", null);
        }
        double derivative = 0.5 / tensor_c_sqrt(x);
        if (!conversion_double_is_finite(derivative)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/numeric-overflow", "ml/grad: sqrt map backward derivative is not finite", null);
        }
        *out_derivative = derivative;
        *out_supported = true;
        return null;
    }
    return null;
}

fn double ml_grad_min_normal_for_dtype(TensorDType dtype) @inline {
    return dtype == TENSOR_DTYPE_FLOAT32 ? ML_GRAD_FLOAT32_MIN_NORMAL : ML_GRAD_FLOAT64_MIN_NORMAL;
}

fn Value* ml_grad_division_denominator_value(double b, Interp* interp) {
    if (!conversion_double_is_finite(b) || b == 0.0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/grad: division map backward encountered zero or non-finite denominator", null);
    }
    return null;
}

fn Value* ml_grad_division_denominator_square(TensorDType dtype, double b, double* out_square, Interp* interp) {
    if (out_square == null) return tensor_runtime_raise(interp, "ml/grad: division map backward state is missing");
    Value* err = ml_grad_division_denominator_value(b, interp);
    if (err != null) return err;
    double square = b * b;
    double min_normal = ml_grad_min_normal_for_dtype(dtype);
    if (!conversion_double_is_finite(square) || !(square >= min_normal)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/grad: division map backward denominator square is subnormal or not finite", null);
    }
    *out_square = square;
    return null;
}

fn Value* ml_grad_expr_accumulate_to_wrt(Value* node, Value* wrt, double* upstream, double* out_grad, Interp* interp) {
    if (node == null || upstream == null || out_grad == null) return tensor_runtime_raise(interp, "ml/grad: Tensor gradient state is missing");
    if (ml_grad_expr_same_tensor(node, wrt)) {
        TensorVal* tensor = null;
        Value* err = ml_grad_expr_resolve_dense_cpu(node, &tensor, interp, "ml/grad: Tensor gradient leaf backing storage is missing");
        if (err != null) return err;
        for (usz i = 0; i < tensor.element_count; i++) out_grad[i] += upstream[i];
        return null;
    }
    if (!is_tensor(node) || node.tensor_val == null) return null;
    TensorVal* tensor = node.tensor_val;
    switch (tensor.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return null;
        case TENSOR_PAYLOAD_MAP:
            return ml_grad_expr_backprop_map(node, wrt, upstream, out_grad, interp);
        case TENSOR_PAYLOAD_CONTRACT:
            return ml_grad_expr_backprop_contract(node, wrt, upstream, out_grad, interp);
        case TENSOR_PAYLOAD_VIEW:
            return ml_grad_expr_contains_value(tensor.view_source, wrt)
                ? raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Tensor view backward rule is not implemented", null)
                : null;
    }
}

fn Value* ml_grad_expr_backprop_map(Value* node, Value* wrt, double* upstream, double* out_grad, Interp* interp) {
    TensorVal* expr = node.tensor_val;
    TensorVal* output = null;
    Value* err = ml_grad_expr_resolve_dense_cpu(node, &output, interp, "ml/grad: Tensor map output backing storage is missing");
    if (err != null) return err;

    bool left_path = ml_grad_expr_contains_value(expr.map_left, wrt);
    bool right_path = ml_grad_expr_contains_value(expr.map_right, wrt);
    if (!left_path && !right_path) return null;

    TensorVal* left = null;
    TensorVal* right = null;
    if (expr.map_left != null && is_tensor(expr.map_left)) {
        err = ml_grad_expr_resolve_dense_cpu(expr.map_left, &left, interp, "ml/grad: Tensor map left backing storage is missing");
        if (err != null) return err;
    }
    if (expr.map_right != null && is_tensor(expr.map_right)) {
        err = ml_grad_expr_resolve_dense_cpu(expr.map_right, &right, interp, "ml/grad: Tensor map right backing storage is missing");
        if (err != null) return err;
    }

    double* left_upstream = null;
    double* right_upstream = null;
    if (left_path && left != null) {
        err = ml_grad_expr_buffer(interp, left.element_count, &left_upstream, "ml/grad: failed to allocate map left gradient");
        if (err != null) return err;
    }
    defer if (left_upstream != null) mem::free(left_upstream);
    if (right_path && right != null) {
        err = ml_grad_expr_buffer(interp, right.element_count, &right_upstream, "ml/grad: failed to allocate map right gradient");
        if (err != null) return err;
    }
    defer if (right_upstream != null) mem::free(right_upstream);

    uint binary_op = 0;
    if (expr.map_is_binary && !ml_grad_map_binary_op(expr.map_func, &binary_op)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: unsupported Tensor map backward rule", null);
    }

    for (usz i = 0; i < output.element_count; i++) {
        double u = upstream[i];
        if (!expr.map_is_binary) {
            if (left == null) return tensor_runtime_raise(interp, "ml/grad: Tensor map left input is missing");
            usz left_offset = 0;
            if (!tensor_map_operand_offset(left, output, i, &left_offset)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: Tensor map gradient shape mismatch", null);
            }
            double x = 0.0;
            double derivative = 0.0;
            bool derivative_supported = false;
            if (!ml_reduction_read(left, left_offset, &x)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: Tensor map gradient shape mismatch", null);
            }
            err = ml_grad_map_unary_derivative(expr.map_func, x, &derivative, &derivative_supported, interp);
            if (err != null) return err;
            if (!derivative_supported) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: unsupported Tensor map backward rule", null);
            }
            if (left_upstream != null) left_upstream[left_offset] += u * derivative;
            continue;
        }

        double a = expr.map_left_scalar;
        double b = expr.map_right_scalar;
        usz left_offset = 0;
        usz right_offset = 0;
        if (left != null) {
            if (!tensor_map_operand_offset(left, output, i, &left_offset) ||
                !ml_reduction_read(left, left_offset, &a)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: Tensor map gradient shape mismatch", null);
            }
        }
        if (right != null) {
            if (!tensor_map_operand_offset(right, output, i, &right_offset) ||
                !ml_reduction_read(right, right_offset, &b)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: Tensor map gradient shape mismatch", null);
            }
        }
        switch (binary_op) {
            case 0:
                if (left_upstream != null) left_upstream[left_offset] += u;
                if (right_upstream != null) right_upstream[right_offset] += u;
                break;
            case 1:
                if (left_upstream != null) left_upstream[left_offset] += u;
                if (right_upstream != null) right_upstream[right_offset] -= u;
                break;
            case 2:
                if (left_upstream != null) left_upstream[left_offset] += u * b;
                if (right_upstream != null) right_upstream[right_offset] += u * a;
                break;
            case 3:
                err = ml_grad_division_denominator_value(b, interp);
                if (err != null) return err;
                if (left_upstream != null) {
                    double left_grad = u / b;
                    if (!conversion_double_is_finite(left_grad)) {
                        return raise_error_with_payload_names(interp, "tensor", "tensor/numeric-overflow", "ml/grad: division map backward left gradient is not finite", null);
                    }
                    left_upstream[left_offset] += left_grad;
                }
                if (right_upstream != null) {
                    double denominator_square = 0.0;
                    err = ml_grad_division_denominator_square(output.dtype, b, &denominator_square, interp);
                    if (err != null) return err;
                    double right_grad = u * a / denominator_square;
                    if (!conversion_double_is_finite(right_grad)) {
                        return raise_error_with_payload_names(interp, "tensor", "tensor/numeric-overflow", "ml/grad: division map backward right gradient is not finite", null);
                    }
                    right_upstream[right_offset] -= right_grad;
                }
                break;
            case 4:
                if (left_upstream != null && a <= b) left_upstream[left_offset] += u;
                if (right_upstream != null && b <= a) right_upstream[right_offset] += u;
                break;
            case 5:
                if (left_upstream != null && a >= b) left_upstream[left_offset] += u;
                if (right_upstream != null && b >= a) right_upstream[right_offset] += u;
                break;
            default:
                return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: unsupported Tensor map backward rule", null);
        }
    }

    if (left_upstream != null) {
        err = ml_grad_expr_accumulate_to_wrt(expr.map_left, wrt, left_upstream, out_grad, interp);
        if (err != null) return err;
    }
    if (right_upstream != null) {
        err = ml_grad_expr_accumulate_to_wrt(expr.map_right, wrt, right_upstream, out_grad, interp);
        if (err != null) return err;
    }
    return null;
}

fn Value* ml_grad_expr_backprop_contract(Value* node, Value* wrt, double* upstream, double* out_grad, Interp* interp) {
    TensorVal* expr = node.tensor_val;
    TensorVal* output = null;
    Value* err = ml_grad_expr_resolve_dense_cpu(node, &output, interp, "ml/grad: Tensor contract output backing storage is missing");
    if (err != null) return err;

    bool left_path = ml_grad_expr_contains_value(expr.contract_left, wrt);
    bool right_path = ml_grad_expr_contains_value(expr.contract_right, wrt);
    if (!left_path && !right_path) return null;

    TensorVal* left = null;
    TensorVal* right = null;
    err = ml_grad_expr_resolve_dense_cpu(expr.contract_left, &left, interp, "ml/grad: Tensor contract left backing storage is missing");
    if (err != null) return err;
    err = ml_grad_expr_resolve_dense_cpu(expr.contract_right, &right, interp, "ml/grad: Tensor contract right backing storage is missing");
    if (err != null) return err;

    double* left_upstream = null;
    double* right_upstream = null;
    if (left_path) {
        err = ml_grad_expr_buffer(interp, left.element_count, &left_upstream, "ml/grad: failed to allocate contract left gradient");
        if (err != null) return err;
    }
    defer if (left_upstream != null) mem::free(left_upstream);
    if (right_path) {
        err = ml_grad_expr_buffer(interp, right.element_count, &right_upstream, "ml/grad: failed to allocate contract right gradient");
        if (err != null) return err;
    }
    defer if (right_upstream != null) mem::free(right_upstream);

    usz contracted_count = 1;
    for (usz axis = 0; axis < expr.contract_axis_count; axis++) {
        if (!tensor_checked_mul_usz(contracted_count, left.shape[expr.contract_left_axes[axis]], &contracted_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/grad: contracted axis product overflow", null);
        }
    }

    for (usz out_index = 0; out_index < output.element_count; out_index++) {
        usz left_base_offset = 0;
        usz right_base_offset = 0;
        usz result_pos = 0;
        for (usz axis = 0; axis < left.rank; axis++) {
            if (tensor_axis_is_listed(axis, expr.contract_left_axes, expr.contract_axis_count)) continue;
            usz coord = output.rank == 0 ? 0 : (out_index / output.strides[result_pos]) % output.shape[result_pos];
            left_base_offset += coord * left.strides[axis];
            result_pos++;
        }
        for (usz axis = 0; axis < right.rank; axis++) {
            if (tensor_axis_is_listed(axis, expr.contract_right_axes, expr.contract_axis_count)) continue;
            usz coord = output.rank == 0 ? 0 : (out_index / output.strides[result_pos]) % output.shape[result_pos];
            right_base_offset += coord * right.strides[axis];
            result_pos++;
        }
        double u = upstream[out_index];
        for (usz contract_index = 0; contract_index < contracted_count; contract_index++) {
            usz left_offset = left_base_offset;
            usz right_offset = right_base_offset;
            usz remainder = contract_index;
            for (usz i = expr.contract_axis_count; i > 0; i--) {
                usz axis_pos = i - 1;
                usz dim = left.shape[expr.contract_left_axes[axis_pos]];
                usz coord = remainder % dim;
                remainder /= dim;
                left_offset += coord * left.strides[expr.contract_left_axes[axis_pos]];
                right_offset += coord * right.strides[expr.contract_right_axes[axis_pos]];
            }
            double left_value = 0.0;
            double right_value = 0.0;
            if (!ml_reduction_read(left, left_offset, &left_value) ||
                !ml_reduction_read(right, right_offset, &right_value)) {
                return tensor_runtime_raise(interp, "ml/grad: Tensor contract index is invalid");
            }
            if (left_upstream != null) left_upstream[left_offset] += u * right_value;
            if (right_upstream != null) right_upstream[right_offset] += u * left_value;
        }
    }

    if (left_upstream != null) {
        err = ml_grad_expr_accumulate_to_wrt(expr.contract_left, wrt, left_upstream, out_grad, interp);
        if (err != null) return err;
    }
    if (right_upstream != null) {
        err = ml_grad_expr_accumulate_to_wrt(expr.contract_right, wrt, right_upstream, out_grad, interp);
        if (err != null) return err;
    }
    return null;
}

fn Value* ml_grad_tensor_make_tape(Interp* interp, TensorDType dtype, char[] source_name, char[] device_name, bool softmax_cross_entropy) {
    Value* nodes = make_array_checked(interp, softmax_cross_entropy ? 4 : 3, "ml/grad: failed to allocate tape nodes");
    if (nodes == null || nodes.tag == ERROR) return nodes;
    usz id = 0;
    if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "expression", "forward")) return err;
    if (softmax_cross_entropy) {
        if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "softmax", "activation")) return err;
        if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "cross-entropy", "loss")) return err;
    } else {
        if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "mean-squared-error", "loss")) return err;
    }
    if (Value* err = ml_grad_tape_push_node(nodes, interp, id++, "reverse-accumulate", "backward")) return err;

    Value* tape = nn_make_dict(interp, 16);
    if (tape == null || tape.tag == ERROR) return tape;
    if (Value* err = nn_dict_set_symbol(tape, interp, "kind", "gradient-tape")) return err;
    if (Value* err = nn_dict_set(tape, interp, "version", make_int(interp, 1))) return err;
    if (Value* err = nn_dict_set_symbol(tape, interp, "policy", "metadata-only")) return err;
    if (Value* err = nn_dict_set_symbol(tape, interp, "owner", "scope-region")) return err;
    if (Value* err = nn_dict_set(tape, interp, "retains-handles", make_bool_value(interp, false))) return err;
    if (Value* err = nn_dict_set_symbol(tape, interp, "device", device_name)) return err;
    if (Value* err = nn_dict_set(tape, interp, "dtype", make_symbol(interp, tensor_dtype_symbol(dtype, interp)))) return err;
    if (Value* err = nn_dict_set_symbol(tape, interp, "source", source_name)) return err;
    if (Value* err = nn_dict_set(tape, interp, "node-count", make_int(interp, (isz)nodes.array_val.length))) return err;
    if (Value* err = nn_dict_set(tape, interp, "nodes", nodes)) return err;
    return tape;
}

fn Value* ml_grad_tensor_value_result(
    Interp* interp,
    char[] source_name,
    TensorDType dtype,
    char[] device_name,
    Value* loss_value,
    Value* output,
    Value* input_grad,
    bool softmax_cross_entropy
) {
    Value* tape = ml_grad_tensor_make_tape(interp, dtype, source_name, device_name, softmax_cross_entropy);
    if (tape == null || tape.tag == ERROR) return tape;

    Value* result = nn_make_dict(interp, 10);
    if (result == null || result.tag == ERROR) return result;
    if (Value* set_err = nn_dict_set_symbol(result, interp, "kind", "gradients")) return set_err;
    if (Value* set_err = nn_dict_set_symbol(result, interp, "source", source_name)) return set_err;
    if (Value* set_err = nn_dict_set(result, interp, "loss", loss_value)) return set_err;
    if (Value* set_err = nn_dict_set(result, interp, "output", output)) return set_err;
    if (Value* set_err = nn_dict_set(result, interp, "input-gradient", input_grad)) return set_err;
    if (Value* set_err = nn_dict_set(result, interp, "tape", tape)) return set_err;
    return result;
}

fn Value* ml_grad_tensor_result(
    Interp* interp,
    char[] source_name,
    TensorDType dtype,
    double loss,
    Value* output,
    TensorVal* wrt,
    double* wrt_grad,
    bool softmax_cross_entropy
) {
    usz[1] scalar_shape = { 0 };
    Value* loss_value = ml_grad_make_tensor(interp, dtype, scalar_shape[0:0], "ml/grad: failed to allocate loss tensor");
    if (loss_value == null || loss_value.tag == ERROR) return loss_value;
    double[1] loss_values = { loss };
    Value* err = ml_reduction_write_result(loss_value.tensor_val, loss_values[..], interp, "ml/grad: Float32 loss is not representable");
    if (err != null) return err;

    Value* input_grad = ml_grad_make_tensor(interp, dtype, wrt.shape[0:wrt.rank], "ml/grad: failed to allocate Tensor expression gradient");
    if (input_grad == null || input_grad.tag == ERROR) return input_grad;
    err = ml_reduction_write_result(input_grad.tensor_val, wrt_grad, interp, "ml/grad: Float32 input gradient is not representable");
    if (err != null) return err;

    return ml_grad_tensor_value_result(interp, source_name, dtype, "cpu", loss_value, output, input_grad, softmax_cross_entropy);
}
