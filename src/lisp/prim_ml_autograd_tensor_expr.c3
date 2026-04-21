module lisp;

import std::core::mem;

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
            return false;
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
            return ml_grad_expr_same_tensor(node, target) ? 1 : 0;
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
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward kernels are not implemented", null);
    }
    if (tensor_expr_has_non_cpu_device(primary) ||
        tensor_expr_has_non_cpu_device(targets) ||
        tensor_expr_has_non_cpu_device(wrt)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: backend kernel is not implemented for non-CPU tensors", null);
    }
    return null;
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
        op,
        interp
    );
}

fn Value* ml_grad_tensor_mse_try_vulkan(Value* prediction_arg, Value* targets_arg, Value* wrt_arg, Interp* interp) {
    bool touches_vulkan = ml_grad_touches_vulkan(prediction_arg) ||
        ml_grad_touches_vulkan(targets_arg) ||
        ml_grad_touches_vulkan(wrt_arg);
    if (!touches_vulkan) return null;
    if (tensor_expr_has_non_vulkan_device(prediction_arg) ||
        tensor_expr_has_non_vulkan_device(targets_arg) ||
        tensor_expr_has_non_vulkan_device(wrt_arg)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward operands must remain Vulkan-placed", null);
    }
    if (!ml_grad_expr_same_tensor(prediction_arg, wrt_arg)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan tensor-expression backward currently supports wrt equal to prediction", null);
    }

    TensorVal* prediction = prediction_arg.tensor_val;
    TensorVal* targets = targets_arg.tensor_val;
    TensorVal* wrt = wrt_arg.tensor_val;
    if (prediction == null || targets == null || wrt == null ||
        prediction.kind != TENSOR_PAYLOAD_CONCRETE ||
        targets.kind != TENSOR_PAYLOAD_CONCRETE ||
        wrt.kind != TENSOR_PAYLOAD_CONCRETE) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan expression graph backward is not implemented", null);
    }
    if (prediction.device != TENSOR_DEVICE_VULKAN ||
        targets.device != TENSOR_DEVICE_VULKAN ||
        wrt.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan backward operands must remain Vulkan-placed", null);
    }
    if (prediction.dtype != TENSOR_DTYPE_FLOAT32 || targets.dtype != TENSOR_DTYPE_FLOAT32 || wrt.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan MSE backward currently supports dense row-major Float32 tensors", null);
    }
    if (!tensor_dense_row_major(prediction) || !tensor_dense_row_major(targets) || !tensor_dense_row_major(wrt)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/grad: Vulkan MSE backward currently supports dense row-major tensors", null);
    }
    if (prediction.element_count == 0 || !tensor_same_shape(prediction, targets) || !tensor_same_shape(prediction, wrt)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/grad: Vulkan MSE backward tensors must have the same non-empty shape", null);
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
        targets_arg,
        targets,
        prediction.dtype,
        prediction.shape[0:prediction.rank]
    );
    if (diff == null || diff.tag == ERROR) return diff;
    float scale = (float)(2.0 / (double)prediction.element_count);
    Value* scale_value = make_float32(interp, scale);
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
    if (!tensor_map_callable_vulkan_op(func, &op) || op > 3) return false;
    if (out_op != null) *out_op = op;
    return true;
}

fn bool ml_grad_map_unary_derivative(Value* func, double x, double* out_derivative) {
    if (out_derivative == null) return false;
    if (primitive_name_equals(func, "+")) {
        *out_derivative = 1.0;
        return true;
    }
    if (primitive_name_equals(func, "-")) {
        *out_derivative = -1.0;
        return true;
    }
    if (primitive_name_equals(func, "sqrt") && x > 0.0) {
        *out_derivative = 0.5 / tensor_c_sqrt(x);
        return true;
    }
    return false;
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
            if (!ml_reduction_read(left, left_offset, &x) ||
                !ml_grad_map_unary_derivative(expr.map_func, x, &derivative)) {
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
            case 1:
                if (left_upstream != null) left_upstream[left_offset] += u;
                if (right_upstream != null) right_upstream[right_offset] -= u;
            case 2:
                if (left_upstream != null) left_upstream[left_offset] += u * b;
                if (right_upstream != null) right_upstream[right_offset] += u * a;
            case 3:
                if (b == 0.0) return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "ml/grad: division map backward encountered zero denominator", null);
                if (left_upstream != null) left_upstream[left_offset] += u / b;
                if (right_upstream != null) right_upstream[right_offset] -= u * a / (b * b);
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

fn Value* ml_grad_tensor_softmax_cross_entropy(Value* spec, Interp* interp) {
    Value* logits_arg = ml_grad_required_tensor(spec, interp, "logits");
    if (logits_arg == null || logits_arg.tag == ERROR) return logits_arg;
    Value* targets_arg = ml_grad_required_tensor(spec, interp, "targets");
    if (targets_arg == null || targets_arg.tag == ERROR) return targets_arg;
    Value* wrt_arg = ml_grad_required_tensor(spec, interp, "wrt");
    if (wrt_arg == null || wrt_arg.tag == ERROR) return wrt_arg;
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
