module lisp;

import std::core::mem;
import main;

fn bool is_tensor(Value* v) @inline {
    return v != null && v.tag == TENSOR;
}

fn bool tensor_dtype_arg_is_double(Value* dtype_arg, Interp* interp) @inline {
    if (dtype_arg == null) return false;
    if (dtype_arg.tag == PRIMITIVE) return primitive_name_equals(dtype_arg, "Double");
    if (dtype_arg.tag == METHOD_TABLE && dtype_arg.method_table_val != null) {
        return dtype_arg.method_table_val.name == interp.sym_Double;
    }
    return false;
}

fn Value* tensor_type_raise(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", "type/arg-mismatch", message, null);
}

fn Value* tensor_runtime_raise(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", message, null);
}

fn bool tensor_container_count(Value* container, usz* out_count) {
    if (out_count == null) return false;
    *out_count = 0;

    if (is_array(container)) {
        if (container.array_val == null) return false;
        *out_count = container.array_val.length;
        return true;
    }

    if (is_nil(container)) return true;

    usz count = 0;
    Value* cur = container;
    while (is_cons(cur)) {
        if (count == usz.max) return false;
        count++;
        cur = cur.cons_val.cdr;
    }
    if (!is_nil(cur)) return false;
    *out_count = count;
    return true;
}

fn Value* tensor_container_nth(Value* container, usz index) {
    if (is_array(container)) return container.array_val.items[index];

    Value* cur = container;
    for (usz i = 0; i < index; i++) {
        if (!is_cons(cur)) return null;
        cur = cur.cons_val.cdr;
    }
    return is_cons(cur) ? cur.cons_val.car : null;
}

fn bool tensor_parse_shape(Value* shape_arg, Interp* interp, usz** out_shape, usz* out_rank, Value** err_out) {
    if (out_shape == null || out_rank == null || err_out == null) return false;
    *out_shape = null;
    *out_rank = 0;
    *err_out = null;

    usz rank = 0;
    if (!tensor_container_count(shape_arg, &rank)) {
        *err_out = tensor_type_raise(interp, "Tensor: shape must be an array or proper list of non-negative integers");
        return false;
    }
    if (!is_array(shape_arg) && !is_cons(shape_arg) && !is_nil(shape_arg)) {
        *err_out = tensor_type_raise(interp, "Tensor: shape must be an array or proper list of non-negative integers");
        return false;
    }
    if (rank > usz.max / usz.sizeof) {
        *err_out = raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: shape rank is too large", null);
        return false;
    }

    usz* shape = null;
    if (rank > 0) {
        shape = (usz*)mem::malloc(usz.sizeof * rank);
        if (shape == null) {
            *err_out = raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: out of memory while reading shape", null);
            return false;
        }
    }

    for (usz i = 0; i < rank; i++) {
        Value* dim = tensor_container_nth(shape_arg, i);
        if (!is_int(dim) || dim.int_val < 0) {
            if (shape != null) mem::free(shape);
            *err_out = tensor_type_raise(interp, "Tensor: shape dimensions must be non-negative integers");
            return false;
        }
        if ((ulong)dim.int_val > (ulong)usz.max) {
            if (shape != null) mem::free(shape);
            *err_out = tensor_type_raise(interp, "Tensor: shape dimensions exceed supported size");
            return false;
        }
        shape[i] = (usz)dim.int_val;
    }

    *out_shape = shape;
    *out_rank = rank;
    return true;
}

fn bool tensor_value_to_double(Value* value, double* out) @inline {
    if (out == null || value == null) return false;
    if (value.tag == DOUBLE) {
        *out = value.double_val;
        return true;
    }
    if (value.tag == INT) {
        *out = (double)value.int_val;
        return true;
    }
    return false;
}

fn Value* tensor_fill_from_data_arg(TensorVal* tensor, Value* data_arg, Interp* interp) {
    if (tensor == null) return tensor_runtime_raise(interp, "Tensor: tensor backing storage is missing");
    if (tensor.byte_len > 0 && tensor.data == null) return tensor_runtime_raise(interp, "Tensor: tensor data storage is missing");
    double* dst = (double*)tensor.data;

    double scalar = 0.0;
    if (tensor_value_to_double(data_arg, &scalar)) {
        for (usz i = 0; i < tensor.element_count; i++) dst[i] = scalar;
        return null;
    }

    usz data_count = 0;
    if (!tensor_container_count(data_arg, &data_count)) {
        return tensor_type_raise(interp, "Tensor: data must be a number, array, or proper list of numbers");
    }
    if (!is_array(data_arg) && !is_cons(data_arg) && !is_nil(data_arg)) {
        return tensor_type_raise(interp, "Tensor: data must be a number, array, or proper list of numbers");
    }
    if (data_count != tensor.element_count) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/constructor-data-length", "Tensor: data length does not match shape product", null);
    }

    for (usz i = 0; i < data_count; i++) {
        double value = 0.0;
        if (!tensor_value_to_double(tensor_container_nth(data_arg, i), &value)) {
            return tensor_type_raise(interp, "Tensor: data elements must be numbers");
        }
        dst[i] = value;
    }
    return null;
}

fn bool tensor_has_valid_storage(TensorVal* tensor) @inline {
    if (tensor == null) return false;
    if (tensor.kind != TENSOR_PAYLOAD_CONCRETE) return false;
    if (tensor.rank > 0 && (tensor.shape == null || tensor.strides == null)) return false;
    if (tensor.byte_len > 0 && tensor.data == null) return false;
    return true;
}

fn bool tensor_has_valid_metadata(TensorVal* tensor) @inline {
    if (tensor == null) return false;
    if (tensor.rank > 0 && (tensor.shape == null || tensor.strides == null)) return false;
    return true;
}

fn bool tensor_same_shape(TensorVal* a, TensorVal* b) {
    if (a == null || b == null || a.rank != b.rank) return false;
    for (usz i = 0; i < a.rank; i++) {
        if (a.shape[i] != b.shape[i]) return false;
    }
    return true;
}

fn bool tensor_axis_is_listed(usz axis, usz* axes, usz axis_count) {
    for (usz i = 0; i < axis_count; i++) {
        if (axes[i] == axis) return true;
    }
    return false;
}

fn Value* tensor_parse_axis_value(
    Value* axis_value,
    TensorVal* tensor,
    Interp* interp,
    char[] label,
    usz* prior_axes,
    usz prior_axis_count,
    usz* out_axis
) {
    if (out_axis == null) return tensor_runtime_raise(interp, "contract: axis parser state is missing");
    *out_axis = 0;
    if (tensor == null) return tensor_runtime_raise(interp, "contract: tensor backing storage is missing");
    if (tensor.rank > (usz)long.max) return raise_error(interp, "contract: tensor rank is too large");
    if (!is_int(axis_value)) {
        return tensor_type_raise(interp, "contract: axes must be integers");
    }

    long rank = (long)tensor.rank;
    long axis = axis_value.int_val;
    if (axis < 0) axis += rank;
    if (axis < 0 || axis >= rank) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/axis-out-of-range", label, null);
    }
    usz normalized = (usz)axis;
    if (tensor_axis_is_listed(normalized, prior_axes, prior_axis_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/axis-duplicate", "contract: duplicate contracted axis", null);
    }
    *out_axis = normalized;
    return null;
}

fn Value* tensor_parse_axes(Value* axes_arg, TensorVal* tensor, Interp* interp, char[] label, usz** out_axes, usz* out_count) {
    if (out_axes == null || out_count == null) return tensor_runtime_raise(interp, "contract: axis parser state is missing");
    *out_axes = null;
    *out_count = 0;
    if (tensor == null) return tensor_runtime_raise(interp, "contract: tensor backing storage is missing");

    usz count = 0;
    if (!tensor_container_count(axes_arg, &count) ||
        (!is_array(axes_arg) && !is_cons(axes_arg) && !is_nil(axes_arg))) {
        return tensor_type_raise(interp, "contract: axes must be arrays or proper lists of integers");
    }
    if (count > 0 && count > usz.max / usz.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: axis list is too large", null);
    }

    usz* axes = null;
    if (count > 0) {
        axes = (usz*)mem::malloc(usz.sizeof * count);
        if (axes == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: failed to allocate axis list", null);
        }
    }

    for (usz i = 0; i < count; i++) {
        Value* axis_value = tensor_container_nth(axes_arg, i);
        usz normalized = 0;
        Value* err = tensor_parse_axis_value(axis_value, tensor, interp, label, axes, i, &normalized);
        if (err != null) {
            if (axes != null) mem::free(axes);
            return err;
        }
        axes[i] = normalized;
    }

    *out_axes = axes;
    *out_count = count;
    return null;
}

fn bool tensor_axis_pair_container(Value* value) @inline {
    return is_array(value) || is_cons(value);
}

fn Value* tensor_parse_axis_pair_values(
    Value* left_axis_value,
    Value* right_axis_value,
    TensorVal* left,
    TensorVal* right,
    Interp* interp,
    usz* left_axes,
    usz* right_axes,
    usz index
) {
    usz left_axis = 0;
    Value* err = tensor_parse_axis_value(left_axis_value, left, interp, "contract: left axis out of range", left_axes, index, &left_axis);
    if (err != null) return err;

    usz right_axis = 0;
    err = tensor_parse_axis_value(right_axis_value, right, interp, "contract: right axis out of range", right_axes, index, &right_axis);
    if (err != null) return err;

    left_axes[index] = left_axis;
    right_axes[index] = right_axis;
    return null;
}

fn Value* tensor_parse_axis_pairs(Value* pairs_arg, TensorVal* left, TensorVal* right, Interp* interp, usz** out_left_axes, usz** out_right_axes, usz* out_count) {
    if (out_left_axes == null || out_right_axes == null || out_count == null) {
        return tensor_runtime_raise(interp, "contract: axis parser state is missing");
    }
    *out_left_axes = null;
    *out_right_axes = null;
    *out_count = 0;

    usz input_count = 0;
    if (!tensor_container_count(pairs_arg, &input_count) ||
        (!is_array(pairs_arg) && !is_cons(pairs_arg) && !is_nil(pairs_arg))) {
        return tensor_type_raise(interp, "contract: axis pairs must be an array or proper list");
    }

    bool single_pair = false;
    if (input_count == 2) {
        Value* first = tensor_container_nth(pairs_arg, 0);
        Value* second = tensor_container_nth(pairs_arg, 1);
        single_pair = is_int(first) && is_int(second);
    }

    usz count = single_pair ? 1 : input_count;
    if (count > 0 && count > usz.max / usz.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: axis list is too large", null);
    }

    usz* left_axes = null;
    usz* right_axes = null;
    if (count > 0) {
        left_axes = (usz*)mem::malloc(usz.sizeof * count);
        if (left_axes == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: failed to allocate axis list", null);
        }
        right_axes = (usz*)mem::malloc(usz.sizeof * count);
        if (right_axes == null) {
            mem::free(left_axes);
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: failed to allocate axis list", null);
        }
    }

    if (single_pair) {
        Value* err = tensor_parse_axis_pair_values(
            tensor_container_nth(pairs_arg, 0),
            tensor_container_nth(pairs_arg, 1),
            left, right, interp, left_axes, right_axes, 0);
        if (err != null) {
            if (left_axes != null) mem::free(left_axes);
            if (right_axes != null) mem::free(right_axes);
            return err;
        }
    } else {
        for (usz i = 0; i < count; i++) {
            Value* pair = tensor_container_nth(pairs_arg, i);
            usz pair_count = 0;
            if (!tensor_axis_pair_container(pair) || !tensor_container_count(pair, &pair_count) || pair_count != 2) {
                if (left_axes != null) mem::free(left_axes);
                if (right_axes != null) mem::free(right_axes);
                return tensor_type_raise(interp, "contract: axis pairs must contain two-integer arrays or proper lists");
            }
            Value* err = tensor_parse_axis_pair_values(
                tensor_container_nth(pair, 0),
                tensor_container_nth(pair, 1),
                left, right, interp, left_axes, right_axes, i);
            if (err != null) {
                if (left_axes != null) mem::free(left_axes);
                if (right_axes != null) mem::free(right_axes);
                return err;
            }
        }
    }

    *out_left_axes = left_axes;
    *out_right_axes = right_axes;
    *out_count = count;
    return null;
}

fn bool tensor_value_storage_aliases(Value* source_value, TensorVal* dest) {
    if (source_value == null || dest == null || dest.data == null || !is_tensor(source_value)) return false;
    TensorVal* source = source_value.tensor_val;
    return tensor_has_valid_storage(source) && source.byte_len > 0 && source.data == dest.data;
}

fn bool tensor_expr_storage_aliases(Value* source_expr, TensorVal* dest) {
    if (source_expr == null || dest == null || dest.data == null || !is_tensor(source_expr)) return false;

    TensorVal* source = source_expr.tensor_val;
    if (source == null || source.kind == TENSOR_PAYLOAD_CONCRETE) {
        return tensor_value_storage_aliases(source_expr, dest);
    }

    switch (source.kind) {
        case TENSOR_PAYLOAD_MAP:
            return tensor_expr_storage_aliases(source.map_left, dest) || tensor_expr_storage_aliases(source.map_right, dest);
        case TENSOR_PAYLOAD_CONTRACT:
            return tensor_expr_storage_aliases(source.contract_left, dest) || tensor_expr_storage_aliases(source.contract_right, dest);
        default:
            return false;
    }
}

fn bool tensor_contract_sources_alias_destination(TensorVal* source, TensorVal* dest) {
    if (source == null || dest == null || source.kind != TENSOR_PAYLOAD_CONTRACT) return false;
    return tensor_expr_storage_aliases(source.contract_left, dest) || tensor_expr_storage_aliases(source.contract_right, dest);
}

fn bool tensor_rank2_row_major(TensorVal* tensor) @inline {
    return tensor != null &&
        tensor.rank == 2 &&
        tensor.shape != null &&
        tensor.strides != null &&
        tensor.strides[1] == 1 &&
        tensor.strides[0] == tensor.shape[1];
}

fn bool tensor_usz_fits_blas_int(usz value) @inline {
    return value <= 2147483647;
}

fn bool tensor_contract_try_blas_dgemm(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    TensorVal* out_tensor
) {
    if (axis_count != 1 || left_axes == null || right_axes == null) return false;
    if (!tensor_rank2_row_major(left) || !tensor_rank2_row_major(right) || !tensor_rank2_row_major(out_tensor)) return false;

    usz left_axis = left_axes[0];
    usz right_axis = right_axes[0];
    if (left_axis > 1 || right_axis > 1) return false;

    usz left_free_axis = left_axis == 0 ? 1 : 0;
    usz right_free_axis = right_axis == 0 ? 1 : 0;
    usz m = left.shape[left_free_axis];
    usz k = left.shape[left_axis];
    usz n = right.shape[right_free_axis];
    if (m == 0 || n == 0 || k == 0) return false;
    if (right.shape[right_axis] != k || out_tensor.shape[0] != m || out_tensor.shape[1] != n) return false;
    if (!tensor_usz_fits_blas_int(m) || !tensor_usz_fits_blas_int(n) || !tensor_usz_fits_blas_int(k) ||
        !tensor_usz_fits_blas_int(left.strides[0]) || !tensor_usz_fits_blas_int(right.strides[0]) ||
        !tensor_usz_fits_blas_int(out_tensor.strides[0])) {
        return false;
    }

    int trans_a = left_axis == 0 ? 1 : 0;
    int trans_b = right_axis == 1 ? 1 : 0;
    return omni_tensor_backend_blas_dgemm(
        m,
        n,
        k,
        trans_a,
        trans_b,
        (double*)left.data,
        left.strides[0],
        (double*)right.data,
        right.strides[0],
        (double*)out_tensor.data,
        out_tensor.strides[0]
    ) != 0;
}

fn Value* tensor_copy_into_destination(Value* source_value, Value* dest_value, Interp* interp) {
    if (!is_tensor(source_value)) return tensor_type_raise(interp, "realize: expected Tensor source");
    if (!is_tensor(dest_value)) return tensor_type_raise(interp, "realize: destination must be a Tensor");

    TensorVal* source = source_value.tensor_val;
    TensorVal* dest = dest_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "realize: source tensor metadata is missing");
    if (!tensor_has_valid_storage(dest)) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    if (source.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (source.element_count != dest.element_count || source.byte_len != dest.byte_len || !tensor_same_shape(source, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }

    if (source.kind != TENSOR_PAYLOAD_CONCRETE) {
        if (tensor_contract_sources_alias_destination(source, dest)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/aliasing", "contract: destination aliases source tensor", null);
        }
        usz[1] scalar_shape = { 0 };
        usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
        Value* staged_value = make_tensor_double_uninitialized_checked(interp, shape_slice, "realize: failed to allocate tensor result");
        if (staged_value == null || staged_value.tag == ERROR) return staged_value;
        bool keep_staged = false;
        defer if (!keep_staged) {
            boundary_cleanup_materialized_value(null, staged_value, interp.current_scope);
        }
        if (staged_value.tag != TENSOR || staged_value.tensor_val == null || !tensor_has_valid_storage(staged_value.tensor_val)) {
            return tensor_runtime_raise(interp, "realize: tensor result backing storage is missing");
        }
        Value* err = tensor_eval_expr_into(source, staged_value.tensor_val, interp);
        if (err != null) return err;
        if (source.byte_len > 0) mem::copy(dest.data, staged_value.tensor_val.data, source.byte_len);
        return dest_value;
    }
    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
    if (source_value == dest_value || source == dest || source.data == dest.data) return dest_value;
    if (source.byte_len > 0) mem::copy(dest.data, source.data, source.byte_len);
    return dest_value;
}

fn Value* tensor_map_dtype_raise(Interp* interp) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: function result dtype mismatch", null);
}

fn bool tensor_map_callable(Value* func) @inline {
    if (func == null) return false;
    switch (func.tag) {
        case CLOSURE:
        case PRIMITIVE:
        case PARTIAL_PRIM:
        case METHOD_TABLE:
        case CONTINUATION:
            return true;
        default:
            return false;
    }
}

fn Value* tensor_map_apply_unary(Value* func, double a, double* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* arg_list = make_cons_or_error(interp, make_double(interp, a), make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!tensor_value_to_double(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary(Value* func, double a, double b, double* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* tail = make_cons_or_error(interp, make_double(interp, b), make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* arg_list = make_cons_or_error(interp, make_double(interp, a), tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;

    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!tensor_value_to_double(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_source_storage(Value* source, TensorVal** out_tensor, double* out_scalar, Interp* interp) {
    if (out_tensor == null || out_scalar == null) return tensor_runtime_raise(interp, "map: tensor source state is missing");
    *out_tensor = null;
    *out_scalar = 0.0;
    if (is_tensor(source)) {
        TensorVal* tensor = source.tensor_val;
        if (!tensor_has_valid_metadata(tensor)) return tensor_runtime_raise(interp, "map: tensor metadata is missing");
        if (tensor.dtype != TENSOR_DTYPE_DOUBLE) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: tensor dtype mismatch", null);
        }
        *out_tensor = tensor;
        return null;
    }
    if (tensor_value_to_double(source, out_scalar)) return null;
    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers");
}

fn Value* tensor_map_result_shape(
    TensorVal* left_tensor,
    TensorVal* right_tensor,
    bool is_binary,
    Interp* interp,
    usz** out_shape,
    usz* out_rank
) {
    if (out_shape == null || out_rank == null) return tensor_runtime_raise(interp, "map: result shape state is missing");
    *out_shape = null;
    *out_rank = 0;

    TensorVal* shape_left = left_tensor;
    TensorVal* shape_right = is_binary ? right_tensor : null;
    if (shape_left == null && shape_right == null) {
        return tensor_type_raise(interp, "map: expected at least one Tensor input");
    }

    if (shape_left != null && !tensor_has_valid_metadata(shape_left)) return tensor_runtime_raise(interp, "map: tensor metadata is missing");
    if (shape_right != null && !tensor_has_valid_metadata(shape_right)) return tensor_runtime_raise(interp, "map: tensor metadata is missing");

    if (shape_left == null || shape_right == null) {
        TensorVal* source = shape_left != null ? shape_left : shape_right;
        if (source == null) return tensor_type_raise(interp, "map: expected at least one Tensor input");
        if (source.rank == 0) return null;
        if (source.rank > usz.max / usz.sizeof) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "map: result rank is too large", null);
        }
        usz* result_shape = (usz*)mem::malloc(usz.sizeof * source.rank);
        if (result_shape == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "map: failed to allocate tensor expression", null);
        }
        mem::copy(result_shape, source.shape, usz.sizeof * source.rank);
        *out_shape = result_shape;
        *out_rank = source.rank;
        return null;
    }

    usz result_rank = shape_left.rank > shape_right.rank ? shape_left.rank : shape_right.rank;
    if (result_rank > 0 && result_rank > usz.max / usz.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "map: result rank is too large", null);
    }

    usz* result_shape = null;
    if (result_rank > 0) {
        result_shape = (usz*)mem::malloc(usz.sizeof * result_rank);
        if (result_shape == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "map: failed to allocate tensor expression", null);
        }
    }

    for (usz i = 0; i < result_rank; i++) {
        usz result_axis = result_rank - 1 - i;
        usz left_dim = i < shape_left.rank ? shape_left.shape[shape_left.rank - 1 - i] : 1;
        usz right_dim = i < shape_right.rank ? shape_right.shape[shape_right.rank - 1 - i] : 1;
        if (left_dim != right_dim && left_dim != 1 && right_dim != 1) {
            if (result_shape != null) mem::free(result_shape);
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
        }
        result_shape[result_axis] = left_dim == 1 ? right_dim : left_dim;
    }

    *out_shape = result_shape;
    *out_rank = result_rank;
    return null;
}

fn bool tensor_map_operand_matches_output(TensorVal* operand, TensorVal* out_tensor) {
    if (operand == null || out_tensor == null) return false;
    if (!tensor_has_valid_metadata(operand) || !tensor_has_valid_metadata(out_tensor)) return false;
    if (operand.rank > out_tensor.rank) return false;
    if (operand.rank == 0) return true;

    usz axis_delta = out_tensor.rank - operand.rank;
    for (usz out_axis = 0; out_axis < out_tensor.rank; out_axis++) {
        if (out_axis < axis_delta) continue;
        usz operand_axis = out_axis - axis_delta;
        usz operand_dim = operand.shape[operand_axis];
        usz out_dim = out_tensor.shape[out_axis];
        if (operand_dim != out_dim && operand_dim != 1) return false;
    }
    return true;
}

fn bool tensor_map_operand_offset(TensorVal* operand, TensorVal* out_tensor, usz out_index, usz* out_offset) {
    if (operand == null || out_tensor == null || out_offset == null) return false;
    if (!tensor_has_valid_metadata(operand) || !tensor_has_valid_metadata(out_tensor)) return false;
    if (operand.rank > out_tensor.rank) return false;
    if (operand.rank == 0) {
        *out_offset = 0;
        return true;
    }

    usz axis_delta = out_tensor.rank - operand.rank;
    usz offset = 0;
    for (usz out_axis = 0; out_axis < out_tensor.rank; out_axis++) {
        if (out_axis < axis_delta) continue;
        usz operand_axis = out_axis - axis_delta;
        usz operand_dim = operand.shape[operand_axis];
        usz out_dim = out_tensor.shape[out_axis];
        if (operand_dim != out_dim && operand_dim != 1) return false;
        if (operand_dim == 1) continue;

        usz coord = (out_index / out_tensor.strides[out_axis]) % out_tensor.shape[out_axis];
        usz axis_offset = 0;
        if (!tensor_checked_mul_usz(coord, operand.strides[operand_axis], &axis_offset)) return false;
        if (axis_offset > usz.max - offset) return false;
        offset += axis_offset;
    }
    *out_offset = offset;
    return true;
}

fn Value* tensor_map_eval_into(
    Value* func,
    TensorVal* left_tensor,
    double left_scalar,
    TensorVal* right_tensor,
    double right_scalar,
    bool is_binary,
    TensorVal* out_tensor,
    Interp* interp
) {
    if (!tensor_map_callable(func)) return tensor_type_raise(interp, "map: function must be callable");
    if (!tensor_has_valid_storage(out_tensor)) return tensor_runtime_raise(interp, "map: tensor result backing storage is missing");
    if (out_tensor.dtype != TENSOR_DTYPE_DOUBLE) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: tensor dtype mismatch", null);
    }
    if (left_tensor == null && (!is_binary || right_tensor == null)) {
        return tensor_type_raise(interp, "map: expected at least one Tensor input");
    }
    if (left_tensor != null) {
        if (!tensor_has_valid_storage(left_tensor)) return tensor_runtime_raise(interp, "map: tensor backing storage is missing");
        if (left_tensor.dtype != out_tensor.dtype) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: tensor dtype mismatch", null);
        }
        if (!tensor_map_operand_matches_output(left_tensor, out_tensor)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
        }
    }
    if (right_tensor != null) {
        if (!tensor_has_valid_storage(right_tensor)) return tensor_runtime_raise(interp, "map: tensor backing storage is missing");
        if (right_tensor.dtype != out_tensor.dtype) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: tensor dtype mismatch", null);
        }
        if (!tensor_map_operand_matches_output(right_tensor, out_tensor)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
        }
    }

    double* out = (double*)out_tensor.data;
    double* left_data = left_tensor != null ? (double*)left_tensor.data : null;
    double* right_data = right_tensor != null ? (double*)right_tensor.data : null;

    for (usz i = 0; i < out_tensor.element_count; i++) {
        double result = 0.0;
        Value* err = null;
        if (!is_binary) {
            usz left_offset = 0;
            if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
            }
            err = tensor_map_apply_unary(func, left_data[left_offset], &result, interp);
        } else {
            double a = left_scalar;
            double b = right_scalar;
            if (left_tensor != null) {
                usz left_offset = 0;
                if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                    return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                }
                a = left_data[left_offset];
            }
            if (right_tensor != null) {
                usz right_offset = 0;
                if (!tensor_map_operand_offset(right_tensor, out_tensor, i, &right_offset)) {
                    return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                }
                b = right_data[right_offset];
            }
            err = tensor_map_apply_binary(func, a, b, &result, interp);
        }
        if (err != null) return err;
        out[i] = result;
    }
    return null;
}

fn Value* make_tensor_map_expr(
    Value* func,
    Value* left_value,
    TensorVal* left_tensor,
    double left_scalar,
    Value* right_value,
    TensorVal* right_tensor,
    double right_scalar,
    bool is_binary,
    usz[] result_shape,
    Interp* interp
) {
    TensorVal* tensor = tensor_alloc_expression_payload_checked(TENSOR_DTYPE_DOUBLE, result_shape, TENSOR_PAYLOAD_MAP, "map: failed to allocate tensor expression");
    if (tensor == null) return raise_error(interp, "map: failed to allocate tensor expression");
    tensor.map_func = func;
    tensor.map_left = left_tensor != null ? left_value : null;
    tensor.map_right = is_binary && right_tensor != null ? right_value : null;
    tensor.map_left_scalar = left_tensor != null ? 0.0 : left_scalar;
    tensor.map_right_scalar = right_tensor != null ? 0.0 : right_scalar;
    tensor.map_is_binary = is_binary;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return raise_error(interp, "map: failed to allocate tensor expression");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* prim_tensor_map(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2 && args.len != 3) {
        return primitive_arity_raise(interp, "__tensor-map", "2 or 3 arguments: function, tensor [input]", args.len);
    }

    Value* func = args[0];
    if (!tensor_map_callable(func)) return tensor_type_raise(interp, "map: function must be callable");

    TensorVal* left_tensor = null;
    TensorVal* right_tensor = null;
    double left_scalar = 0.0;
    double right_scalar = 0.0;
    Value* err = tensor_map_source_storage(args[1], &left_tensor, &left_scalar, interp);
    if (err != null) return err;

    usz* result_shape = null;
    usz result_rank = 0;
    if (args.len == 3) {
        err = tensor_map_source_storage(args[2], &right_tensor, &right_scalar, interp);
        if (err != null) return err;
        if (left_tensor == null && right_tensor == null) {
            return tensor_type_raise(interp, "map: expected at least one Tensor input");
        }
        if (left_tensor != null && right_tensor != null && left_tensor.dtype != right_tensor.dtype) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: tensor dtype mismatch", null);
        }
        err = tensor_map_result_shape(left_tensor, right_tensor, true, interp, &result_shape, &result_rank);
        if (err != null) return err;
    } else if (left_tensor == null) {
        return tensor_type_raise(interp, "map: expected Tensor input");
    } else {
        err = tensor_map_result_shape(left_tensor, null, false, interp, &result_shape, &result_rank);
        if (err != null) return err;
    }

    defer if (result_shape != null) mem::free(result_shape);
    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
    return make_tensor_map_expr(func, args[1], left_tensor, left_scalar, args.len == 3 ? args[2] : null, right_tensor, right_scalar, args.len == 3, shape_slice, interp);
}

fn Value* tensor_contract_result_shape(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz left_axis_count,
    usz* right_axes,
    usz right_axis_count,
    Interp* interp,
    usz** out_shape,
    usz* out_rank
) {
    if (out_shape == null || out_rank == null) return tensor_runtime_raise(interp, "contract: result shape state is missing");
    *out_shape = null;
    *out_rank = 0;
    if (left == null || right == null) return tensor_runtime_raise(interp, "contract: tensor backing storage is missing");

    if (left_axis_count != right_axis_count) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/rank-mismatch", "contract: axis list length mismatch", null);
    }
    for (usz i = 0; i < left_axis_count; i++) {
        if (left.shape[left_axes[i]] != right.shape[right_axes[i]]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: contracted dimensions must match", null);
        }
    }

    usz result_rank = (left.rank - left_axis_count) + (right.rank - right_axis_count);
    if (result_rank > 0 && result_rank > usz.max / usz.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: result rank is too large", null);
    }

    usz* result_shape = null;
    if (result_rank > 0) {
        result_shape = (usz*)mem::malloc(usz.sizeof * result_rank);
        if (result_shape == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: failed to allocate result shape", null);
        }
    }

    usz result_axis = 0;
    for (usz axis = 0; axis < left.rank; axis++) {
        if (!tensor_axis_is_listed(axis, left_axes, left_axis_count)) {
            result_shape[result_axis++] = left.shape[axis];
        }
    }
    for (usz axis = 0; axis < right.rank; axis++) {
        if (!tensor_axis_is_listed(axis, right_axes, right_axis_count)) {
            result_shape[result_axis++] = right.shape[axis];
        }
    }

    *out_shape = result_shape;
    *out_rank = result_rank;
    return null;
}

fn Value* tensor_contract_validate_destination(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    TensorVal* out_tensor,
    Interp* interp
) {
    usz expected_rank = (left.rank - axis_count) + (right.rank - axis_count);
    if (out_tensor.rank != expected_rank) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: destination shape mismatch", null);
    }
    for (usz i = 0; i < axis_count; i++) {
        if (left.shape[left_axes[i]] != right.shape[right_axes[i]]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: contracted dimensions must match", null);
        }
    }

    usz result_axis = 0;
    for (usz axis = 0; axis < left.rank; axis++) {
        if (tensor_axis_is_listed(axis, left_axes, axis_count)) continue;
        if (out_tensor.shape[result_axis] != left.shape[axis]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: destination shape mismatch", null);
        }
        result_axis++;
    }
    for (usz axis = 0; axis < right.rank; axis++) {
        if (tensor_axis_is_listed(axis, right_axes, axis_count)) continue;
        if (out_tensor.shape[result_axis] != right.shape[axis]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: destination shape mismatch", null);
        }
        result_axis++;
    }
    return null;
}

fn Value* tensor_contract_eval_into(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    TensorVal* out_tensor,
    Interp* interp
) {
    if (!tensor_has_valid_storage(left) || !tensor_has_valid_storage(right) || !tensor_has_valid_storage(out_tensor)) {
        return tensor_runtime_raise(interp, "contract: tensor backing storage is missing");
    }
    if (left.dtype != TENSOR_DTYPE_DOUBLE || right.dtype != TENSOR_DTYPE_DOUBLE || out_tensor.dtype != TENSOR_DTYPE_DOUBLE) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "contract: tensor dtype mismatch", null);
    }
    Value* err = tensor_contract_validate_destination(left, right, left_axes, right_axes, axis_count, out_tensor, interp);
    if (err != null) return err;
    if (out_tensor.data != null && (out_tensor.data == left.data || out_tensor.data == right.data)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/aliasing", "contract: destination aliases source tensor", null);
    }
    if (tensor_contract_try_blas_dgemm(left, right, left_axes, right_axes, axis_count, out_tensor)) {
        return null;
    }

    usz contracted_count = 1;
    for (usz i = 0; i < axis_count; i++) {
        if (!tensor_checked_mul_usz(contracted_count, left.shape[left_axes[i]], &contracted_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: contracted axis product overflow", null);
        }
    }

    double* out = (double*)out_tensor.data;
    double* left_data = (double*)left.data;
    double* right_data = (double*)right.data;

    for (usz out_index = 0; out_index < out_tensor.element_count; out_index++) {
        usz left_base_offset = 0;
        usz right_base_offset = 0;
        usz result_pos = 0;

        for (usz axis = 0; axis < left.rank; axis++) {
            if (tensor_axis_is_listed(axis, left_axes, axis_count)) continue;
            usz coord = out_tensor.rank == 0 ? 0 : (out_index / out_tensor.strides[result_pos]) % out_tensor.shape[result_pos];
            left_base_offset += coord * left.strides[axis];
            result_pos++;
        }
        for (usz axis = 0; axis < right.rank; axis++) {
            if (tensor_axis_is_listed(axis, right_axes, axis_count)) continue;
            usz coord = out_tensor.rank == 0 ? 0 : (out_index / out_tensor.strides[result_pos]) % out_tensor.shape[result_pos];
            right_base_offset += coord * right.strides[axis];
            result_pos++;
        }

        double sum = 0.0;
        if (contracted_count == 0) {
            out[out_index] = 0.0;
            continue;
        }
        for (usz contract_index = 0; contract_index < contracted_count; contract_index++) {
            usz left_offset = left_base_offset;
            usz right_offset = right_base_offset;
            usz remainder = contract_index;
            for (usz i = axis_count; i > 0; i--) {
                usz axis_pos = i - 1;
                usz dim = left.shape[left_axes[axis_pos]];
                usz coord = remainder % dim;
                remainder /= dim;
                left_offset += coord * left.strides[left_axes[axis_pos]];
                right_offset += coord * right.strides[right_axes[axis_pos]];
            }
            sum += left_data[left_offset] * right_data[right_offset];
        }
        out[out_index] = sum;
    }
    return null;
}

fn Value* make_tensor_contract_expr(
    Value* left_value,
    Value* right_value,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    usz[] result_shape,
    Interp* interp
) {
    TensorVal* tensor = tensor_alloc_expression_payload_checked(TENSOR_DTYPE_DOUBLE, result_shape, TENSOR_PAYLOAD_CONTRACT, "contract: failed to allocate tensor expression");
    if (tensor == null) return raise_error(interp, "contract: failed to allocate tensor expression");
    tensor.contract_left = left_value;
    tensor.contract_right = right_value;
    tensor.contract_axis_count = axis_count;
    if (axis_count > 0) {
        if (axis_count > usz.max / usz.sizeof) {
            tensor_free_payload(tensor);
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: axis list is too large", null);
        }
        tensor.contract_left_axes = (usz*)mem::malloc(usz.sizeof * axis_count);
        if (tensor.contract_left_axes == null) {
            tensor_free_payload(tensor);
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: failed to allocate axis list", null);
        }
        tensor.contract_right_axes = (usz*)mem::malloc(usz.sizeof * axis_count);
        if (tensor.contract_right_axes == null) {
            tensor_free_payload(tensor);
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: failed to allocate axis list", null);
        }
        mem::copy(tensor.contract_left_axes, left_axes, usz.sizeof * axis_count);
        mem::copy(tensor.contract_right_axes, right_axes, usz.sizeof * axis_count);
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return raise_error(interp, "contract: failed to allocate tensor expression");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* prim_contract(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 3 && args.len != 4) {
        return primitive_arity_raise(interp, "contract", "3 or 4 arguments: left right axis-pairs | left right left-axes right-axes", args.len);
    }
    if (!is_tensor(args[0]) || !is_tensor(args[1])) {
        return tensor_type_raise(interp, "contract: expected Tensor operands");
    }

    TensorVal* left = args[0].tensor_val;
    TensorVal* right = args[1].tensor_val;
    if (!tensor_has_valid_metadata(left) || !tensor_has_valid_metadata(right)) {
        return tensor_runtime_raise(interp, "contract: tensor metadata is missing");
    }
    if (left.dtype != TENSOR_DTYPE_DOUBLE || right.dtype != TENSOR_DTYPE_DOUBLE || left.dtype != right.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "contract: tensor dtype mismatch", null);
    }

    usz* left_axes = null;
    usz left_axis_count = 0;
    Value* err = null;
    if (args.len == 3) {
        usz* right_axes = null;
        usz right_axis_count = 0;
        err = tensor_parse_axis_pairs(args[2], left, right, interp, &left_axes, &right_axes, &left_axis_count);
        if (err != null) return err;
        right_axis_count = left_axis_count;
        defer if (left_axes != null) mem::free(left_axes);
        defer if (right_axes != null) mem::free(right_axes);

        usz* result_shape = null;
        usz result_rank = 0;
        err = tensor_contract_result_shape(left, right, left_axes, left_axis_count, right_axes, right_axis_count, interp, &result_shape, &result_rank);
        if (err != null) return err;
        defer if (result_shape != null) mem::free(result_shape);

        usz[1] scalar_shape = { 0 };
        usz[] result_shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
        return make_tensor_contract_expr(args[0], args[1], left_axes, right_axes, left_axis_count, result_shape_slice, interp);
    }

    err = tensor_parse_axes(args[2], left, interp, "contract: left axis out of range", &left_axes, &left_axis_count);
    if (err != null) return err;
    defer if (left_axes != null) mem::free(left_axes);

    usz* right_axes = null;
    usz right_axis_count = 0;
    err = tensor_parse_axes(args[3], right, interp, "contract: right axis out of range", &right_axes, &right_axis_count);
    if (err != null) return err;
    defer if (right_axes != null) mem::free(right_axes);

    usz* result_shape = null;
    usz result_rank = 0;
    err = tensor_contract_result_shape(left, right, left_axes, left_axis_count, right_axes, right_axis_count, interp, &result_shape, &result_rank);
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    usz[1] scalar_shape = { 0 };
    usz[] result_shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
    return make_tensor_contract_expr(args[0], args[1], left_axes, right_axes, left_axis_count, result_shape_slice, interp);
}

fn Value* tensor_realize_to_value(Value* source_value, Interp* interp) {
    if (!is_tensor(source_value)) return tensor_type_raise(interp, "realize: expected Tensor source");
    TensorVal* source = source_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "realize: source tensor metadata is missing");
    if (source.kind == TENSOR_PAYLOAD_CONCRETE) {
        if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        return source_value;
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_double_uninitialized_checked(interp, shape_slice, "realize: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out && out_value != source_value) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || out_value.tensor_val == null || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "realize: tensor result backing storage is missing");
    }
    Value* err = tensor_eval_expr_into(source, out_value.tensor_val, interp);
    if (err != null) return err;
    keep_out = true;
    return out_value;
}

fn Value* tensor_expr_resolve_concrete(Value* source_value, TensorVal** out_tensor, Value** out_value, Interp* interp, char[] label) {
    if (out_tensor == null) return tensor_runtime_raise(interp, label);
    *out_tensor = null;
    if (out_value != null) *out_value = null;
    if (!is_tensor(source_value)) return tensor_type_raise(interp, label);
    Value* realized = tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    if (!is_tensor(realized) || !tensor_has_valid_storage(realized.tensor_val)) {
        return tensor_runtime_raise(interp, label);
    }
    *out_tensor = realized.tensor_val;
    if (out_value != null) *out_value = realized;
    return null;
}

fn Value* tensor_eval_expr_into(TensorVal* source, TensorVal* dest, Interp* interp) {
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "realize: source tensor metadata is missing");
    if (!tensor_has_valid_storage(dest)) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    if (source.dtype != dest.dtype) return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    if (source.element_count != dest.element_count || source.byte_len != dest.byte_len || !tensor_same_shape(source, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }

    switch (source.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
            if (source == dest || source.data == dest.data) return null;
            if (source.byte_len > 0) mem::copy(dest.data, source.data, source.byte_len);
            return null;
        case TENSOR_PAYLOAD_MAP: {
            TensorVal* left_tensor = null;
            TensorVal* right_tensor = null;
            Value* left_value = null;
            Value* right_value = null;
            Value* err = null;
            defer if (left_value != null && left_value != source.map_left) {
                boundary_cleanup_materialized_value(null, left_value, interp.current_scope);
            }
            defer if (right_value != null && right_value != source.map_right) {
                boundary_cleanup_materialized_value(null, right_value, interp.current_scope);
            }
            if (source.map_left != null) {
                err = tensor_expr_resolve_concrete(source.map_left, &left_tensor, &left_value, interp, "map: tensor backing storage is missing");
                if (err != null) return err;
            }
            if (source.map_is_binary && source.map_right != null) {
                err = tensor_expr_resolve_concrete(source.map_right, &right_tensor, &right_value, interp, "map: tensor backing storage is missing");
                if (err != null) return err;
            }
            return tensor_map_eval_into(
                source.map_func,
                left_tensor,
                source.map_left_scalar,
                right_tensor,
                source.map_right_scalar,
                source.map_is_binary,
                dest,
                interp
            );
        }
        case TENSOR_PAYLOAD_CONTRACT: {
            TensorVal* left_tensor = null;
            TensorVal* right_tensor = null;
            Value* left_value = null;
            Value* right_value = null;
            defer if (left_value != null && left_value != source.contract_left) {
                boundary_cleanup_materialized_value(null, left_value, interp.current_scope);
            }
            defer if (right_value != null && right_value != source.contract_right) {
                boundary_cleanup_materialized_value(null, right_value, interp.current_scope);
            }
            Value* err = tensor_expr_resolve_concrete(source.contract_left, &left_tensor, &left_value, interp, "contract: tensor backing storage is missing");
            if (err != null) return err;
            err = tensor_expr_resolve_concrete(source.contract_right, &right_tensor, &right_value, interp, "contract: tensor backing storage is missing");
            if (err != null) return err;
            return tensor_contract_eval_into(
                left_tensor,
                right_tensor,
                source.contract_left_axes,
                source.contract_right_axes,
                source.contract_axis_count,
                dest,
                interp
            );
        }
    }
}

fn Value* tensor_ref_value(Value* tensor_value, Value* index_arg, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "ref: expected Tensor");
    Value* concrete_value = tensor_realize_to_value(tensor_value, interp);
    if (concrete_value == null || concrete_value.tag == ERROR) return concrete_value;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    TensorVal* tensor = concrete_value.tensor_val;
    if (tensor == null) return tensor_runtime_raise(interp, "ref: tensor backing storage is missing");
    if (tensor.rank > 0 && (tensor.shape == null || tensor.strides == null)) {
        return tensor_runtime_raise(interp, "ref: tensor shape/stride storage is missing");
    }
    if (tensor.byte_len > 0 && tensor.data == null) return tensor_runtime_raise(interp, "ref: tensor data storage is missing");

    usz index_count = 0;
    if (!tensor_container_count(index_arg, &index_count) ||
        (!is_array(index_arg) && !is_cons(index_arg) && !is_nil(index_arg))) {
        return tensor_type_raise(interp, "ref: tensor requires array or proper list index");
    }
    if (index_count != tensor.rank) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/rank-mismatch", "ref: tensor index rank mismatch", null);
    }

    usz offset = 0;
    for (usz axis = 0; axis < tensor.rank; axis++) {
        Value* index_value = tensor_container_nth(index_arg, axis);
        if (!is_int(index_value)) return tensor_type_raise(interp, "ref: tensor indices must be integers");
        if (tensor.shape[axis] > (usz)long.max) return raise_error(interp, "ref: tensor dimension is too large to index");

        long dim = (long)tensor.shape[axis];
        long idx = index_value.int_val;
        if (idx < 0) idx += dim;
        if (idx < 0 || idx >= dim) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/axis-out-of-range", "ref: tensor index out of bounds", null);
        }

        usz axis_offset = 0;
        if (!tensor_checked_mul_usz((usz)idx, tensor.strides[axis], &axis_offset)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/axis-out-of-range", "ref: tensor index offset overflow", null);
        }
        if (axis_offset > usz.max - offset) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/axis-out-of-range", "ref: tensor index offset overflow", null);
        }
        offset += axis_offset;
    }

    if (offset >= tensor.element_count) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/axis-out-of-range", "ref: tensor index out of bounds", null);
    }
    switch (tensor.dtype) {
        case TENSOR_DTYPE_DOUBLE:
            return make_double(interp, ((double*)tensor.data)[offset]);
    }
}

fn Value* prim_tensor(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 3) return primitive_arity_raise(interp, "Tensor", "3 arguments: dtype, shape, data-or-scalar", args.len);
    if (!tensor_dtype_arg_is_double(args[0], interp)) {
        return tensor_type_raise(interp, "Tensor: first argument must be Double");
    }

    usz* shape = null;
    usz rank = 0;
    Value* err = null;
    if (!tensor_parse_shape(args[1], interp, &shape, &rank, &err)) return err;
    defer if (shape != null) mem::free(shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = rank == 0 ? scalar_shape[0:0] : shape[0:rank];
    Value* tensor = make_tensor_double_uninitialized_checked(interp, shape_slice, "Tensor: out of memory");
    if (tensor == null || tensor.tag == ERROR) return tensor;
    bool keep_tensor = false;
    defer if (!keep_tensor) {
        boundary_cleanup_materialized_value(null, tensor, interp.current_scope);
    }
    if (tensor.tag != TENSOR || tensor.tensor_val == null) {
        return tensor_runtime_raise(interp, "Tensor: constructor produced invalid backing storage");
    }

    err = tensor_fill_from_data_arg(tensor.tensor_val, args[2], interp);
    if (err != null) return err;
    keep_tensor = true;
    return tensor;
}

fn Value* prim_realize(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) {
        return primitive_arity_raise(interp, "realize", "1 or 2 arguments: expr [out]", args.len);
    }

    if (args.len == 1) {
        if (is_tensor(args[0])) return tensor_realize_to_value(args[0], interp);
        return tensor_type_raise(interp, "realize: expected Tensor source");
    }

    if (!is_tensor(args[1])) return tensor_type_raise(interp, "realize: destination must be a Tensor");
    TensorVal* dest = args[1].tensor_val;
    if (!tensor_has_valid_storage(dest)) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }

    if (is_number(args[0])) {
        Value* err = tensor_fill_from_data_arg(dest, args[0], interp);
        if (err != null) return err;
        return args[1];
    }
    return tensor_copy_into_destination(args[0], args[1], interp);
}

fn Value* prim_tensor_p(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "tensor?: expected 1 argument");
    return is_tensor(args[0]) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_dtype(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_tensor(args[0])) return raise_error(interp, "dtype: expected Tensor");
    TensorVal* tensor = args[0].tensor_val;
    if (tensor == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "dtype: tensor backing storage is missing");
    return make_symbol(interp, tensor_dtype_symbol(tensor.dtype, interp));
}

fn Value* prim_rank(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_tensor(args[0])) return raise_error(interp, "rank: expected Tensor");
    TensorVal* tensor = args[0].tensor_val;
    if (tensor == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "rank: tensor backing storage is missing");
    if (tensor.rank > (usz)long.max) return raise_error(interp, "rank: tensor rank is too large");
    return make_int(interp, (long)tensor.rank);
}

fn Value* prim_shape(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_tensor(args[0])) return raise_error(interp, "shape: expected Tensor");
    TensorVal* tensor = args[0].tensor_val;
    if (tensor == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "shape: tensor backing storage is missing");

    Value* out = make_array_checked(interp, tensor.rank, "shape: failed to allocate result array");
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != ARRAY || out.array_val == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "shape: result array backing storage is missing");
    }

    for (usz i = 0; i < tensor.rank; i++) {
        if (tensor.shape == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "shape: tensor shape storage is missing");
        if (tensor.shape[i] > (usz)long.max) return raise_error(interp, "shape: tensor dimension is too large");
        out.array_val.items[i] = make_int(interp, (long)tensor.shape[i]);
    }
    out.array_val.length = tensor.rank;
    return out;
}
