module lisp;

import std::core::mem;
import main;

fn bool is_tensor(Value* v) @inline {
    return v != null && v.tag == TENSOR;
}

fn bool tensor_dtype_arg_is_double(Value* dtype_arg, Interp* interp) @inline {
    if (dtype_arg == null) return false;
    if (dtype_arg.tag == SYMBOL) return dtype_arg.sym_val == interp.sym_Double;
    if (dtype_arg.tag == PRIMITIVE) return primitive_name_equals(dtype_arg, "Double");
    if (dtype_arg.tag == METHOD_TABLE && dtype_arg.method_table_val != null) {
        return dtype_arg.method_table_val.name == interp.sym_Double;
    }
    return false;
}

fn bool tensor_dtype_arg_is_big_float(Value* dtype_arg, Interp* interp) @inline {
    if (dtype_arg == null) return false;
    if (dtype_arg.tag == SYMBOL) return dtype_arg.sym_val == interp.sym_BigFloat;
    if (dtype_arg.tag == PRIMITIVE) return primitive_name_equals(dtype_arg, "BigFloat");
    if (dtype_arg.tag == METHOD_TABLE && dtype_arg.method_table_val != null) {
        return dtype_arg.method_table_val.name == interp.sym_BigFloat;
    }
    return false;
}

fn bool tensor_dtype_arg_is_big_integer(Value* dtype_arg, Interp* interp) @inline {
    if (dtype_arg == null) return false;
    if (dtype_arg.tag == SYMBOL) return dtype_arg.sym_val == interp.sym_BigInteger;
    if (dtype_arg.tag == PRIMITIVE) return primitive_name_equals(dtype_arg, "BigInteger");
    if (dtype_arg.tag == METHOD_TABLE && dtype_arg.method_table_val != null) {
        return dtype_arg.method_table_val.name == interp.sym_BigInteger;
    }
    return false;
}

fn bool tensor_dtype_arg_to_dtype(Value* dtype_arg, Interp* interp, TensorDType* out) @inline {
    if (out == null) return false;
    if (tensor_dtype_arg_is_double(dtype_arg, interp)) {
        *out = TENSOR_DTYPE_DOUBLE;
        return true;
    }
    if (tensor_dtype_arg_is_big_integer(dtype_arg, interp)) {
        *out = TENSOR_DTYPE_BIG_INTEGER;
        return true;
    }
    if (tensor_dtype_arg_is_big_float(dtype_arg, interp)) {
        *out = TENSOR_DTYPE_BIG_FLOAT;
        return true;
    }
    return false;
}

fn Value* tensor_type_raise(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", "type/arg-mismatch", message, null);
}

fn void* big_integer_handle_from_value(Value* v) {
    if (v == null) return null;
    switch (v.tag) {
        case INT:
            return omni_big_integer_from_i64(v.int_val);
        case BIG_INTEGER:
            return omni_big_integer_clone(v.big_integer_val);
        default:
            return null;
    }
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

fn bool tensor_value_is_container(Value* value) @inline {
    return is_array(value) || is_cons(value) || is_nil(value);
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
    return try_numeric_to_double(value, out);
}

fn bool tensor_value_is_real_numeric_leaf(Value* value) @inline {
    return value != null &&
        (value.tag == INT || value.tag == DOUBLE || value.tag == BIG_INTEGER || value.tag == BIG_FLOAT);
}

fn bool tensor_infer_shape_from_value(
    Value* value,
    Interp* interp,
    usz** out_shape,
    usz* out_rank,
    usz* out_count,
    Value** err_out
) {
    if (out_shape == null || out_rank == null || out_count == null || err_out == null) return false;
    *out_shape = null;
    *out_rank = 0;
    *out_count = 0;
    *err_out = null;

    if (tensor_value_is_real_numeric_leaf(value)) {
        *out_count = 1;
        return true;
    }

    if (!tensor_value_is_container(value)) {
        *err_out = tensor_type_raise(interp, "Tensor: inferred data must be numeric or nested arrays/lists of numbers");
        return false;
    }

    usz count = 0;
    if (!tensor_container_count(value, &count)) {
        *err_out = tensor_type_raise(interp, "Tensor: inferred data must be a proper nested array/list");
        return false;
    }

    if (count == 0) {
        usz* shape = (usz*)mem::malloc(usz.sizeof);
        if (shape == null) {
            *err_out = raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: failed to infer shape", null);
            return false;
        }
        shape[0] = 0;
        *out_shape = shape;
        *out_rank = 1;
        *out_count = 0;
        return true;
    }

    usz* child_shape = null;
    usz child_rank = 0;
    usz child_count = 0;
    if (!tensor_infer_shape_from_value(tensor_container_nth(value, 0), interp, &child_shape, &child_rank, &child_count, err_out)) {
        return false;
    }
    defer if (child_shape != null) mem::free(child_shape);

    if (child_count > 0 && count > usz.max / child_count) {
        *err_out = raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: inferred shape product overflow", null);
        return false;
    }
    usz total_count = count * child_count;
    usz rank = child_rank + 1;
    if (rank == 0 || rank > usz.max / usz.sizeof) {
        *err_out = raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: inferred rank is too large", null);
        return false;
    }

    for (usz i = 1; i < count; i++) {
        usz* next_shape = null;
        usz next_rank = 0;
        usz next_count = 0;
        if (!tensor_infer_shape_from_value(tensor_container_nth(value, i), interp, &next_shape, &next_rank, &next_count, err_out)) {
            return false;
        }
        if (next_rank != child_rank || next_count != child_count) {
            if (next_shape != null) mem::free(next_shape);
            *err_out = raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "Tensor: inferred data must be rectangular", null);
            return false;
        }
        for (usz axis = 0; axis < child_rank; axis++) {
            if (next_shape == null || child_shape == null || next_shape[axis] != child_shape[axis]) {
                if (next_shape != null) mem::free(next_shape);
                *err_out = raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "Tensor: inferred data must be rectangular", null);
                return false;
            }
        }
        if (next_shape != null) mem::free(next_shape);
    }

    usz* shape = (usz*)mem::malloc(usz.sizeof * rank);
    if (shape == null) {
        *err_out = raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: failed to infer shape", null);
        return false;
    }
    shape[0] = count;
    for (usz axis = 0; axis < child_rank; axis++) {
        shape[axis + 1] = child_shape[axis];
    }

    *out_shape = shape;
    *out_rank = rank;
    *out_count = total_count;
    return true;
}

fn Value* tensor_store_value_at(TensorVal* tensor, usz index, Value* value, Interp* interp, char[] label) {
    if (tensor == null || index >= tensor.element_count) return tensor_runtime_raise(interp, label);
    if (tensor.byte_len > 0 && tensor.data == null) return tensor_runtime_raise(interp, label);
    switch (tensor.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double scalar = 0.0;
            if (!tensor_value_to_double(value, &scalar)) return tensor_type_raise(interp, "Tensor: data elements must be real numbers representable as Double");
            ((double*)tensor.data)[index] = scalar;
            return null;
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void* handle = big_integer_handle_from_value(value);
            if (handle == null) return tensor_type_raise(interp, "Tensor: data elements must be exact integers representable as BigInteger");
            void** values = (void**)tensor.data;
            if (values[index] != null) omni_big_integer_free(values[index]);
            values[index] = handle;
            return null;
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void* handle = big_float_handle_from_value(value);
            if (handle == null) return tensor_type_raise(interp, "Tensor: data elements must be real numbers representable as BigFloat");
            void** values = (void**)tensor.data;
            if (values[index] != null) omni_big_float_free(values[index]);
            values[index] = handle;
            return null;
        }
    }
}

fn Value* tensor_flatten_inferred_data(TensorVal* tensor, Value* value, usz* offset, Interp* interp) {
    if (tensor == null || offset == null) return tensor_runtime_raise(interp, "Tensor: inferred tensor storage is missing");

    if (tensor_value_is_real_numeric_leaf(value)) {
        Value* err = tensor_store_value_at(tensor, *offset, value, interp, "Tensor: inferred tensor storage is missing");
        if (err != null) return err;
        *offset += 1;
        return null;
    }

    usz count = 0;
    if (!tensor_container_count(value, &count)) {
        return tensor_type_raise(interp, "Tensor: inferred data must be numeric or nested arrays/lists of numbers");
    }
    for (usz i = 0; i < count; i++) {
        Value* err = tensor_flatten_inferred_data(tensor, tensor_container_nth(value, i), offset, interp);
        if (err != null) return err;
    }
    return null;
}

fn Value* tensor_fill_scalar(TensorVal* tensor, Value* data_arg, Interp* interp) {
    if (tensor == null) return tensor_runtime_raise(interp, "Tensor: tensor backing storage is missing");
    switch (tensor.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double scalar = 0.0;
            if (!tensor_value_to_double(data_arg, &scalar)) return null;
            double* dst = (double*)tensor.data;
            for (usz i = 0; i < tensor.element_count; i++) dst[i] = scalar;
            return data_arg;
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void* source = big_float_handle_from_value(data_arg);
            if (source == null) return null;
            defer omni_big_float_free(source);
            void** staged = (void**)mem::malloc(tensor.byte_len);
            if (tensor.byte_len > 0 && staged == null) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: failed to allocate BigFloat fill storage", null);
            }
            defer if (staged != null) mem::free(staged);
            for (usz i = 0; i < tensor.element_count; i++) staged[i] = null;
            bool ok = true;
            for (usz i = 0; i < tensor.element_count; i++) {
                staged[i] = omni_big_float_clone(source);
                if (staged[i] == null) {
                    ok = false;
                    break;
                }
            }
            if (!ok) {
                for (usz i = 0; i < tensor.element_count; i++) {
                    if (staged[i] != null) omni_big_float_free(staged[i]);
                }
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: failed to allocate BigFloat fill value", null);
            }
            tensor_big_float_clear_data(tensor);
            void** dst = (void**)tensor.data;
            for (usz i = 0; i < tensor.element_count; i++) {
                dst[i] = staged[i];
                staged[i] = null;
            }
            return data_arg;
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void* source = big_integer_handle_from_value(data_arg);
            if (source == null) return null;
            defer omni_big_integer_free(source);
            void** staged = (void**)mem::malloc(tensor.byte_len);
            if (tensor.byte_len > 0 && staged == null) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: failed to allocate BigInteger fill storage", null);
            }
            defer if (staged != null) mem::free(staged);
            for (usz i = 0; i < tensor.element_count; i++) staged[i] = null;
            bool ok = true;
            for (usz i = 0; i < tensor.element_count; i++) {
                staged[i] = omni_big_integer_clone(source);
                if (staged[i] == null) {
                    ok = false;
                    break;
                }
            }
            if (!ok) {
                for (usz i = 0; i < tensor.element_count; i++) {
                    if (staged[i] != null) omni_big_integer_free(staged[i]);
                }
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: failed to allocate BigInteger fill value", null);
            }
            tensor_big_integer_clear_data(tensor);
            void** dst = (void**)tensor.data;
            for (usz i = 0; i < tensor.element_count; i++) {
                dst[i] = staged[i];
                staged[i] = null;
            }
            return data_arg;
        }
    }
}

fn Value* tensor_fill_from_data_arg(TensorVal* tensor, Value* data_arg, Interp* interp) {
    if (tensor == null) return tensor_runtime_raise(interp, "Tensor: tensor backing storage is missing");
    if (tensor.byte_len > 0 && tensor.data == null) return tensor_runtime_raise(interp, "Tensor: tensor data storage is missing");

    Value* scalar_fill = tensor_fill_scalar(tensor, data_arg, interp);
    if (scalar_fill != null) {
        if (scalar_fill.tag == ERROR) return scalar_fill;
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
        Value* err = tensor_store_value_at(tensor, i, tensor_container_nth(data_arg, i), interp, "Tensor: tensor data storage is missing");
        if (err != null) return err;
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

fn bool tensor_rank1_contiguous(TensorVal* tensor) @inline {
    return tensor != null &&
        tensor.rank == 1 &&
        tensor.shape != null &&
        tensor.strides != null &&
        tensor.strides[0] == 1;
}

fn bool tensor_rank0_scalar(TensorVal* tensor) @inline {
    return tensor != null &&
        tensor.rank == 0 &&
        tensor.element_count == 1 &&
        tensor.data != null;
}

fn bool tensor_usz_fits_blas_int(usz value) @inline {
    return value <= 2147483647;
}

fn bool tensor_contract_try_blas_ddot(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    TensorVal* out_tensor
) {
    if (axis_count != 1 || left_axes == null || right_axes == null) return false;
    if (!tensor_rank1_contiguous(left) || !tensor_rank1_contiguous(right) || !tensor_rank0_scalar(out_tensor)) return false;
    if (left_axes[0] != 0 || right_axes[0] != 0) return false;
    usz n = left.shape[0];
    if (n == 0 || right.shape[0] != n || !tensor_usz_fits_blas_int(n)) return false;
    return omni_tensor_backend_blas_ddot(
        n,
        (double*)left.data,
        (double*)right.data,
        (double*)out_tensor.data
    ) != 0;
}

fn bool tensor_contract_try_blas_dger(
    TensorVal* left,
    TensorVal* right,
    usz axis_count,
    TensorVal* out_tensor
) {
    if (axis_count != 0) return false;
    if (!tensor_rank1_contiguous(left) || !tensor_rank1_contiguous(right) || !tensor_rank2_row_major(out_tensor)) return false;
    usz m = left.shape[0];
    usz n = right.shape[0];
    if (m == 0 || n == 0 || out_tensor.shape[0] != m || out_tensor.shape[1] != n) return false;
    if (!tensor_usz_fits_blas_int(m) || !tensor_usz_fits_blas_int(n) || !tensor_usz_fits_blas_int(out_tensor.strides[0])) return false;
    return omni_tensor_backend_blas_dger(
        m,
        n,
        (double*)left.data,
        (double*)right.data,
        (double*)out_tensor.data,
        out_tensor.strides[0]
    ) != 0;
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

fn bool tensor_contract_try_blas_dgemv(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    TensorVal* out_tensor
) {
    if (axis_count != 1 || left_axes == null || right_axes == null) return false;

    usz left_axis = left_axes[0];
    usz right_axis = right_axes[0];

    if (tensor_rank2_row_major(left) && tensor_rank1_contiguous(right) && tensor_rank1_contiguous(out_tensor)) {
        if (left_axis > 1 || right_axis != 0) return false;
        usz rows = left.shape[0];
        usz cols = left.shape[1];
        usz contracted = left.shape[left_axis];
        usz result_len = left.shape[left_axis == 0 ? 1 : 0];
        if (contracted == 0 || result_len == 0 || right.shape[0] != contracted || out_tensor.shape[0] != result_len) return false;
        if (!tensor_usz_fits_blas_int(rows) || !tensor_usz_fits_blas_int(cols) || !tensor_usz_fits_blas_int(left.strides[0])) return false;
        int trans_a = left_axis == 0 ? 1 : 0;
        return omni_tensor_backend_blas_dgemv(
            rows,
            cols,
            trans_a,
            (double*)left.data,
            left.strides[0],
            (double*)right.data,
            (double*)out_tensor.data
        ) != 0;
    }

    if (tensor_rank1_contiguous(left) && tensor_rank2_row_major(right) && tensor_rank1_contiguous(out_tensor)) {
        if (left_axis != 0 || right_axis > 1) return false;
        usz rows = right.shape[0];
        usz cols = right.shape[1];
        usz contracted = right.shape[right_axis];
        usz result_len = right.shape[right_axis == 0 ? 1 : 0];
        if (contracted == 0 || result_len == 0 || left.shape[0] != contracted || out_tensor.shape[0] != result_len) return false;
        if (!tensor_usz_fits_blas_int(rows) || !tensor_usz_fits_blas_int(cols) || !tensor_usz_fits_blas_int(right.strides[0])) return false;
        int trans_a = right_axis == 0 ? 1 : 0;
        return omni_tensor_backend_blas_dgemv(
            rows,
            cols,
            trans_a,
            (double*)right.data,
            right.strides[0],
            (double*)left.data,
            (double*)out_tensor.data
        ) != 0;
    }

    return false;
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
        Value* staged_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "realize: failed to allocate tensor result");
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
        if (!tensor_copy_concrete_storage(dest, staged_value.tensor_val)) {
            return tensor_runtime_raise(interp, "realize: failed to copy staged tensor result");
        }
        return dest_value;
    }
    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
    if (source_value == dest_value || source == dest || source.data == dest.data) return dest_value;
    if (!tensor_copy_concrete_storage(dest, source)) {
        return tensor_runtime_raise(interp, "realize: failed to copy tensor storage");
    }
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

fn Value* tensor_map_apply_unary_big_integer(Value* func, void* a, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null) return tensor_runtime_raise(interp, "map: BigInteger tensor input is missing");
    Value* arg = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(a), "map: failed to allocate tensor call argument");
    if (arg == null || arg.tag == ERROR) return arg;
    Value* arg_list = make_cons_or_error(interp, arg, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_integer_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary_big_integer(Value* func, void* a, void* b, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null || b == null) return tensor_runtime_raise(interp, "map: BigInteger tensor input is missing");
    Value* right = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(b), "map: failed to allocate tensor call argument");
    if (right == null || right.tag == ERROR) return right;
    Value* tail = make_cons_or_error(interp, right, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* left = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(a), "map: failed to allocate tensor call argument");
    if (left == null || left.tag == ERROR) return left;
    Value* arg_list = make_cons_or_error(interp, left, tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;

    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_integer_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_unary_big_float(Value* func, void* a, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null) return tensor_runtime_raise(interp, "map: BigFloat tensor input is missing");
    Value* arg = make_big_float_from_owned_handle(interp, omni_big_float_clone(a), "map: failed to allocate tensor call argument");
    if (arg == null || arg.tag == ERROR) return arg;
    Value* arg_list = make_cons_or_error(interp, arg, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_float_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary_big_float(Value* func, void* a, void* b, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null || b == null) return tensor_runtime_raise(interp, "map: BigFloat tensor input is missing");
    Value* right = make_big_float_from_owned_handle(interp, omni_big_float_clone(b), "map: failed to allocate tensor call argument");
    if (right == null || right.tag == ERROR) return right;
    Value* tail = make_cons_or_error(interp, right, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* left = make_big_float_from_owned_handle(interp, omni_big_float_clone(a), "map: failed to allocate tensor call argument");
    if (left == null || left.tag == ERROR) return left;
    Value* arg_list = make_cons_or_error(interp, left, tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;

    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_float_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_source_storage(Value* source, TensorVal** out_tensor, Interp* interp) {
    if (out_tensor == null) return tensor_runtime_raise(interp, "map: tensor source state is missing");
    *out_tensor = null;
    if (is_tensor(source)) {
        TensorVal* tensor = source.tensor_val;
        if (!tensor_has_valid_metadata(tensor)) return tensor_runtime_raise(interp, "map: tensor metadata is missing");
        *out_tensor = tensor;
        return null;
    }
    if (tensor_value_is_real_numeric_leaf(source)) return null;
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
    void* left_big_integer_scalar,
    void* left_big_float_scalar,
    TensorVal* right_tensor,
    double right_scalar,
    void* right_big_integer_scalar,
    void* right_big_float_scalar,
    bool is_binary,
    TensorVal* out_tensor,
    Interp* interp
) {
    if (!tensor_map_callable(func)) return tensor_type_raise(interp, "map: function must be callable");
    if (!tensor_has_valid_storage(out_tensor)) return tensor_runtime_raise(interp, "map: tensor result backing storage is missing");
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

    switch (out_tensor.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
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
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** out = (void**)out_tensor.data;
            void** left_data = left_tensor != null ? (void**)left_tensor.data : null;
            void** right_data = right_tensor != null ? (void**)right_tensor.data : null;

            for (usz i = 0; i < out_tensor.element_count; i++) {
                void* result = null;
                Value* err = null;
                if (!is_binary) {
                    usz left_offset = 0;
                    if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                    }
                    err = tensor_map_apply_unary_big_integer(func, left_data[left_offset], &result, interp);
                } else {
                    void* a = left_big_integer_scalar;
                    void* b = right_big_integer_scalar;
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
                    err = tensor_map_apply_binary_big_integer(func, a, b, &result, interp);
                }
                if (err != null) {
                    if (result != null) omni_big_integer_free(result);
                    return err;
                }
                if (out[i] != null) omni_big_integer_free(out[i]);
                out[i] = result;
            }
            return null;
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** out = (void**)out_tensor.data;
            void** left_data = left_tensor != null ? (void**)left_tensor.data : null;
            void** right_data = right_tensor != null ? (void**)right_tensor.data : null;

            for (usz i = 0; i < out_tensor.element_count; i++) {
                void* result = null;
                Value* err = null;
                if (!is_binary) {
                    usz left_offset = 0;
                    if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                    }
                    err = tensor_map_apply_unary_big_float(func, left_data[left_offset], &result, interp);
                } else {
                    void* a = left_big_float_scalar;
                    void* b = right_big_float_scalar;
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
                    err = tensor_map_apply_binary_big_float(func, a, b, &result, interp);
                }
                if (err != null) {
                    if (result != null) omni_big_float_free(result);
                    return err;
                }
                if (out[i] != null) omni_big_float_free(out[i]);
                out[i] = result;
            }
            return null;
        }
    }
}

fn Value* make_tensor_map_expr(
    Value* func,
    Value* left_value,
    TensorVal* left_tensor,
    Value* right_value,
    TensorVal* right_tensor,
    bool is_binary,
    TensorDType dtype,
    usz[] result_shape,
    Interp* interp
) {
    TensorVal* tensor = tensor_alloc_expression_payload_checked(dtype, result_shape, TENSOR_PAYLOAD_MAP, "map: failed to allocate tensor expression");
    if (tensor == null) return raise_error(interp, "map: failed to allocate tensor expression");
    tensor.map_func = func;
    tensor.map_left = left_tensor != null ? left_value : null;
    tensor.map_right = is_binary && right_tensor != null ? right_value : null;
    tensor.map_is_binary = is_binary;
    if (left_tensor == null) {
        switch (dtype) {
            case TENSOR_DTYPE_DOUBLE:
                if (!tensor_value_to_double(left_value, &tensor.map_left_scalar)) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers representable as Double");
                }
            case TENSOR_DTYPE_BIG_INTEGER:
                tensor.map_left_big_integer_scalar = big_integer_handle_from_value(left_value);
                if (tensor.map_left_big_integer_scalar == null) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be exact integers");
                }
            case TENSOR_DTYPE_BIG_FLOAT:
                tensor.map_left_big_float_scalar = big_float_handle_from_value(left_value);
                if (tensor.map_left_big_float_scalar == null) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers");
                }
        }
    }
    if (is_binary && right_tensor == null) {
        switch (dtype) {
            case TENSOR_DTYPE_DOUBLE:
                if (!tensor_value_to_double(right_value, &tensor.map_right_scalar)) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers representable as Double");
                }
            case TENSOR_DTYPE_BIG_INTEGER:
                tensor.map_right_big_integer_scalar = big_integer_handle_from_value(right_value);
                if (tensor.map_right_big_integer_scalar == null) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be exact integers");
                }
            case TENSOR_DTYPE_BIG_FLOAT:
                tensor.map_right_big_float_scalar = big_float_handle_from_value(right_value);
                if (tensor.map_right_big_float_scalar == null) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers");
                }
        }
    }

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
    Value* err = tensor_map_source_storage(args[1], &left_tensor, interp);
    if (err != null) return err;

    usz* result_shape = null;
    usz result_rank = 0;
    TensorDType result_dtype = left_tensor != null ? left_tensor.dtype : TENSOR_DTYPE_DOUBLE;
    if (args.len == 3) {
        err = tensor_map_source_storage(args[2], &right_tensor, interp);
        if (err != null) return err;
        if (left_tensor == null && right_tensor == null) {
            return tensor_type_raise(interp, "map: expected at least one Tensor input");
        }
        if (left_tensor != null && right_tensor != null && left_tensor.dtype != right_tensor.dtype) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: tensor dtype mismatch", null);
        }
        if (left_tensor == null && right_tensor != null) result_dtype = right_tensor.dtype;
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
    return make_tensor_map_expr(func, args[1], left_tensor, args.len == 3 ? args[2] : null, right_tensor, args.len == 3, result_dtype, shape_slice, interp);
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
    if (left.dtype != right.dtype || left.dtype != out_tensor.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "contract: tensor dtype mismatch", null);
    }
    Value* err = tensor_contract_validate_destination(left, right, left_axes, right_axes, axis_count, out_tensor, interp);
    if (err != null) return err;
    if (out_tensor.data != null && (out_tensor.data == left.data || out_tensor.data == right.data)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/aliasing", "contract: destination aliases source tensor", null);
    }
    if (left.dtype == TENSOR_DTYPE_DOUBLE) {
        if (tensor_contract_try_blas_dger(left, right, axis_count, out_tensor)) {
            return null;
        }
        if (tensor_contract_try_blas_ddot(left, right, left_axes, right_axes, axis_count, out_tensor)) {
            return null;
        }
        if (tensor_contract_try_blas_dgemm(left, right, left_axes, right_axes, axis_count, out_tensor)) {
            return null;
        }
        if (tensor_contract_try_blas_dgemv(left, right, left_axes, right_axes, axis_count, out_tensor)) {
            return null;
        }
    }

    usz contracted_count = 1;
    for (usz i = 0; i < axis_count; i++) {
        if (!tensor_checked_mul_usz(contracted_count, left.shape[left_axes[i]], &contracted_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "contract: contracted axis product overflow", null);
        }
    }

    switch (left.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
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
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** out = (void**)out_tensor.data;
            void** left_data = (void**)left.data;
            void** right_data = (void**)right.data;

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

                void* sum = omni_big_integer_from_i64(0);
                if (sum == null) return raise_error(interp, "contract: failed to allocate BigInteger accumulator");
                bool ok = true;
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
                    if (left_data[left_offset] == null || right_data[right_offset] == null) {
                        ok = false;
                        break;
                    }
                    void* product = omni_big_integer_binary(left_data[left_offset], right_data[right_offset], BIG_INTEGER_OP_MUL);
                    if (product == null) {
                        ok = false;
                        break;
                    }
                    void* next_sum = omni_big_integer_binary(sum, product, BIG_INTEGER_OP_ADD);
                    omni_big_integer_free(product);
                    if (next_sum == null) {
                        ok = false;
                        break;
                    }
                    omni_big_integer_free(sum);
                    sum = next_sum;
                }
                if (!ok) {
                    omni_big_integer_free(sum);
                    return raise_error(interp, "contract: failed to compute BigInteger contraction");
                }
                if (out[out_index] != null) omni_big_integer_free(out[out_index]);
                out[out_index] = sum;
            }
            return null;
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** out = (void**)out_tensor.data;
            void** left_data = (void**)left.data;
            void** right_data = (void**)right.data;

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

                void* sum = omni_big_float_from_i64(0);
                if (sum == null) return raise_error(interp, "contract: failed to allocate BigFloat accumulator");
                bool ok = true;
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
                    if (left_data[left_offset] == null || right_data[right_offset] == null) {
                        ok = false;
                        break;
                    }
                    void* product = omni_big_float_binary(left_data[left_offset], right_data[right_offset], BIG_FLOAT_OP_MUL);
                    if (product == null) {
                        ok = false;
                        break;
                    }
                    void* next_sum = omni_big_float_binary(sum, product, BIG_FLOAT_OP_ADD);
                    omni_big_float_free(product);
                    if (next_sum == null) {
                        ok = false;
                        break;
                    }
                    omni_big_float_free(sum);
                    sum = next_sum;
                }
                if (!ok) {
                    omni_big_float_free(sum);
                    return raise_error(interp, "contract: failed to compute BigFloat contraction");
                }
                if (out[out_index] != null) omni_big_float_free(out[out_index]);
                out[out_index] = sum;
            }
            return null;
        }
    }
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
    TensorVal* source = left_value.tensor_val;
    TensorVal* tensor = tensor_alloc_expression_payload_checked(source.dtype, result_shape, TENSOR_PAYLOAD_CONTRACT, "contract: failed to allocate tensor expression");
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
    if (left.dtype != right.dtype) {
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
    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "realize: failed to allocate tensor result");
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
            if (!tensor_copy_concrete_storage(dest, source)) {
                return tensor_runtime_raise(interp, "realize: failed to copy tensor storage");
            }
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
                source.map_left_big_integer_scalar,
                source.map_left_big_float_scalar,
                right_tensor,
                source.map_right_scalar,
                source.map_right_big_integer_scalar,
                source.map_right_big_float_scalar,
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
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** values = (void**)tensor.data;
            if (values[offset] == null) return tensor_runtime_raise(interp, "ref: tensor BigInteger element is missing");
            void* cloned = omni_big_integer_clone(values[offset]);
            return make_big_integer_from_owned_handle(interp, cloned, "ref: failed to allocate BigInteger tensor element");
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** values = (void**)tensor.data;
            if (values[offset] == null) return tensor_runtime_raise(interp, "ref: tensor BigFloat element is missing");
            void* cloned = omni_big_float_clone(values[offset]);
            return make_big_float_from_owned_handle(interp, cloned, "ref: failed to allocate BigFloat tensor element");
        }
    }
}

fn Value* tensor_to_flat_array_value(Value* tensor_value, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "Array: expected Tensor");
    Value* concrete_value = tensor_realize_to_value(tensor_value, interp);
    if (concrete_value == null || concrete_value.tag == ERROR) return concrete_value;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    TensorVal* tensor = concrete_value.tensor_val;
    if (!tensor_has_valid_storage(tensor)) return tensor_runtime_raise(interp, "Array: tensor backing storage is missing");
    Value* result = make_array_checked(interp, tensor.element_count, "Array: failed to allocate tensor conversion result");
    if (result == null || result.tag == ERROR) return result;
    if (result.tag != ARRAY || result.array_val == null) {
        return tensor_runtime_raise(interp, "Array: result array backing storage is missing");
    }

    for (usz i = 0; i < tensor.element_count; i++) {
        Value* elem = null;
        switch (tensor.dtype) {
            case TENSOR_DTYPE_DOUBLE:
                elem = make_double(interp, ((double*)tensor.data)[i]);
            case TENSOR_DTYPE_BIG_INTEGER: {
                void** values = (void**)tensor.data;
                if (values[i] == null) return tensor_runtime_raise(interp, "Array: tensor BigInteger element is missing");
                elem = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(values[i]), "Array: failed to allocate BigInteger tensor element");
            }
            case TENSOR_DTYPE_BIG_FLOAT: {
                void** values = (void**)tensor.data;
                if (values[i] == null) return tensor_runtime_raise(interp, "Array: tensor BigFloat element is missing");
                elem = make_big_float_from_owned_handle(interp, omni_big_float_clone(values[i]), "Array: failed to allocate BigFloat tensor element");
            }
        }
        if (elem == null || elem.tag == ERROR) return elem;
        Value* promoted = array_promote_or_error(elem, interp);
        if (promoted == null || promoted.tag == ERROR) return promoted;
        result.array_val.items[i] = promoted;
    }
    result.array_val.length = tensor.element_count;
    return result;
}

fn Value* tensor_to_flat_list_value(Value* tensor_value, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "List: expected Tensor");
    Value* concrete_value = tensor_realize_to_value(tensor_value, interp);
    if (concrete_value == null || concrete_value.tag == ERROR) return concrete_value;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    TensorVal* tensor = concrete_value.tensor_val;
    if (!tensor_has_valid_storage(tensor)) return tensor_runtime_raise(interp, "List: tensor backing storage is missing");
    Value* result = make_nil(interp);
    usz remaining = tensor.element_count;
    while (remaining > 0) {
        remaining--;
        Value* elem = null;
        switch (tensor.dtype) {
            case TENSOR_DTYPE_DOUBLE:
                elem = make_double(interp, ((double*)tensor.data)[remaining]);
            case TENSOR_DTYPE_BIG_INTEGER: {
                void** values = (void**)tensor.data;
                if (values[remaining] == null) return tensor_runtime_raise(interp, "List: tensor BigInteger element is missing");
                elem = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(values[remaining]), "List: failed to allocate BigInteger tensor element");
            }
            case TENSOR_DTYPE_BIG_FLOAT: {
                void** values = (void**)tensor.data;
                if (values[remaining] == null) return tensor_runtime_raise(interp, "List: tensor BigFloat element is missing");
                elem = make_big_float_from_owned_handle(interp, omni_big_float_clone(values[remaining]), "List: failed to allocate BigFloat tensor element");
            }
        }
        if (elem == null || elem.tag == ERROR) return elem;
        result = make_cons_or_error(interp, elem, result, "List: failed to allocate tensor conversion result");
        if (result == null || result.tag == ERROR) return result;
    }
    return result;
}

fn Value* tensor_construct_from_shape_and_data(Value* dtype_arg, Value* shape_arg, Value* data_arg, Interp* interp) {
    TensorDType dtype = TENSOR_DTYPE_DOUBLE;
    if (!tensor_dtype_arg_to_dtype(dtype_arg, interp, &dtype)) {
        return tensor_type_raise(interp, "Tensor: first argument must be Double or BigFloat");
    }

    usz* shape = null;
    usz rank = 0;
    Value* err = null;
    if (!tensor_parse_shape(shape_arg, interp, &shape, &rank, &err)) return err;
    defer if (shape != null) mem::free(shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = rank == 0 ? scalar_shape[0:0] : shape[0:rank];
    Value* tensor = make_tensor_uninitialized_checked(interp, dtype, shape_slice, "Tensor: out of memory");
    if (tensor == null || tensor.tag == ERROR) return tensor;
    bool keep_tensor = false;
    defer if (!keep_tensor) {
        boundary_cleanup_materialized_value(null, tensor, interp.current_scope);
    }
    if (tensor.tag != TENSOR || tensor.tensor_val == null) {
        return tensor_runtime_raise(interp, "Tensor: constructor produced invalid backing storage");
    }

    err = tensor_fill_from_data_arg(tensor.tensor_val, data_arg, interp);
    if (err != null) return err;
    keep_tensor = true;
    return tensor;
}

fn Value* tensor_construct_inferred(Value* data_arg, TensorDType dtype, Interp* interp) {
    usz* shape = null;
    usz rank = 0;
    usz count = 0;
    Value* err = null;
    if (!tensor_infer_shape_from_value(data_arg, interp, &shape, &rank, &count, &err)) return err;
    defer if (shape != null) mem::free(shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = rank == 0 ? scalar_shape[0:0] : shape[0:rank];
    Value* tensor = make_tensor_uninitialized_checked(interp, dtype, shape_slice, "Tensor: out of memory");
    if (tensor == null || tensor.tag == ERROR) return tensor;
    bool keep_tensor = false;
    defer if (!keep_tensor) {
        boundary_cleanup_materialized_value(null, tensor, interp.current_scope);
    }
    if (tensor.tag != TENSOR || tensor.tensor_val == null) {
        return tensor_runtime_raise(interp, "Tensor: constructor produced invalid backing storage");
    }
    if (tensor.tensor_val.element_count != count) {
        return tensor_runtime_raise(interp, "Tensor: inferred shape count mismatch");
    }

    usz offset = 0;
    if (count > 0 || rank == 0) {
        err = tensor_flatten_inferred_data(tensor.tensor_val, data_arg, &offset, interp);
        if (err != null) return err;
    }
    if (offset != count) {
        return tensor_runtime_raise(interp, "Tensor: inferred data length mismatch");
    }

    keep_tensor = true;
    return tensor;
}

fn Value* prim_tensor(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 1) return tensor_construct_inferred(args[0], TENSOR_DTYPE_DOUBLE, interp);
    if (args.len == 2) {
        TensorDType dtype = TENSOR_DTYPE_DOUBLE;
        if (tensor_dtype_arg_to_dtype(args[0], interp, &dtype)) return tensor_construct_inferred(args[1], dtype, interp);
        if (tensor_dtype_arg_to_dtype(args[1], interp, &dtype)) return tensor_construct_inferred(args[0], dtype, interp);
        return tensor_type_raise(interp, "Tensor: expected data with optional Double or BigFloat dtype");
    }
    if (args.len == 3) return tensor_construct_from_shape_and_data(args[0], args[1], args[2], interp);
    return primitive_arity_raise(interp, "Tensor", "1, 2, or 3 arguments: data [Double] or Double shape data-or-scalar", args.len);
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

    if (is_numeric_value(args[0]) && args[0].tag != BIG_COMPLEX) {
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
