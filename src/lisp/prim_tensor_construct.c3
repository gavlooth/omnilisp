module lisp;

import std::core::mem;
import main;

const double TENSOR_FLOAT32_FINITE_LIMIT = 3.4028234663852886e38;

fn bool is_tensor(Value* v) @inline {
    return v != null && v.tag == TENSOR;
}

fn bool tensor_dtype_arg_is_double(Value* dtype_arg, Interp* interp) @inline {
    if (dtype_arg == null) return false;
    if (dtype_arg.tag == SYMBOL) return dtype_arg.sym_val == interp.sym_Double;
    if (dtype_arg.tag == PRIMITIVE) return primitive_name_equals(dtype_arg, "Float64");
    if (dtype_arg.tag == METHOD_TABLE && dtype_arg.method_table_val != null) {
        return dtype_arg.method_table_val.name == interp.sym_Double;
    }
    return false;
}

fn bool tensor_dtype_arg_is_float32(Value* dtype_arg, Interp* interp) @inline {
    if (dtype_arg == null) return false;
    if (dtype_arg.tag == SYMBOL) return dtype_arg.sym_val == interp.sym_Float32;
    if (dtype_arg.tag == PRIMITIVE) return primitive_name_equals(dtype_arg, "Float32");
    if (dtype_arg.tag == METHOD_TABLE && dtype_arg.method_table_val != null) {
        return dtype_arg.method_table_val.name == interp.sym_Float32;
    }
    return false;
}

fn bool tensor_dtype_arg_is_complex128(Value* dtype_arg, Interp* interp) @inline {
    if (dtype_arg == null) return false;
    if (dtype_arg.tag == SYMBOL) return dtype_arg.sym_val == interp.sym_Complex128;
    if (dtype_arg.tag == PRIMITIVE) return primitive_name_equals(dtype_arg, "Complex128");
    if (dtype_arg.tag == METHOD_TABLE && dtype_arg.method_table_val != null) {
        return dtype_arg.method_table_val.name == interp.sym_Complex128;
    }
    return false;
}

fn bool tensor_dtype_arg_is_complex64(Value* dtype_arg, Interp* interp) @inline {
    if (dtype_arg == null) return false;
    if (dtype_arg.tag == SYMBOL) return dtype_arg.sym_val == interp.sym_Complex64;
    if (dtype_arg.tag == PRIMITIVE) return primitive_name_equals(dtype_arg, "Complex64");
    if (dtype_arg.tag == METHOD_TABLE && dtype_arg.method_table_val != null) {
        return dtype_arg.method_table_val.name == interp.sym_Complex64;
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

fn bool tensor_dtype_arg_is_big_complex(Value* dtype_arg, Interp* interp) @inline {
    if (dtype_arg == null) return false;
    if (dtype_arg.tag == SYMBOL) return dtype_arg.sym_val == interp.sym_BigComplex;
    if (dtype_arg.tag == PRIMITIVE) return primitive_name_equals(dtype_arg, "BigComplex");
    if (dtype_arg.tag == METHOD_TABLE && dtype_arg.method_table_val != null) {
        return dtype_arg.method_table_val.name == interp.sym_BigComplex;
    }
    return false;
}

fn bool tensor_dtype_arg_to_dtype(Value* dtype_arg, Interp* interp, TensorDType* out) @inline {
    if (out == null) return false;
    if (tensor_dtype_arg_is_double(dtype_arg, interp)) {
        *out = TENSOR_DTYPE_DOUBLE;
        return true;
    }
    if (tensor_dtype_arg_is_float32(dtype_arg, interp)) {
        *out = TENSOR_DTYPE_FLOAT32;
        return true;
    }
    if (tensor_dtype_arg_is_complex128(dtype_arg, interp)) {
        *out = TENSOR_DTYPE_COMPLEX128;
        return true;
    }
    if (tensor_dtype_arg_is_complex64(dtype_arg, interp)) {
        *out = TENSOR_DTYPE_COMPLEX64;
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
    if (tensor_dtype_arg_is_big_complex(dtype_arg, interp)) {
        *out = TENSOR_DTYPE_BIG_COMPLEX;
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

fn bool tensor_value_to_float32(Value* value, float* out) @inline {
    if (out == null) return false;
    double scalar = 0.0;
    if (!try_numeric_to_double(value, &scalar)) return false;
    if (!conversion_double_is_finite(scalar)) return false;
    if (scalar > TENSOR_FLOAT32_FINITE_LIMIT || scalar < -TENSOR_FLOAT32_FINITE_LIMIT) return false;
    *out = (float)scalar;
    return true;
}

fn bool tensor_double_to_float32(double scalar, float* out) @inline {
    if (out == null) return false;
    if (!conversion_double_is_finite(scalar)) return false;
    if (scalar > TENSOR_FLOAT32_FINITE_LIMIT || scalar < -TENSOR_FLOAT32_FINITE_LIMIT) return false;
    *out = (float)scalar;
    return true;
}

fn bool tensor_dtype_is_vulkan_real(TensorDType dtype) @inline {
    return dtype == TENSOR_DTYPE_DOUBLE || dtype == TENSOR_DTYPE_FLOAT32;
}

fn bool tensor_dtype_is_cuda_real(TensorDType dtype) @inline {
    return dtype == TENSOR_DTYPE_DOUBLE || dtype == TENSOR_DTYPE_FLOAT32;
}

fn bool tensor_dtype_is_fixed_complex(TensorDType dtype) @inline {
    return dtype == TENSOR_DTYPE_COMPLEX128 || dtype == TENSOR_DTYPE_COMPLEX64;
}

fn bool tensor_dtype_is_cuda_storage(TensorDType dtype) @inline {
    return tensor_dtype_is_cuda_real(dtype) || tensor_dtype_is_fixed_complex(dtype);
}

fn bool tensor_dtype_is_vulkan_storage(TensorDType dtype) @inline {
    return tensor_dtype_is_vulkan_real(dtype) || tensor_dtype_is_fixed_complex(dtype);
}

fn bool tensor_value_is_numeric_leaf(Value* value) @inline {
    return value != null &&
        (value.tag == INT || value.tag == DOUBLE || value.tag == FLOAT32 ||
         value.tag == COMPLEX128 || value.tag == COMPLEX64 ||
         value.tag == BIG_INTEGER || value.tag == BIG_FLOAT || value.tag == BIG_COMPLEX);
}

fn bool tensor_promote_inferred_fixed_complex_dtype(TensorDType* dtype, TensorDType next) {
    if (dtype == null) return false;
    if (next == TENSOR_DTYPE_COMPLEX128) {
        *dtype = TENSOR_DTYPE_COMPLEX128;
        return true;
    }
    if (next == TENSOR_DTYPE_COMPLEX64 && *dtype != TENSOR_DTYPE_COMPLEX128) {
        *dtype = TENSOR_DTYPE_COMPLEX64;
    }
    return true;
}

fn bool tensor_infer_fixed_complex_dtype_from_value(Value* value, TensorDType* dtype) {
    if (value == null || dtype == null) return false;
    if (value.tag == COMPLEX128) return tensor_promote_inferred_fixed_complex_dtype(dtype, TENSOR_DTYPE_COMPLEX128);
    if (value.tag == COMPLEX64) return tensor_promote_inferred_fixed_complex_dtype(dtype, TENSOR_DTYPE_COMPLEX64);
    if (tensor_value_is_numeric_leaf(value)) return true;
    if (!tensor_value_is_container(value)) return false;

    usz count = 0;
    if (!tensor_container_count(value, &count)) return false;
    for (usz i = 0; i < count; i++) {
        Value* child = tensor_container_nth(value, i);
        if (!tensor_infer_fixed_complex_dtype_from_value(child, dtype)) return false;
    }
    return true;
}

fn bool tensor_default_inferred_dtype(Value* data_arg, TensorDType* out_dtype) {
    if (out_dtype == null) return false;
    TensorDType dtype = TENSOR_DTYPE_DOUBLE;
    if (!tensor_infer_fixed_complex_dtype_from_value(data_arg, &dtype)) return false;
    *out_dtype = dtype;
    return true;
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

    if (tensor_value_is_numeric_leaf(value)) {
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
    if (tensor.storage_offset > tensor.storage_element_count ||
        index >= tensor.storage_element_count - tensor.storage_offset) {
        return tensor_runtime_raise(interp, label);
    }
    usz storage_index = tensor.storage_offset + index;
    if (tensor.byte_len > 0 && tensor.data == null) return tensor_runtime_raise(interp, label);
    switch (tensor.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double scalar = 0.0;
            if (!tensor_value_to_double(value, &scalar)) return tensor_type_raise(interp, "Tensor: data elements must be real numbers representable as Float64");
            ((double*)tensor.data)[storage_index] = scalar;
            return null;
        }
        case TENSOR_DTYPE_FLOAT32: {
            float scalar = 0.0f;
            if (!tensor_value_to_float32(value, &scalar)) return tensor_type_raise(interp, "Tensor: data elements must be real numbers representable as Float32");
            ((float*)tensor.data)[storage_index] = scalar;
            return null;
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val scalar;
            if (!fixed_complex_value_to_complex128(value, &scalar)) return tensor_type_raise(interp, "Tensor: data elements must be numbers representable as Complex128");
            ((Complex128Val*)tensor.data)[storage_index] = scalar;
            return null;
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val scalar;
            if (!fixed_complex_value_to_complex64(value, &scalar)) return tensor_type_raise(interp, "Tensor: data elements must be numbers representable as Complex64");
            ((Complex64Val*)tensor.data)[storage_index] = scalar;
            return null;
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void* handle = big_integer_handle_from_value(value);
            if (handle == null) return tensor_type_raise(interp, "Tensor: data elements must be exact integers representable as BigInteger");
            void** values = (void**)tensor.data;
            if (values[storage_index] != null) omni_big_integer_free(values[storage_index]);
            values[storage_index] = handle;
            return null;
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void* handle = big_float_handle_from_value(value);
            if (handle == null) return tensor_type_raise(interp, "Tensor: data elements must be real numbers representable as BigFloat");
            void** values = (void**)tensor.data;
            if (values[storage_index] != null) omni_big_float_free(values[storage_index]);
            values[storage_index] = handle;
            return null;
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void* handle = big_complex_handle_from_value(value);
            if (handle == null) return tensor_type_raise(interp, "Tensor: data elements must be numbers representable as BigComplex");
            void** values = (void**)tensor.data;
            if (values[storage_index] != null) omni_big_complex_free(values[storage_index]);
            values[storage_index] = handle;
            return null;
        }
    }
}

fn Value* tensor_flatten_inferred_data(TensorVal* tensor, Value* value, usz* offset, Interp* interp) {
    if (tensor == null || offset == null) return tensor_runtime_raise(interp, "Tensor: inferred tensor storage is missing");

    if (tensor_value_is_numeric_leaf(value)) {
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
        case TENSOR_DTYPE_FLOAT32: {
            float scalar = 0.0f;
            if (!tensor_value_to_float32(data_arg, &scalar)) return null;
            float* dst = (float*)tensor.data;
            for (usz i = 0; i < tensor.element_count; i++) dst[i] = scalar;
            return data_arg;
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val scalar;
            if (!fixed_complex_value_to_complex128(data_arg, &scalar)) return null;
            Complex128Val* dst = (Complex128Val*)tensor.data;
            for (usz i = 0; i < tensor.element_count; i++) dst[i] = scalar;
            return data_arg;
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val scalar;
            if (!fixed_complex_value_to_complex64(data_arg, &scalar)) return null;
            Complex64Val* dst = (Complex64Val*)tensor.data;
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
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void* source = big_complex_handle_from_value(data_arg);
            if (source == null) return null;
            defer omni_big_complex_free(source);
            void** staged = (void**)mem::malloc(tensor.byte_len);
            if (tensor.byte_len > 0 && staged == null) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: failed to allocate BigComplex fill storage", null);
            }
            defer if (staged != null) mem::free(staged);
            for (usz i = 0; i < tensor.element_count; i++) staged[i] = null;
            bool ok = true;
            for (usz i = 0; i < tensor.element_count; i++) {
                staged[i] = omni_big_complex_clone(source);
                if (staged[i] == null) {
                    ok = false;
                    break;
                }
            }
            if (!ok) {
                for (usz i = 0; i < tensor.element_count; i++) {
                    if (staged[i] != null) omni_big_complex_free(staged[i]);
                }
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "Tensor: failed to allocate BigComplex fill value", null);
            }
            tensor_big_complex_clear_data(tensor);
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
