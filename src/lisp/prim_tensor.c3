module lisp;

import std::core::mem;
import main;

extern fn double tensor_c_fabs(double x) @extern("fabs");
extern fn double tensor_c_sin(double x) @extern("sin");
extern fn double tensor_c_cos(double x) @extern("cos");
extern fn double tensor_c_tan(double x) @extern("tan");
extern fn double tensor_c_asin(double x) @extern("asin");
extern fn double tensor_c_acos(double x) @extern("acos");
extern fn double tensor_c_atan(double x) @extern("atan");
extern fn double tensor_c_atan2(double y, double x) @extern("atan2");
extern fn double tensor_c_sinh(double x) @extern("sinh");
extern fn double tensor_c_cosh(double x) @extern("cosh");
extern fn double tensor_c_tanh(double x) @extern("tanh");
extern fn double tensor_c_exp(double x) @extern("exp");
extern fn double tensor_c_log(double x) @extern("log");
extern fn double tensor_c_log10(double x) @extern("log10");
extern fn double tensor_c_erf(double x) @extern("erf");
extern fn double tensor_c_erfc(double x) @extern("erfc");
extern fn double tensor_c_sqrt(double x) @extern("sqrt");
extern fn double tensor_c_pow(double x, double y) @extern("pow");
extern fn double tensor_c_floor(double x) @extern("floor");
extern fn double tensor_c_ceil(double x) @extern("ceil");
extern fn double tensor_c_round(double x) @extern("round");
extern fn double tensor_c_fmin(double x, double y) @extern("fmin");
extern fn double tensor_c_fmax(double x, double y) @extern("fmax");

const double TENSOR_FLOAT32_FINITE_LIMIT = 3.4028234663852886e38;
const double TENSOR_INV_SQRT_2 = 0.70710678118654752440084436210484903928;

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

fn TensorDType tensor_default_inferred_dtype(Value* data_arg) {
    TensorDType dtype = TENSOR_DTYPE_DOUBLE;
    (void)tensor_infer_fixed_complex_dtype_from_value(data_arg, &dtype);
    return dtype;
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

fn bool tensor_has_valid_storage(TensorVal* tensor) @inline {
    if (tensor == null) return false;
    if (tensor.kind != TENSOR_PAYLOAD_CONCRETE) return false;
    if (tensor.device != TENSOR_DEVICE_CPU) return false;
    if (tensor.rank > 0 && (tensor.shape == null || tensor.strides == null)) return false;
    if (tensor.storage_element_count < tensor.element_count) return false;
    if (tensor.storage_byte_len < tensor.byte_len) return false;
    if (tensor.storage_offset > tensor.storage_element_count) return false;
    if (tensor.element_count > tensor.storage_element_count - tensor.storage_offset) return false;
    usz storage_bytes = 0;
    if (!tensor_checked_mul_usz(tensor.storage_element_count, tensor_dtype_size(tensor.dtype), &storage_bytes)) return false;
    if (storage_bytes > tensor.storage_byte_len) return false;
    if (tensor.byte_len > 0 && tensor.data == null) return false;
    return true;
}

fn bool tensor_has_valid_device_storage(TensorVal* tensor, TensorDeviceKind device) @inline {
    if (tensor == null) return false;
    if (tensor.kind != TENSOR_PAYLOAD_CONCRETE) return false;
    if (tensor.device != device) return false;
    if (tensor.rank > 0 && (tensor.shape == null || tensor.strides == null)) return false;
    if (tensor.storage_element_count < tensor.element_count) return false;
    if (tensor.storage_byte_len < tensor.byte_len) return false;
    if (tensor.storage_offset != 0) return false;
    if (tensor.storage_offset > tensor.storage_element_count) return false;
    if (tensor.element_count > tensor.storage_element_count - tensor.storage_offset) return false;
    usz storage_bytes = 0;
    if (!tensor_checked_mul_usz(tensor.storage_element_count, tensor_dtype_size(tensor.dtype), &storage_bytes)) return false;
    if (storage_bytes > tensor.storage_byte_len) return false;
    if (!tensor_dense_row_major(tensor)) return false;
    if (tensor.byte_len > 0 && tensor.device_handle == null) return false;
    return true;
}

fn Value* tensor_require_cpu_destination(TensorVal* dest, Interp* interp) {
    if (dest == null) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    if (dest.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: destination Tensor must be CPU-placed", null);
    }
    if (!tensor_has_valid_storage(dest)) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    return null;
}

fn Value* tensor_require_cuda_destination(TensorVal* dest, Interp* interp) {
    if (dest == null) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    if (dest.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: destination Tensor must be CPU-placed, CUDA-placed, or Vulkan-placed", null);
    }
    if (!tensor_dtype_is_cuda_storage(dest.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: CUDA destination currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if (!tensor_has_valid_device_storage(dest, TENSOR_DEVICE_CUDA)) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    return null;
}

fn Value* tensor_require_vulkan_destination(TensorVal* dest, Interp* interp) {
    if (dest == null) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    if (dest.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: destination Tensor must be CPU-placed, CUDA-placed, or Vulkan-placed", null);
    }
    if (!tensor_dtype_is_vulkan_storage(dest.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan destination currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if (!tensor_has_valid_device_storage(dest, TENSOR_DEVICE_VULKAN)) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    return null;
}

fn bool tensor_has_valid_metadata(TensorVal* tensor) @inline {
    if (tensor == null) return false;
    if (tensor.rank > 0 && (tensor.shape == null || tensor.strides == null)) return false;
    if (tensor.kind == TENSOR_PAYLOAD_MAP || tensor.kind == TENSOR_PAYLOAD_CONTRACT) {
        return tensor.storage_offset == 0 &&
            tensor.storage_element_count == 0 &&
            tensor.storage_byte_len == 0;
    }
    if (tensor.kind == TENSOR_PAYLOAD_VIEW && (!tensor.is_view || tensor.view_source == null)) return false;
    if (tensor.storage_element_count < tensor.element_count) return false;
    if (tensor.storage_byte_len < tensor.byte_len) return false;
    if (tensor.storage_offset > tensor.storage_element_count) return false;
    if (tensor.element_count > tensor.storage_element_count - tensor.storage_offset) return false;
    usz storage_bytes = 0;
    if (!tensor_checked_mul_usz(tensor.storage_element_count, tensor_dtype_size(tensor.dtype), &storage_bytes)) return false;
    if (storage_bytes > tensor.storage_byte_len) return false;
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
        case TENSOR_PAYLOAD_VIEW:
            return tensor_expr_storage_aliases(source.view_source, dest);
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
        tensor.storage_offset == 0 &&
        tensor.shape != null &&
        tensor.strides != null &&
        tensor.strides[1] == 1 &&
        tensor.strides[0] == tensor.shape[1];
}

fn bool tensor_rank1_contiguous(TensorVal* tensor) @inline {
    return tensor != null &&
        tensor.rank == 1 &&
        tensor.storage_offset == 0 &&
        tensor.shape != null &&
        tensor.strides != null &&
        tensor.strides[0] == 1;
}

fn bool tensor_dense_row_major(TensorVal* tensor) {
    if (tensor == null || !tensor_has_valid_metadata(tensor)) return false;
    if (tensor.storage_offset != 0) return false;
    if (tensor.rank == 0) return true;
    usz expected_stride = 1;
    for (usz i = tensor.rank; i > 0; i--) {
        usz axis = i - 1;
        if (tensor.strides[axis] != expected_stride) return false;
        if (tensor.shape[axis] != 0 && expected_stride > usz.max / tensor.shape[axis]) return false;
        expected_stride *= tensor.shape[axis];
    }
    return true;
}

fn bool tensor_concrete_zero_offset_dense_row_major(TensorVal* tensor) {
    return tensor != null &&
        tensor.kind == TENSOR_PAYLOAD_CONCRETE &&
        !tensor.is_view &&
        tensor.storage_offset == 0 &&
        tensor_dense_row_major(tensor);
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
    if (dest != null && dest.device == TENSOR_DEVICE_CUDA) {
        return tensor_copy_into_cuda_destination(source_value, dest_value, interp);
    }
    if (dest != null && dest.device == TENSOR_DEVICE_VULKAN) {
        return tensor_copy_into_vulkan_destination(source_value, dest_value, interp);
    }
    if (dest == null) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    Value* dest_check = tensor_require_cpu_destination(dest, interp);
    if (dest_check != null) return dest_check;
    if (source.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (source.element_count != dest.element_count || source.byte_len != dest.byte_len || !tensor_same_shape(source, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }
    if (tensor_expr_has_non_cpu_device(source_value)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: non-CPU Tensor source requires explicit to-device cpu copy before destination realization", null);
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

fn Value* tensor_fill_cuda_destination(TensorVal* dest, Value* data_arg, Interp* interp) {
    Value* dest_check = tensor_require_cuda_destination(dest, interp);
    if (dest_check != null) return dest_check;
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    if (!tensor_dense_row_major(dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: CUDA destination must be dense row-major", null);
    }

    int status = OMNI_TENSOR_CUDA_INVALID;
    if (dest.dtype == TENSOR_DTYPE_DOUBLE) {
        double scalar = 0.0;
        if (!tensor_value_to_double(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Float64");
        }
        status = omni_tensor_backend_cuda_fill_f64(dest.device_handle, dest.element_count, scalar);
    } else if (dest.dtype == TENSOR_DTYPE_FLOAT32) {
        float scalar = 0.0f;
        if (!tensor_value_to_float32(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Float32");
        }
        status = omni_tensor_backend_cuda_fill_f32(dest.device_handle, dest.element_count, scalar);
    } else if (dest.dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val scalar;
        if (!fixed_complex_value_to_complex128(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Complex128");
        }
        if (dest.byte_len == 0) return null;
        Complex128Val* staged = (Complex128Val*)mem::malloc(dest.byte_len);
        if (staged == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "realize: failed to allocate Complex128 CUDA fill staging", null);
        }
        defer mem::free(staged);
        for (usz i = 0; i < dest.element_count; i++) staged[i] = scalar;
        status = omni_tensor_backend_cuda_copy_to_existing_device(staged, dest.byte_len, dest.device_handle);
    } else if (dest.dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val scalar;
        if (!fixed_complex_value_to_complex64(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Complex64");
        }
        if (dest.byte_len == 0) return null;
        Complex64Val* staged = (Complex64Val*)mem::malloc(dest.byte_len);
        if (staged == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "realize: failed to allocate Complex64 CUDA fill staging", null);
        }
        defer mem::free(staged);
        for (usz i = 0; i < dest.element_count; i++) staged[i] = scalar;
        status = omni_tensor_backend_cuda_copy_to_existing_device(staged, dest.byte_len, dest.device_handle);
    }

    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        return tensor_cuda_status_raise(interp, status, "realize: CUDA destination fill failed");
    }
    return null;
}

fn Value* tensor_copy_into_cuda_destination(Value* source_value, Value* dest_value, Interp* interp) {
    if (!is_tensor(source_value)) return tensor_type_raise(interp, "realize: expected Tensor source");
    if (!is_tensor(dest_value)) return tensor_type_raise(interp, "realize: destination must be a Tensor");

    TensorVal* source = source_value.tensor_val;
    TensorVal* dest = dest_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "realize: source tensor metadata is missing");
    Value* dest_check = tensor_require_cuda_destination(dest, interp);
    if (dest_check != null) return dest_check;
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    if (source.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (source.element_count != dest.element_count || source.byte_len != dest.byte_len || !tensor_same_shape(source, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }
    if (source.kind == TENSOR_PAYLOAD_VIEW || source.is_view ||
        !tensor_dense_row_major(source) || !tensor_dense_row_major(dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: CUDA destination requires dense row-major tensors", null);
    }

    bool touches_non_cpu = tensor_expr_has_non_cpu_device(source_value);
    Value* realized = source.kind == TENSOR_PAYLOAD_CONCRETE ? source_value : tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    defer if (realized != source_value) {
        boundary_cleanup_materialized_value(null, realized, interp.current_scope);
    }
    if (!is_tensor(realized) || realized.tensor_val == null) {
        return tensor_runtime_raise(interp, "realize: realized Tensor backing storage is missing");
    }

    TensorVal* concrete = realized.tensor_val;
    if (concrete.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (concrete.element_count != dest.element_count || concrete.byte_len != dest.byte_len || !tensor_same_shape(concrete, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }
    if (!tensor_concrete_zero_offset_dense_row_major(concrete)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: CUDA destination requires dense row-major tensors", null);
    }

    if (concrete.device == TENSOR_DEVICE_CPU) {
        if (touches_non_cpu) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: CUDA source did not realize to CUDA storage", null);
        }
        if (!tensor_has_valid_storage(concrete)) {
            return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        }
        int status = omni_tensor_backend_cuda_copy_to_existing_device(concrete.data, concrete.byte_len, dest.device_handle);
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            return tensor_cuda_status_raise(interp, status, "realize: CUDA destination copy failed");
        }
        return dest_value;
    }

    if (concrete.device == TENSOR_DEVICE_CUDA) {
        if (!tensor_has_valid_device_storage(concrete, TENSOR_DEVICE_CUDA)) {
            return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        }
        int status = omni_tensor_backend_cuda_copy_device_to_existing_device(concrete.device_handle, concrete.byte_len, dest.device_handle);
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            return tensor_cuda_status_raise(interp, status, "realize: CUDA destination copy failed");
        }
        return dest_value;
    }

    return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: unsupported source device for CUDA destination", null);
}

fn Value* tensor_fill_vulkan_destination(TensorVal* dest, Value* data_arg, Interp* interp) {
    Value* dest_check = tensor_require_vulkan_destination(dest, interp);
    if (dest_check != null) return dest_check;
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    if (!tensor_dense_row_major(dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan destination must be dense row-major", null);
    }

    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (dest.dtype == TENSOR_DTYPE_DOUBLE) {
        double scalar = 0.0;
        if (!tensor_value_to_double(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Float64");
        }
        status = omni_tensor_backend_vulkan_fill_f64(dest.device_handle, dest.element_count, scalar);
    } else if (dest.dtype == TENSOR_DTYPE_FLOAT32) {
        float scalar = 0.0f;
        if (!tensor_value_to_float32(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Float32");
        }
        status = omni_tensor_backend_vulkan_fill_f32(dest.device_handle, dest.element_count, scalar);
    } else if (dest.dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val scalar;
        if (!fixed_complex_value_to_complex128(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Complex128");
        }
        if (dest.byte_len == 0) return null;
        Complex128Val* staged = (Complex128Val*)mem::malloc(dest.byte_len);
        if (staged == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "realize: failed to allocate Complex128 Vulkan fill staging", null);
        }
        defer mem::free(staged);
        for (usz i = 0; i < dest.element_count; i++) staged[i] = scalar;
        status = omni_tensor_backend_vulkan_copy_to_existing_device(staged, dest.byte_len, dest.device_handle);
    } else if (dest.dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val scalar;
        if (!fixed_complex_value_to_complex64(data_arg, &scalar)) {
            return tensor_type_raise(interp, "realize: scalar destination fill must be representable as Complex64");
        }
        if (dest.byte_len == 0) return null;
        Complex64Val* staged = (Complex64Val*)mem::malloc(dest.byte_len);
        if (staged == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "realize: failed to allocate Complex64 Vulkan fill staging", null);
        }
        defer mem::free(staged);
        for (usz i = 0; i < dest.element_count; i++) staged[i] = scalar;
        status = omni_tensor_backend_vulkan_copy_to_existing_device(staged, dest.byte_len, dest.device_handle);
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "realize: Vulkan destination fill failed");
    }
    return null;
}

fn Value* tensor_copy_into_vulkan_destination(Value* source_value, Value* dest_value, Interp* interp) {
    if (!is_tensor(source_value)) return tensor_type_raise(interp, "realize: expected Tensor source");
    if (!is_tensor(dest_value)) return tensor_type_raise(interp, "realize: destination must be a Tensor");

    TensorVal* source = source_value.tensor_val;
    TensorVal* dest = dest_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "realize: source tensor metadata is missing");
    Value* dest_check = tensor_require_vulkan_destination(dest, interp);
    if (dest_check != null) return dest_check;
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    if (source.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (source.element_count != dest.element_count || source.byte_len != dest.byte_len || !tensor_same_shape(source, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }
    bool source_is_view = source.kind == TENSOR_PAYLOAD_VIEW || source.is_view;
    if (source_is_view && source.device != TENSOR_DEVICE_CPU && source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan destination currently supports CPU-backed or Vulkan transpose-view materialization", null);
    }
    if ((!source_is_view && !tensor_dense_row_major(source)) || !tensor_dense_row_major(dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan destination requires dense row-major tensors", null);
    }

    bool touches_non_cpu = tensor_expr_has_non_cpu_device(source_value);
    Value* realized = source.kind == TENSOR_PAYLOAD_CONCRETE ? source_value : tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    defer if (realized != source_value) {
        boundary_cleanup_materialized_value(null, realized, interp.current_scope);
    }
    if (!is_tensor(realized) || realized.tensor_val == null) {
        return tensor_runtime_raise(interp, "realize: realized Tensor backing storage is missing");
    }

    TensorVal* concrete = realized.tensor_val;
    if (concrete.dtype != dest.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "realize: tensor dtype mismatch", null);
    }
    if (concrete.element_count != dest.element_count || concrete.byte_len != dest.byte_len || !tensor_same_shape(concrete, dest)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "realize: tensor shape mismatch", null);
    }
    if (!tensor_concrete_zero_offset_dense_row_major(concrete)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan destination requires dense row-major tensors", null);
    }

    if (concrete.device == TENSOR_DEVICE_CPU) {
        if (touches_non_cpu) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan source did not realize to Vulkan storage", null);
        }
        if (!tensor_has_valid_storage(concrete)) {
            return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        }
        int status = omni_tensor_backend_vulkan_copy_to_existing_device(concrete.data, concrete.byte_len, dest.device_handle);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "realize: Vulkan destination copy failed");
        }
        return dest_value;
    }

    if (concrete.device == TENSOR_DEVICE_VULKAN) {
        if (!tensor_has_valid_device_storage(concrete, TENSOR_DEVICE_VULKAN)) {
            return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        }
        int status = omni_tensor_backend_vulkan_copy_device_to_existing_device(concrete.device_handle, concrete.byte_len, dest.device_handle);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "realize: Vulkan destination copy failed");
        }
        return dest_value;
    }

    return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: unsupported source device for Vulkan destination", null);
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

fn bool tensor_primitive_payload_name_equals(Primitive* prim, char[] expected) @inline {
    if (prim == null || expected.len >= 64) return false;
    for (usz i = 0; i < expected.len; i++) {
        char actual = prim.name[i];
        if (actual == 0 || actual != expected[i]) return false;
    }
    return prim.name[expected.len] == 0;
}

fn bool tensor_map_callable_is_add(Value* func) @inline {
    if (primitive_name_equals(func, "+")) return true;
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        return tensor_primitive_payload_name_equals(fallback, "+");
    }
    return false;
}

fn bool tensor_map_callable_vulkan_op(Value* func, uint* out_op) @inline {
    if (out_op == null) return false;
    if (primitive_name_equals(func, "+")) {
        *out_op = 0;
        return true;
    }
    if (primitive_name_equals(func, "-")) {
        *out_op = 1;
        return true;
    }
    if (primitive_name_equals(func, "*")) {
        *out_op = 2;
        return true;
    }
    if (primitive_name_equals(func, "/")) {
        *out_op = 3;
        return true;
    }
    if (primitive_name_equals(func, "min")) {
        *out_op = 4;
        return true;
    }
    if (primitive_name_equals(func, "max")) {
        *out_op = 5;
        return true;
    }
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        if (tensor_primitive_payload_name_equals(fallback, "+")) {
            *out_op = 0;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "-")) {
            *out_op = 1;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "*")) {
            *out_op = 2;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "/")) {
            *out_op = 3;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "min")) {
            *out_op = 4;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "max")) {
            *out_op = 5;
            return true;
        }
    }
    return false;
}

fn bool tensor_map_callable_cuda_op(Value* func, uint* out_op) @inline {
    return tensor_map_callable_vulkan_op(func, out_op);
}

fn bool tensor_map_callable_complex_binary_op(Value* func, uint* out_op) @inline {
    if (out_op == null) return false;
    if (primitive_name_equals(func, "+")) {
        *out_op = 0;
        return true;
    }
    if (primitive_name_equals(func, "-")) {
        *out_op = 1;
        return true;
    }
    if (primitive_name_equals(func, "*")) {
        *out_op = 2;
        return true;
    }
    if (primitive_name_equals(func, "/")) {
        *out_op = 3;
        return true;
    }
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        if (tensor_primitive_payload_name_equals(fallback, "+")) {
            *out_op = 0;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "-")) {
            *out_op = 1;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "*")) {
            *out_op = 2;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "/")) {
            *out_op = 3;
            return true;
        }
    }
    return false;
}

fn bool tensor_map_callable_complex_unary_op(Value* func, uint* out_op) @inline {
    if (out_op == null) return false;
    if (primitive_name_equals(func, "abs")) {
        *out_op = 0;
        return true;
    }
    if (primitive_name_equals(func, "-")) {
        *out_op = 1;
        return true;
    }
    if (primitive_name_equals(func, "+")) {
        *out_op = 2;
        return true;
    }
    if (primitive_name_equals(func, "real-part")) {
        *out_op = 3;
        return true;
    }
    if (primitive_name_equals(func, "imag-part")) {
        *out_op = 4;
        return true;
    }
    if (primitive_name_equals(func, "conjugate")) {
        *out_op = 5;
        return true;
    }
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        if (tensor_primitive_payload_name_equals(fallback, "abs")) {
            *out_op = 0;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "-")) {
            *out_op = 1;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "+")) {
            *out_op = 2;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "real-part")) {
            *out_op = 3;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "imag-part")) {
            *out_op = 4;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "conjugate")) {
            *out_op = 5;
            return true;
        }
    }
    return false;
}

fn bool tensor_map_callable_cuda_unary_op(Value* func, uint* out_op) @inline {
    if (tensor_map_callable_vulkan_unary_op(func, out_op)) return tensor_cuda_unary_op_available(*out_op);
    if (out_op == null) return false;
    if (primitive_name_equals(func, "math/erf")) {
        *out_op = 17;
        return tensor_cuda_unary_op_available(*out_op);
    }
    if (primitive_name_equals(func, "math/erfc")) {
        *out_op = 18;
        return tensor_cuda_unary_op_available(*out_op);
    }
    if (primitive_name_equals(func, "stats/normal-cdf")) {
        *out_op = 19;
        return tensor_cuda_unary_op_available(*out_op);
    }
    if (primitive_name_equals(func, "stats/normal-quantile")) {
        *out_op = 20;
        return tensor_cuda_unary_op_available(*out_op);
    }
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        if (tensor_primitive_payload_name_equals(fallback, "math/erf")) {
            *out_op = 17;
            return tensor_cuda_unary_op_available(*out_op);
        }
        if (tensor_primitive_payload_name_equals(fallback, "math/erfc")) {
            *out_op = 18;
            return tensor_cuda_unary_op_available(*out_op);
        }
        if (tensor_primitive_payload_name_equals(fallback, "stats/normal-cdf")) {
            *out_op = 19;
            return tensor_cuda_unary_op_available(*out_op);
        }
        if (tensor_primitive_payload_name_equals(fallback, "stats/normal-quantile")) {
            *out_op = 20;
            return tensor_cuda_unary_op_available(*out_op);
        }
    }
    return false;
}

fn bool tensor_cuda_unary_op_available(uint op) @inline {
    if (op <= 4) return true;
    if (op <= 20) return omni_tensor_backend_cuda_scientific_map_available() != 0;
    return false;
}

fn bool tensor_cuda_complex_map_available_for_dtype(TensorDType dtype) @inline {
    if (!tensor_dtype_is_fixed_complex(dtype)) return false;
    return omni_tensor_backend_cuda_complex_map_available() != 0;
}

fn Value* tensor_map_cuda_callable_op_or_raise(Value* func, bool is_binary, TensorDType dtype, uint* out_op, Interp* interp) {
    if (out_op == null) return tensor_runtime_raise(interp, "map: CUDA callable state is missing");
    uint op = 0;
    if (tensor_dtype_is_fixed_complex(dtype)) {
        if (!tensor_cuda_complex_map_available_for_dtype(dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA fixed-width complex elementwise kernels are unavailable", null);
        }
        bool supported = is_binary
            ? tensor_map_callable_complex_binary_op(func, &op)
            : tensor_map_callable_complex_unary_op(func, &op);
        if (!supported) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Complex128 and Complex64 arithmetic elementwise kernels", null);
        }
    } else if (!is_binary) {
        if (!tensor_map_callable_cuda_unary_op(func, &op)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64 and Float32 arithmetic and scientific unary kernels", null);
        }
    } else if (!tensor_map_callable_cuda_op(func, &op)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64 and Float32 arithmetic kernels", null);
    }
    *out_op = op;
    return null;
}

fn bool tensor_map_callable_vulkan_unary_op(Value* func, uint* out_op) @inline {
    if (out_op == null) return false;
    if (primitive_name_equals(func, "+")) {
        *out_op = 3;
        return true;
    }
    if (primitive_name_equals(func, "abs")) {
        *out_op = 0;
        return true;
    }
    if (primitive_name_equals(func, "-")) {
        *out_op = 1;
        return true;
    }
    if (primitive_name_equals(func, "sqrt")) {
        *out_op = 2;
        return true;
    }
    if (primitive_name_equals(func, "real-part")) {
        *out_op = 3;
        return true;
    }
    if (primitive_name_equals(func, "conjugate")) {
        *out_op = 3;
        return true;
    }
    if (primitive_name_equals(func, "imag-part")) {
        *out_op = 4;
        return true;
    }
    if (primitive_name_equals(func, "sin")) {
        *out_op = 5;
        return true;
    }
    if (primitive_name_equals(func, "cos")) {
        *out_op = 6;
        return true;
    }
    if (primitive_name_equals(func, "tan")) {
        *out_op = 7;
        return true;
    }
    if (primitive_name_equals(func, "asin")) {
        *out_op = 8;
        return true;
    }
    if (primitive_name_equals(func, "acos")) {
        *out_op = 9;
        return true;
    }
    if (primitive_name_equals(func, "atan")) {
        *out_op = 10;
        return true;
    }
    if (primitive_name_equals(func, "sinh")) {
        *out_op = 11;
        return true;
    }
    if (primitive_name_equals(func, "cosh")) {
        *out_op = 12;
        return true;
    }
    if (primitive_name_equals(func, "tanh")) {
        *out_op = 13;
        return true;
    }
    if (primitive_name_equals(func, "exp")) {
        *out_op = 14;
        return true;
    }
    if (primitive_name_equals(func, "log")) {
        *out_op = 15;
        return true;
    }
    if (primitive_name_equals(func, "log10")) {
        *out_op = 16;
        return true;
    }
    if (primitive_name_equals(func, "stats/normal-cdf")) {
        *out_op = 19;
        return true;
    }
    if (primitive_name_equals(func, "stats/normal-quantile")) {
        *out_op = 20;
        return true;
    }
    if (func != null && func.tag == METHOD_TABLE && func.method_table_val != null) {
        Primitive* fallback = method_table_fallback_prim(func.method_table_val);
        if (tensor_primitive_payload_name_equals(fallback, "+")) {
            *out_op = 3;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "abs")) {
            *out_op = 0;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "-")) {
            *out_op = 1;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "sqrt")) {
            *out_op = 2;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "real-part")) {
            *out_op = 3;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "conjugate")) {
            *out_op = 3;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "imag-part")) {
            *out_op = 4;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "sin")) {
            *out_op = 5;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "cos")) {
            *out_op = 6;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "tan")) {
            *out_op = 7;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "asin")) {
            *out_op = 8;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "acos")) {
            *out_op = 9;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "atan")) {
            *out_op = 10;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "sinh")) {
            *out_op = 11;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "cosh")) {
            *out_op = 12;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "tanh")) {
            *out_op = 13;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "exp")) {
            *out_op = 14;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "log")) {
            *out_op = 15;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "log10")) {
            *out_op = 16;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "stats/normal-cdf")) {
            *out_op = 19;
            return true;
        }
        if (tensor_primitive_payload_name_equals(fallback, "stats/normal-quantile")) {
            *out_op = 20;
            return true;
        }
    }
    return false;
}

fn bool tensor_unary_math_vulkan_op(int big_float_op, uint* out_op) @inline {
    if (out_op == null) return false;
    switch (big_float_op) {
        case BIG_FLOAT_MATH_SIN:
            *out_op = 5;
            return true;
        case BIG_FLOAT_MATH_COS:
            *out_op = 6;
            return true;
        case BIG_FLOAT_MATH_TAN:
            *out_op = 7;
            return true;
        case BIG_FLOAT_MATH_ASIN:
            *out_op = 8;
            return true;
        case BIG_FLOAT_MATH_ACOS:
            *out_op = 9;
            return true;
        case BIG_FLOAT_MATH_ATAN:
            *out_op = 10;
            return true;
        case BIG_FLOAT_MATH_SINH:
            *out_op = 11;
            return true;
        case BIG_FLOAT_MATH_COSH:
            *out_op = 12;
            return true;
        case BIG_FLOAT_MATH_TANH:
            *out_op = 13;
            return true;
        case BIG_FLOAT_MATH_EXP:
            *out_op = 14;
            return true;
        case BIG_FLOAT_MATH_LOG:
            *out_op = 15;
            return true;
        case BIG_FLOAT_MATH_LOG10:
            *out_op = 16;
            return true;
        case BIG_FLOAT_MATH_NORMAL_CDF:
            *out_op = 19;
            return true;
        case BIG_FLOAT_MATH_NORMAL_QUANTILE:
            *out_op = 20;
            return true;
        case BIG_FLOAT_MATH_SQRT:
            *out_op = 2;
            return true;
        default:
            return false;
    }
}

fn bool tensor_unary_math_cuda_op(int big_float_op, uint* out_op) @inline {
    if (!tensor_unary_math_vulkan_op(big_float_op, out_op)) return false;
    return tensor_cuda_unary_op_available(*out_op);
}

fn bool tensor_unary_math_cuda_extra_op(int big_float_op, uint* out_op) @inline {
    if (out_op == null) return false;
    switch (big_float_op) {
        case BIG_FLOAT_MATH_ERF:
            *out_op = 17;
            return tensor_cuda_unary_op_available(*out_op);
        case BIG_FLOAT_MATH_ERFC:
            *out_op = 18;
            return tensor_cuda_unary_op_available(*out_op);
        case BIG_FLOAT_MATH_NORMAL_CDF:
            *out_op = 19;
            return tensor_cuda_unary_op_available(*out_op);
        case BIG_FLOAT_MATH_NORMAL_QUANTILE:
            *out_op = 20;
            return tensor_cuda_unary_op_available(*out_op);
        default:
            return false;
    }
}

fn bool tensor_unary_math_cuda_dispatch_op(int big_float_op, uint* out_op) @inline {
    if (tensor_unary_math_cuda_op(big_float_op, out_op)) return true;
    if (tensor_unary_math_cuda_extra_op(big_float_op, out_op)) return true;
    return false;
}

fn bool tensor_unary_math_has_complex_op(int big_complex_op) @inline {
    return big_complex_op > 0;
}

fn Value* tensor_vulkan_map_unary_value(
    TensorVal* source,
    uint op,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (source.dtype != TENSOR_DTYPE_DOUBLE && source.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (source.dtype == TENSOR_DTYPE_DOUBLE && op >= 5 && op != 19 && op != 20) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (source.dtype == TENSOR_DTYPE_DOUBLE) {
        if (op == 20) {
            status = omni_tensor_backend_vulkan_map_normal_quantile_f64(
                source.device_handle,
                source.byte_len,
                source.element_count,
                &out_device
            );
        } else {
            status = omni_tensor_backend_vulkan_map_unary_f64(
                source.device_handle,
                source.byte_len,
                source.element_count,
                op,
                &out_device
            );
        }
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32) {
        if (op == 20) {
            status = omni_tensor_backend_vulkan_map_normal_quantile_f32(
                source.device_handle,
                source.byte_len,
                source.element_count,
                &out_device
            );
        } else {
            status = omni_tensor_backend_vulkan_map_unary_f32(
                source.device_handle,
                source.byte_len,
                source.element_count,
                op,
                &out_device
            );
        }
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        if (op == 20 && status == OMNI_TENSOR_VULKAN_DOMAIN_ERROR) {
            return raise_error(interp, "stats/normal-quantile: probability must be between 0 and 1");
        }
        if (op == 20 && status == OMNI_TENSOR_VULKAN_INVALID_ARGUMENT) {
            return raise_error(interp, "stats/normal-quantile: expected finite numeric input");
        }
        return tensor_vulkan_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_vulkan_map_complex_unary_value(
    TensorVal* source,
    uint op,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_VULKAN || !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = source.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_vulkan_map_complex128_unary(source.device_handle, source.byte_len, source.element_count, op, &out_device)
        : omni_tensor_backend_vulkan_map_complex64_unary(source.device_handle, source.byte_len, source.element_count, op, &out_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_vulkan_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_vulkan_map_complex_to_real_value(
    TensorVal* source,
    uint op,
    TensorDType result_dtype,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_VULKAN || !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if ((source.dtype == TENSOR_DTYPE_COMPLEX128 && result_dtype != TENSOR_DTYPE_DOUBLE) ||
        (source.dtype == TENSOR_DTYPE_COMPLEX64 && result_dtype != TENSOR_DTYPE_FLOAT32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: Vulkan complex component result dtype mismatch", null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(result_dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = source.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_vulkan_map_complex128_to_real(source.device_handle, source.byte_len, source.element_count, op, &out_device)
        : omni_tensor_backend_vulkan_map_complex64_to_real(source.device_handle, source.byte_len, source.element_count, op, &out_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_vulkan_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_cuda_map_complex_unary_value(
    TensorVal* source,
    uint op,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_CUDA || !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_cuda_complex_map_available_for_dtype(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = source.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_cuda_map_complex128_unary(source.device_handle, source.byte_len, source.element_count, op, &out_device)
        : omni_tensor_backend_cuda_map_complex64_unary(source.device_handle, source.byte_len, source.element_count, op, &out_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_cuda_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_cuda_map_complex_to_real_value(
    TensorVal* source,
    uint op,
    TensorDType result_dtype,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_CUDA || !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_cuda_complex_map_available_for_dtype(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if ((source.dtype == TENSOR_DTYPE_COMPLEX128 && result_dtype != TENSOR_DTYPE_DOUBLE) ||
        (source.dtype == TENSOR_DTYPE_COMPLEX64 && result_dtype != TENSOR_DTYPE_FLOAT32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA complex component result dtype mismatch", null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(result_dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = source.dtype == TENSOR_DTYPE_COMPLEX128
        ? omni_tensor_backend_cuda_map_complex128_to_real(source.device_handle, source.byte_len, source.element_count, op, &out_device)
        : omni_tensor_backend_cuda_map_complex64_to_real(source.device_handle, source.byte_len, source.element_count, op, &out_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_cuda_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_cuda_map_unary_value(
    TensorVal* source,
    uint op,
    usz[] result_shape,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    if (source == null || source.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_dtype_is_cuda_real(source.dtype) || !tensor_cuda_unary_op_available(op)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, missing_message);
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(source, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = OMNI_TENSOR_CUDA_INVALID;
    if (source.dtype == TENSOR_DTYPE_DOUBLE && op <= 4) {
        status = omni_tensor_backend_cuda_map_unary_f64(
            source.device_handle,
            source.byte_len,
            source.element_count,
            op,
            &out_device
        );
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32 && op <= 4) {
        status = omni_tensor_backend_cuda_map_unary_f32(
            source.device_handle,
            source.byte_len,
            source.element_count,
            op,
            &out_device
        );
    } else if (source.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_cuda_map_scientific_unary_f64(
            source.device_handle,
            source.byte_len,
            source.element_count,
            op,
            &out_device
        );
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_cuda_map_scientific_unary_f32(
            source.device_handle,
            source.byte_len,
            source.element_count,
            op,
            &out_device
        );
    }
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(result_tensor);
        if (op == 20 && status == OMNI_TENSOR_CUDA_DOMAIN_ERROR) {
            return raise_error(interp, "stats/normal-quantile: probability must be between 0 and 1");
        }
        if (op == 20 && status == OMNI_TENSOR_CUDA_INVALID_ARGUMENT) {
            return raise_error(interp, "stats/normal-quantile: expected finite numeric input");
        }
        return tensor_cuda_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_cuda_map_binary_resolved(
    TensorVal* left_tensor,
    double left_scalar,
    TensorVal* right_tensor,
    double right_scalar,
    TensorDType dtype,
    usz[] result_shape,
    uint op,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    TensorVal* tensor = left_tensor != null ? left_tensor : right_tensor;
    if (tensor == null || tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (left_tensor != null && left_tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (right_tensor != null && right_tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (!tensor_dtype_is_cuda_real(dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports matching Float64 or Float32 Tensor storage", null);
    }
    if (tensor.dtype != dtype ||
        (left_tensor != null && left_tensor.dtype != dtype) ||
        (right_tensor != null && right_tensor.dtype != dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA tensor dtype mismatch", null);
    }
    if (!tensor_dense_row_major(tensor) ||
        (left_tensor != null && !tensor_dense_row_major(left_tensor)) ||
        (right_tensor != null && !tensor_dense_row_major(right_tensor))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(tensor, result_shape) ||
        (left_tensor != null && !tensor_map_operand_matches_shape(left_tensor, result_shape)) ||
        (right_tensor != null && !tensor_map_operand_matches_shape(right_tensor, result_shape))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
    }
    if (!tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_CUDA) ||
        (left_tensor != null && !tensor_has_valid_device_storage(left_tensor, TENSOR_DEVICE_CUDA)) ||
        (right_tensor != null && !tensor_has_valid_device_storage(right_tensor, TENSOR_DEVICE_CUDA))) {
        return tensor_runtime_raise(interp, missing_message);
    }

    double scalar_f64 = 0.0;
    float scalar_f32 = 0.0f;
    uint mode = 2;
    if (left_tensor == null) {
        scalar_f64 = left_scalar;
        mode = 1;
    } else if (right_tensor == null) {
        scalar_f64 = right_scalar;
        mode = 0;
    }
    if (dtype == TENSOR_DTYPE_FLOAT32 && mode != 2 && !tensor_double_to_float32(scalar_f64, &scalar_f32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA Float32 scalar operand is not representable", null);
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = OMNI_TENSOR_CUDA_INVALID;
    if (dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_cuda_map_f64(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_f64,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    } else if (dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_cuda_map_f32(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_f32,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    }
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(result_tensor);
        return tensor_cuda_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_cuda_map_complex_binary_resolved(
    TensorVal* left_tensor,
    Complex128Val left_complex128_scalar,
    Complex64Val left_complex64_scalar,
    TensorVal* right_tensor,
    Complex128Val right_complex128_scalar,
    Complex64Val right_complex64_scalar,
    TensorDType dtype,
    usz[] result_shape,
    uint op,
    Interp* interp,
    char[] allocation_message,
    char[] value_message,
    char[] unsupported_message,
    char[] missing_message,
    char[] kernel_message
) {
    TensorVal* tensor = left_tensor != null ? left_tensor : right_tensor;
    if (tensor == null || tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (left_tensor != null && left_tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (right_tensor != null && right_tensor.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (!tensor_dtype_is_fixed_complex(dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports matching Complex128 or Complex64 Tensor storage", null);
    }
    if (!tensor_cuda_complex_map_available_for_dtype(dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (tensor.dtype != dtype ||
        (left_tensor != null && left_tensor.dtype != dtype) ||
        (right_tensor != null && right_tensor.dtype != dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA tensor dtype mismatch", null);
    }
    if (!tensor_dense_row_major(tensor) ||
        (left_tensor != null && !tensor_dense_row_major(left_tensor)) ||
        (right_tensor != null && !tensor_dense_row_major(right_tensor))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_message, null);
    }
    if (!tensor_map_operand_matches_shape(tensor, result_shape) ||
        (left_tensor != null && !tensor_map_operand_matches_shape(left_tensor, result_shape)) ||
        (right_tensor != null && !tensor_map_operand_matches_shape(right_tensor, result_shape))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
    }
    if (!tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_CUDA) ||
        (left_tensor != null && !tensor_has_valid_device_storage(left_tensor, TENSOR_DEVICE_CUDA)) ||
        (right_tensor != null && !tensor_has_valid_device_storage(right_tensor, TENSOR_DEVICE_CUDA))) {
        return tensor_runtime_raise(interp, missing_message);
    }

    uint mode = 2;
    if (left_tensor == null) {
        mode = 1;
    } else if (right_tensor == null) {
        mode = 0;
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(dtype, result_shape, false, false, allocation_message);
    if (result_tensor == null) return tensor_runtime_raise(interp, allocation_message);
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = OMNI_TENSOR_CUDA_INVALID;
    if (dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val scalar = mode == 1 ? left_complex128_scalar : right_complex128_scalar;
        status = omni_tensor_backend_cuda_map_complex128(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar.real,
            scalar.imag,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    } else if (dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val scalar = mode == 1 ? left_complex64_scalar : right_complex64_scalar;
        status = omni_tensor_backend_cuda_map_complex64(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar.real,
            scalar.imag,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    }
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(result_tensor);
        if (status == OMNI_TENSOR_CUDA_DOMAIN_ERROR) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "map: complex division by zero", null);
        }
        if (status == OMNI_TENSOR_CUDA_INVALID_ARGUMENT) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "map: complex result is not representable", null);
        }
        return tensor_cuda_status_raise(interp, status, kernel_message);
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, value_message);
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
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

fn Value* tensor_map_apply_unary_complex128(Value* func, Complex128Val a, Complex128Val* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* arg = make_complex128(interp, a.real, a.imag);
    Value* arg_list = make_cons_or_error(interp, arg, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!fixed_complex_value_to_complex128(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary_complex128(Value* func, Complex128Val a, Complex128Val b, Complex128Val* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* tail = make_cons_or_error(interp, make_complex128(interp, b.real, b.imag), make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* arg_list = make_cons_or_error(interp, make_complex128(interp, a.real, a.imag), tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!fixed_complex_value_to_complex128(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_unary_complex64(Value* func, Complex64Val a, Complex64Val* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* arg = make_complex64(interp, a.real, a.imag);
    Value* arg_list = make_cons_or_error(interp, arg, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!fixed_complex_value_to_complex64(result, out)) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary_complex64(Value* func, Complex64Val a, Complex64Val b, Complex64Val* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    Value* tail = make_cons_or_error(interp, make_complex64(interp, b.real, b.imag), make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* arg_list = make_cons_or_error(interp, make_complex64(interp, a.real, a.imag), tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    if (!fixed_complex_value_to_complex64(result, out)) return tensor_map_dtype_raise(interp);
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

fn Value* tensor_map_apply_unary_big_complex(Value* func, void* a, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null) return tensor_runtime_raise(interp, "map: BigComplex tensor input is missing");
    Value* arg = make_big_complex_from_owned_handle(interp, omni_big_complex_clone(a), "map: failed to allocate tensor call argument");
    if (arg == null || arg.tag == ERROR) return arg;
    Value* arg_list = make_cons_or_error(interp, arg, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;
    Value* result = jit_apply_multi_args(interp, func, arg_list, 1);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_complex_handle_from_value(result);
    if (*out == null) return tensor_map_dtype_raise(interp);
    return null;
}

fn Value* tensor_map_apply_binary_big_complex(Value* func, void* a, void* b, void** out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "map: tensor result storage is missing");
    *out = null;
    if (a == null || b == null) return tensor_runtime_raise(interp, "map: BigComplex tensor input is missing");
    Value* right = make_big_complex_from_owned_handle(interp, omni_big_complex_clone(b), "map: failed to allocate tensor call argument");
    if (right == null || right.tag == ERROR) return right;
    Value* tail = make_cons_or_error(interp, right, make_nil(interp), "map: failed to allocate tensor call arguments");
    if (tail == null || tail.tag == ERROR) return tail;
    Value* left = make_big_complex_from_owned_handle(interp, omni_big_complex_clone(a), "map: failed to allocate tensor call argument");
    if (left == null || left.tag == ERROR) return left;
    Value* arg_list = make_cons_or_error(interp, left, tail, "map: failed to allocate tensor call arguments");
    if (arg_list == null || arg_list.tag == ERROR) return arg_list;

    Value* result = jit_apply_multi_args(interp, func, arg_list, 2);
    if (result == null) return tensor_runtime_raise(interp, "map: function returned invalid value");
    if (result.tag == ERROR) return result;
    *out = big_complex_handle_from_value(result);
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
    if (tensor_value_is_numeric_leaf(source)) return null;
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

fn bool tensor_map_operand_matches_shape(TensorVal* operand, usz[] result_shape) {
    if (operand == null) return false;
    if (!tensor_has_valid_metadata(operand)) return false;
    if (operand.rank > result_shape.len) return false;
    if (operand.rank == 0) return true;

    usz axis_delta = result_shape.len - operand.rank;
    for (usz out_axis = 0; out_axis < result_shape.len; out_axis++) {
        if (out_axis < axis_delta) continue;
        usz operand_axis = out_axis - axis_delta;
        usz operand_dim = operand.shape[operand_axis];
        usz out_dim = result_shape[out_axis];
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

fn bool tensor_map_tensor_dtype_matches_result(TensorDType source, TensorDType result) {
    if (source == result) return true;
    if (result == TENSOR_DTYPE_COMPLEX128) {
        return source == TENSOR_DTYPE_COMPLEX64 ||
            source == TENSOR_DTYPE_DOUBLE ||
            source == TENSOR_DTYPE_FLOAT32 ||
            source == TENSOR_DTYPE_BIG_INTEGER ||
            source == TENSOR_DTYPE_BIG_FLOAT;
    }
    if (result == TENSOR_DTYPE_COMPLEX64) {
        return source == TENSOR_DTYPE_DOUBLE ||
            source == TENSOR_DTYPE_FLOAT32 ||
            source == TENSOR_DTYPE_BIG_INTEGER ||
            source == TENSOR_DTYPE_BIG_FLOAT;
    }
    return false;
}

fn bool tensor_read_complex128_at(TensorVal* tensor, usz offset, Complex128Val* out) {
    if (tensor == null || out == null || offset >= tensor.element_count) return false;
    switch (tensor.dtype) {
        case TENSOR_DTYPE_COMPLEX128:
            *out = ((Complex128Val*)tensor.data)[offset];
            return true;
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val value = ((Complex64Val*)tensor.data)[offset];
            out.real = (double)value.real;
            out.imag = (double)value.imag;
            return true;
        }
        case TENSOR_DTYPE_DOUBLE:
            out.real = ((double*)tensor.data)[offset];
            out.imag = 0.0;
            return conversion_double_is_finite(out.real);
        case TENSOR_DTYPE_FLOAT32:
            out.real = (double)((float*)tensor.data)[offset];
            out.imag = 0.0;
            return true;
        case TENSOR_DTYPE_BIG_INTEGER: {
            Value value;
            value.tag = BIG_INTEGER;
            value.big_integer_val = ((void**)tensor.data)[offset];
            return fixed_complex_value_to_complex128(&value, out);
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            Value value;
            value.tag = BIG_FLOAT;
            value.big_float_val = ((void**)tensor.data)[offset];
            return fixed_complex_value_to_complex128(&value, out);
        }
        default:
            return false;
    }
}

fn bool tensor_read_complex64_at(TensorVal* tensor, usz offset, Complex64Val* out) {
    if (tensor == null || out == null || offset >= tensor.element_count) return false;
    switch (tensor.dtype) {
        case TENSOR_DTYPE_COMPLEX64:
            *out = ((Complex64Val*)tensor.data)[offset];
            return true;
        default: {
            Complex128Val wide;
            if (!tensor_read_complex128_at(tensor, offset, &wide)) return false;
            if (!conversion_try_double_to_float32(wide.real, &out.real)) return false;
            if (!conversion_try_double_to_float32(wide.imag, &out.imag)) return false;
            return true;
        }
    }
}

fn Value* tensor_map_eval_into(
    Value* func,
    TensorVal* left_tensor,
    double left_scalar,
    void* left_big_integer_scalar,
    void* left_big_float_scalar,
    void* left_big_complex_scalar,
    Complex128Val left_complex128_scalar,
    Complex64Val left_complex64_scalar,
    TensorVal* right_tensor,
    double right_scalar,
    void* right_big_integer_scalar,
    void* right_big_float_scalar,
    void* right_big_complex_scalar,
    Complex128Val right_complex128_scalar,
    Complex64Val right_complex64_scalar,
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
        if (!tensor_map_tensor_dtype_matches_result(left_tensor.dtype, out_tensor.dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: tensor dtype mismatch", null);
        }
        if (!tensor_map_operand_matches_output(left_tensor, out_tensor)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
        }
    }
    if (right_tensor != null) {
        if (!tensor_has_valid_storage(right_tensor)) return tensor_runtime_raise(interp, "map: tensor backing storage is missing");
        if (!tensor_map_tensor_dtype_matches_result(right_tensor.dtype, out_tensor.dtype)) {
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
        case TENSOR_DTYPE_FLOAT32: {
            float* out = (float*)out_tensor.data;
            float* left_data = left_tensor != null ? (float*)left_tensor.data : null;
            float* right_data = right_tensor != null ? (float*)right_tensor.data : null;

            for (usz i = 0; i < out_tensor.element_count; i++) {
                double result = 0.0;
                Value* err = null;
                if (!is_binary) {
                    usz left_offset = 0;
                    if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                    }
                    err = tensor_map_apply_unary(func, (double)left_data[left_offset], &result, interp);
                } else {
                    double a = left_scalar;
                    double b = right_scalar;
                    if (left_tensor != null) {
                        usz left_offset = 0;
                        if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                        }
                        a = (double)left_data[left_offset];
                    }
                    if (right_tensor != null) {
                        usz right_offset = 0;
                        if (!tensor_map_operand_offset(right_tensor, out_tensor, i, &right_offset)) {
                            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                        }
                        b = (double)right_data[right_offset];
                    }
                    err = tensor_map_apply_binary(func, a, b, &result, interp);
                }
                if (err != null) return err;
                if (!conversion_double_is_finite(result) ||
                    result > TENSOR_FLOAT32_FINITE_LIMIT ||
                    result < -TENSOR_FLOAT32_FINITE_LIMIT) {
                    return tensor_type_raise(interp, "map: result is not representable as Float32");
                }
                out[i] = (float)result;
            }
            return null;
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val* out = (Complex128Val*)out_tensor.data;

            for (usz i = 0; i < out_tensor.element_count; i++) {
                Complex128Val result;
                Value* err = null;
                if (!is_binary) {
                    usz left_offset = 0;
                    if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                    }
                    Complex128Val a;
                    if (!tensor_read_complex128_at(left_tensor, left_offset, &a)) {
                        return tensor_type_raise(interp, "map: tensor input is not representable as Complex128");
                    }
                    err = tensor_map_apply_unary_complex128(func, a, &result, interp);
                } else {
                    Complex128Val a = left_complex128_scalar;
                    Complex128Val b = right_complex128_scalar;
                    if (left_tensor != null) {
                        usz left_offset = 0;
                        if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                        }
                        if (!tensor_read_complex128_at(left_tensor, left_offset, &a)) {
                            return tensor_type_raise(interp, "map: tensor input is not representable as Complex128");
                        }
                    }
                    if (right_tensor != null) {
                        usz right_offset = 0;
                        if (!tensor_map_operand_offset(right_tensor, out_tensor, i, &right_offset)) {
                            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                        }
                        if (!tensor_read_complex128_at(right_tensor, right_offset, &b)) {
                            return tensor_type_raise(interp, "map: tensor input is not representable as Complex128");
                        }
                    }
                    err = tensor_map_apply_binary_complex128(func, a, b, &result, interp);
                }
                if (err != null) return err;
                out[i] = result;
            }
            return null;
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val* out = (Complex64Val*)out_tensor.data;

            for (usz i = 0; i < out_tensor.element_count; i++) {
                Complex64Val result;
                Value* err = null;
                if (!is_binary) {
                    usz left_offset = 0;
                    if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                    }
                    Complex64Val a;
                    if (!tensor_read_complex64_at(left_tensor, left_offset, &a)) {
                        return tensor_type_raise(interp, "map: tensor input is not representable as Complex64");
                    }
                    err = tensor_map_apply_unary_complex64(func, a, &result, interp);
                } else {
                    Complex64Val a = left_complex64_scalar;
                    Complex64Val b = right_complex64_scalar;
                    if (left_tensor != null) {
                        usz left_offset = 0;
                        if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                        }
                        if (!tensor_read_complex64_at(left_tensor, left_offset, &a)) {
                            return tensor_type_raise(interp, "map: tensor input is not representable as Complex64");
                        }
                    }
                    if (right_tensor != null) {
                        usz right_offset = 0;
                        if (!tensor_map_operand_offset(right_tensor, out_tensor, i, &right_offset)) {
                            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: tensor shape mismatch", null);
                        }
                        if (!tensor_read_complex64_at(right_tensor, right_offset, &b)) {
                            return tensor_type_raise(interp, "map: tensor input is not representable as Complex64");
                        }
                    }
                    err = tensor_map_apply_binary_complex64(func, a, b, &result, interp);
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
        case TENSOR_DTYPE_BIG_COMPLEX: {
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
                    err = tensor_map_apply_unary_big_complex(func, left_data[left_offset], &result, interp);
                } else {
                    void* a = left_big_complex_scalar;
                    void* b = right_big_complex_scalar;
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
                    err = tensor_map_apply_binary_big_complex(func, a, b, &result, interp);
                }
                if (err != null) {
                    if (result != null) omni_big_complex_free(result);
                    return err;
                }
                if (out[i] != null) omni_big_complex_free(out[i]);
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
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers representable as Float64");
                }
            case TENSOR_DTYPE_FLOAT32: {
                float scalar = 0.0f;
                if (!tensor_value_to_float32(left_value, &scalar)) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers representable as Float32");
                }
                tensor.map_left_scalar = (double)scalar;
            }
            case TENSOR_DTYPE_COMPLEX128:
                if (!fixed_complex_value_to_complex128(left_value, &tensor.map_left_complex128_scalar)) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers representable as Complex128");
                }
            case TENSOR_DTYPE_COMPLEX64:
                if (!fixed_complex_value_to_complex64(left_value, &tensor.map_left_complex64_scalar)) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers representable as Complex64");
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
            case TENSOR_DTYPE_BIG_COMPLEX:
                tensor.map_left_big_complex_scalar = big_complex_handle_from_value(left_value);
                if (tensor.map_left_big_complex_scalar == null) {
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
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers representable as Float64");
                }
            case TENSOR_DTYPE_FLOAT32: {
                float scalar = 0.0f;
                if (!tensor_value_to_float32(right_value, &scalar)) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers representable as Float32");
                }
                tensor.map_right_scalar = (double)scalar;
            }
            case TENSOR_DTYPE_COMPLEX128:
                if (!fixed_complex_value_to_complex128(right_value, &tensor.map_right_complex128_scalar)) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers representable as Complex128");
                }
            case TENSOR_DTYPE_COMPLEX64:
                if (!fixed_complex_value_to_complex64(right_value, &tensor.map_right_complex64_scalar)) {
                    tensor_free_payload(tensor);
                    return tensor_type_raise(interp, "map: tensor inputs must be Tensors or numbers representable as Complex64");
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
            case TENSOR_DTYPE_BIG_COMPLEX:
                tensor.map_right_big_complex_scalar = big_complex_handle_from_value(right_value);
                if (tensor.map_right_big_complex_scalar == null) {
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

fn bool tensor_map_input_is_fixed_complex(Value* value, TensorVal* tensor) @inline {
    if (tensor != null) return tensor_dtype_is_fixed_complex(tensor.dtype);
    return is_fixed_complex(value);
}

fn bool tensor_map_input_is_complex128(Value* value, TensorVal* tensor) @inline {
    if (tensor != null) return tensor.dtype == TENSOR_DTYPE_COMPLEX128;
    return is_complex128(value);
}

fn bool tensor_map_input_is_big_complex(Value* value, TensorVal* tensor) @inline {
    if (tensor != null) return tensor.dtype == TENSOR_DTYPE_BIG_COMPLEX;
    return is_big_complex(value);
}

fn bool tensor_map_promoted_result_dtype(
    Value* left_value,
    TensorVal* left_tensor,
    Value* right_value,
    TensorVal* right_tensor,
    bool is_binary,
    TensorDType* out
) {
    if (out == null) return false;
    TensorDType dtype = left_tensor != null ? left_tensor.dtype : TENSOR_DTYPE_DOUBLE;
    if (is_binary && left_tensor == null && right_tensor != null) dtype = right_tensor.dtype;

    bool left_fixed = tensor_map_input_is_fixed_complex(left_value, left_tensor);
    bool right_fixed = is_binary && tensor_map_input_is_fixed_complex(right_value, right_tensor);
    if (left_fixed || right_fixed) {
        if (tensor_map_input_is_big_complex(left_value, left_tensor) ||
            (is_binary && tensor_map_input_is_big_complex(right_value, right_tensor))) {
            return false;
        }
        if (tensor_map_input_is_complex128(left_value, left_tensor) ||
            (is_binary && tensor_map_input_is_complex128(right_value, right_tensor))) {
            *out = TENSOR_DTYPE_COMPLEX128;
        } else {
            *out = TENSOR_DTYPE_COMPLEX64;
        }
        return true;
    }

    *out = dtype;
    return true;
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
        if (!tensor_map_promoted_result_dtype(args[1], left_tensor, args[2], right_tensor, true, &result_dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: fixed-width complex operands do not mix with BigComplex", null);
        }
        if ((left_tensor != null && !tensor_map_tensor_dtype_matches_result(left_tensor.dtype, result_dtype)) ||
            (right_tensor != null && !tensor_map_tensor_dtype_matches_result(right_tensor.dtype, result_dtype))) {
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
    bool touches_cuda = (left_tensor != null && tensor_expr_has_device(args[1], TENSOR_DEVICE_CUDA)) ||
        (args.len == 3 && right_tensor != null && tensor_expr_has_device(args[2], TENSOR_DEVICE_CUDA));
    if (touches_cuda) {
        uint cuda_op = 0;
        Value* preflight_err = tensor_map_cuda_callable_op_or_raise(func, args.len == 3, result_dtype, &cuda_op, interp);
        if (preflight_err != null) return preflight_err;
        if ((left_tensor != null && tensor_expr_has_non_cuda_device(args[1])) ||
            (args.len == 3 && right_tensor != null && tensor_expr_has_non_cuda_device(args[2]))) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
        }
        if (!tensor_dtype_is_cuda_real(result_dtype) && !tensor_dtype_is_fixed_complex(result_dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
        if ((left_tensor != null && left_tensor.dtype != result_dtype) ||
            (args.len == 3 && right_tensor != null && right_tensor.dtype != result_dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA tensor dtype mismatch", null);
        }
    }
    Value* vulkan_result = tensor_map_try_vulkan_direct(
        func,
        args[1],
        left_tensor,
        args.len == 3 ? args[2] : null,
        right_tensor,
        args.len == 3,
        result_dtype,
        shape_slice,
        interp
    );
    if (vulkan_result != null) return vulkan_result;
    Value* cuda_result = tensor_map_try_cuda_direct(
        func,
        args[1],
        left_tensor,
        args.len == 3 ? args[2] : null,
        right_tensor,
        args.len == 3,
        result_dtype,
        shape_slice,
        interp
    );
    if (cuda_result != null) return cuda_result;
    if ((left_tensor != null && left_tensor.device != TENSOR_DEVICE_CPU) ||
        (right_tensor != null && right_tensor.device != TENSOR_DEVICE_CPU)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: unsupported device operands", null);
    }
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
        case TENSOR_DTYPE_FLOAT32: {
            float* out = (float*)out_tensor.data;
            float* left_data = (float*)left.data;
            float* right_data = (float*)right.data;

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
                    out[out_index] = 0.0f;
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
                    sum += (double)left_data[left_offset] * (double)right_data[right_offset];
                }
                float narrowed = 0.0f;
                if (!tensor_double_to_float32(sum, &narrowed)) {
                    return tensor_type_raise(interp, "contract: result is not representable as Float32");
                }
                out[out_index] = narrowed;
            }
            return null;
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val* out = (Complex128Val*)out_tensor.data;
            Complex128Val* left_data = (Complex128Val*)left.data;
            Complex128Val* right_data = (Complex128Val*)right.data;

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

                double sum_real = 0.0;
                double sum_imag = 0.0;
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
                    Complex128Val l = left_data[left_offset];
                    Complex128Val r = right_data[right_offset];
                    sum_real += l.real * r.real - l.imag * r.imag;
                    sum_imag += l.real * r.imag + l.imag * r.real;
                }
                out[out_index].real = sum_real;
                out[out_index].imag = sum_imag;
            }
            return null;
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val* out = (Complex64Val*)out_tensor.data;
            Complex64Val* left_data = (Complex64Val*)left.data;
            Complex64Val* right_data = (Complex64Val*)right.data;

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

                double sum_real = 0.0;
                double sum_imag = 0.0;
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
                    Complex64Val l = left_data[left_offset];
                    Complex64Val r = right_data[right_offset];
                    sum_real += (double)l.real * (double)r.real - (double)l.imag * (double)r.imag;
                    sum_imag += (double)l.real * (double)r.imag + (double)l.imag * (double)r.real;
                }
                if (!tensor_double_to_float32(sum_real, &out[out_index].real) ||
                    !tensor_double_to_float32(sum_imag, &out[out_index].imag)) {
                    return tensor_type_raise(interp, "contract: result is not representable as Complex64");
                }
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
        case TENSOR_DTYPE_BIG_COMPLEX: {
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

                void* sum = omni_big_complex_from_i64(0);
                if (sum == null) return raise_error(interp, "contract: failed to allocate BigComplex accumulator");
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
                    void* product = omni_big_complex_binary(left_data[left_offset], right_data[right_offset], BIG_COMPLEX_OP_MUL);
                    if (product == null) {
                        ok = false;
                        break;
                    }
                    void* next_sum = omni_big_complex_binary(sum, product, BIG_COMPLEX_OP_ADD);
                    omni_big_complex_free(product);
                    if (next_sum == null) {
                        ok = false;
                        break;
                    }
                    omni_big_complex_free(sum);
                    sum = next_sum;
                }
                if (!ok) {
                    omni_big_complex_free(sum);
                    return raise_error(interp, "contract: failed to compute BigComplex contraction");
                }
                if (out[out_index] != null) omni_big_complex_free(out[out_index]);
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
        Value* vulkan_result = tensor_contract_try_vulkan_value(left, right, left_axes, right_axes, left_axis_count, result_shape_slice, interp);
        if (vulkan_result != null) return vulkan_result;
        Value* cuda_result = tensor_contract_try_cuda_value(left, right, left_axes, right_axes, left_axis_count, result_shape_slice, interp);
        if (cuda_result != null) return cuda_result;
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
    Value* vulkan_result = tensor_contract_try_vulkan_value(left, right, left_axes, right_axes, left_axis_count, result_shape_slice, interp);
    if (vulkan_result != null) return vulkan_result;
    Value* cuda_result = tensor_contract_try_cuda_value(left, right, left_axes, right_axes, left_axis_count, result_shape_slice, interp);
    if (cuda_result != null) return cuda_result;
    return make_tensor_contract_expr(args[0], args[1], left_axes, right_axes, left_axis_count, result_shape_slice, interp);
}

fn Value* tensor_expr_resolve_concrete_any_device(Value* source_value, TensorVal** out_tensor, Value** out_value, Interp* interp, char[] label) {
    if (out_tensor == null) return tensor_runtime_raise(interp, label);
    *out_tensor = null;
    if (out_value != null) *out_value = null;
    if (!is_tensor(source_value)) return tensor_type_raise(interp, label);

    TensorVal* source = source_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, label);
    Value* realized = source.kind == TENSOR_PAYLOAD_CONCRETE ? source_value : tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    if (!is_tensor(realized) || realized.tensor_val == null || !tensor_has_valid_metadata(realized.tensor_val) || realized.tensor_val.kind != TENSOR_PAYLOAD_CONCRETE) {
        return tensor_runtime_raise(interp, label);
    }
    *out_tensor = realized.tensor_val;
    if (out_value != null) *out_value = realized;
    return null;
}

fn Value* tensor_map_try_cuda_value(TensorVal* source, Interp* interp) {
    if (source == null || source.kind != TENSOR_PAYLOAD_MAP) return null;

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

    bool touches_cuda = (source.map_left != null && tensor_expr_has_device(source.map_left, TENSOR_DEVICE_CUDA)) ||
        (source.map_is_binary && source.map_right != null && tensor_expr_has_device(source.map_right, TENSOR_DEVICE_CUDA));
    if (!touches_cuda) return null;

    uint op = 0;
    Value* preflight_err = tensor_map_cuda_callable_op_or_raise(source.map_func, source.map_is_binary, source.dtype, &op, interp);
    if (preflight_err != null) return preflight_err;

    if ((source.map_left != null && tensor_expr_has_non_cuda_device(source.map_left)) ||
        (source.map_is_binary && source.map_right != null && tensor_expr_has_non_cuda_device(source.map_right))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (!tensor_dtype_is_cuda_real(source.dtype) && !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if ((source.map_left != null && source.map_left.tensor_val != null && source.map_left.tensor_val.dtype != source.dtype) ||
        (source.map_is_binary && source.map_right != null && source.map_right.tensor_val != null && source.map_right.tensor_val.dtype != source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA tensor dtype mismatch", null);
    }

    if (source.map_left != null) {
        err = tensor_expr_resolve_concrete_any_device(source.map_left, &left_tensor, &left_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }
    if (source.map_is_binary && source.map_right != null) {
        err = tensor_expr_resolve_concrete_any_device(source.map_right, &right_tensor, &right_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }

    if (!source.map_is_binary) {
        if (left_tensor == null || left_tensor.device != TENSOR_DEVICE_CUDA) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
        }
        if (!tensor_dtype_is_cuda_real(source.dtype) && !tensor_dtype_is_fixed_complex(source.dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
        if (!tensor_map_operand_matches_output(left_tensor, source)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
        }
        usz[1] scalar_shape = { 0 };
        usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            return tensor_cuda_map_complex_unary_value(
                left_tensor,
                op,
                shape_slice,
                interp,
                "map: failed to allocate CUDA tensor metadata",
                "map: failed to allocate CUDA tensor value",
                "map: CUDA currently supports dense row-major Complex128 and Complex64 unary operations",
                "map: CUDA tensor backing storage is missing",
                "map: CUDA complex unary kernel failed"
            );
        }
        return tensor_cuda_map_unary_value(
            left_tensor,
            op,
            shape_slice,
            interp,
            "map: failed to allocate CUDA tensor metadata",
            "map: failed to allocate CUDA tensor value",
            "map: CUDA currently supports dense row-major Float64 and Float32 unary operations",
            "map: CUDA tensor backing storage is missing",
            "map: CUDA unary kernel failed"
        );
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (tensor_dtype_is_fixed_complex(source.dtype)) {
        return tensor_cuda_map_complex_binary_resolved(
            left_tensor,
            source.map_left_complex128_scalar,
            source.map_left_complex64_scalar,
            right_tensor,
            source.map_right_complex128_scalar,
            source.map_right_complex64_scalar,
            source.dtype,
            shape_slice,
            op,
            interp,
            "map: failed to allocate CUDA tensor metadata",
            "map: failed to allocate CUDA tensor value",
            "map: CUDA currently supports dense row-major Complex128 and Complex64 binary operations",
            "map: CUDA tensor backing storage is missing",
            "map: CUDA complex kernel failed"
        );
    }
    return tensor_cuda_map_binary_resolved(
        left_tensor,
        source.map_left_scalar,
        right_tensor,
        source.map_right_scalar,
        source.dtype,
        shape_slice,
        op,
        interp,
        "map: failed to allocate CUDA tensor metadata",
        "map: failed to allocate CUDA tensor value",
        "map: CUDA currently supports dense row-major Float64 and Float32 binary operations",
        "map: CUDA tensor backing storage is missing",
        "map: CUDA kernel failed"
    );
}

fn Value* tensor_map_vulkan_callable_op_or_raise(Value* func, bool is_binary, TensorDType dtype, uint* out_op, Interp* interp) {
    if (out_op == null) return tensor_runtime_raise(interp, "map: Vulkan callable state is missing");
    uint op = 0;
    if (tensor_dtype_is_fixed_complex(dtype)) {
        bool supported = is_binary
            ? tensor_map_callable_complex_binary_op(func, &op)
            : tensor_map_callable_complex_unary_op(func, &op);
        if (!supported) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Complex128 and Complex64 arithmetic elementwise kernels", null);
        }
    } else if (!is_binary) {
        if (!tensor_map_callable_vulkan_unary_op(func, &op)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", null);
        }
    } else if (!tensor_map_callable_vulkan_op(func, &op)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64 and Float32 arithmetic kernels", null);
    }
    *out_op = op;
    return null;
}

fn Value* tensor_map_try_vulkan_value(TensorVal* source, Interp* interp) {
    if (source == null || source.kind != TENSOR_PAYLOAD_MAP) return null;

    bool touches_vulkan = (source.map_left != null && tensor_expr_has_device(source.map_left, TENSOR_DEVICE_VULKAN)) ||
        (source.map_is_binary && source.map_right != null && tensor_expr_has_device(source.map_right, TENSOR_DEVICE_VULKAN));
    if (!touches_vulkan) return null;

    uint op = 0;
    Value* preflight_err = tensor_map_vulkan_callable_op_or_raise(source.map_func, source.map_is_binary, source.dtype, &op, interp);
    if (preflight_err != null) return preflight_err;

    return tensor_map_try_vulkan_value_checked(source, op, interp);
}

fn Value* tensor_map_try_vulkan_value_checked(TensorVal* source, uint op, Interp* interp) {

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

    if ((source.map_left != null && tensor_expr_has_non_vulkan_device(source.map_left)) ||
        (source.map_is_binary && source.map_right != null && tensor_expr_has_non_vulkan_device(source.map_right))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (!tensor_dtype_is_vulkan_real(source.dtype) && !tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if ((source.map_left != null && source.map_left.tensor_val != null && source.map_left.tensor_val.dtype != source.dtype) ||
        (source.map_is_binary && source.map_right != null && source.map_right.tensor_val != null && source.map_right.tensor_val.dtype != source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports matching Tensor storage dtypes", null);
    }

    if (source.map_left != null) {
        err = tensor_expr_resolve_concrete_any_device(source.map_left, &left_tensor, &left_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }
    if (source.map_is_binary && source.map_right != null) {
        err = tensor_expr_resolve_concrete_any_device(source.map_right, &right_tensor, &right_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }

    if (!source.map_is_binary) {
        if (left_tensor == null || left_tensor.device != TENSOR_DEVICE_VULKAN) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
        }
        if (!tensor_dtype_is_vulkan_real(source.dtype) && !tensor_dtype_is_fixed_complex(source.dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
        if (!tensor_map_operand_matches_output(left_tensor, source)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
        }
        usz[1] scalar_shape = { 0 };
        usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            return tensor_vulkan_map_complex_unary_value(
                left_tensor,
                op,
                shape_slice,
                interp,
                "map: failed to allocate Vulkan tensor metadata",
                "map: failed to allocate Vulkan tensor value",
                "map: Vulkan currently supports dense row-major Complex128 and Complex64 unary operations",
                "map: Vulkan tensor backing storage is missing",
                "map: Vulkan complex unary kernel failed"
            );
        }
        return tensor_vulkan_map_unary_value(
            left_tensor,
            op,
            shape_slice,
            interp,
            "map: failed to allocate Vulkan tensor metadata",
            "map: failed to allocate Vulkan tensor value",
            "map: Vulkan currently supports dense row-major Float64 and Float32 unary operations",
            "map: Vulkan tensor backing storage is missing",
            "map: Vulkan unary kernel failed"
        );
    }

    TensorVal* tensor = left_tensor != null ? left_tensor : right_tensor;
    double scalar_f64 = 0.0;
    float scalar_f32 = 0.0f;
    uint mode = 2;
    if (left_tensor == null) {
        scalar_f64 = source.map_left_scalar;
        mode = 1;
    } else if (right_tensor == null) {
        scalar_f64 = source.map_right_scalar;
        mode = 0;
    }
    if (tensor == null || tensor.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (left_tensor != null && left_tensor.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (right_tensor != null && right_tensor.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if ((!tensor_dtype_is_vulkan_real(source.dtype) && !tensor_dtype_is_fixed_complex(source.dtype)) || tensor.dtype != source.dtype ||
        (left_tensor != null && left_tensor.dtype != source.dtype) ||
        (right_tensor != null && right_tensor.dtype != source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports matching Tensor storage dtypes", null);
    }
    if (source.dtype == TENSOR_DTYPE_FLOAT32 && mode != 2 && !tensor_double_to_float32(scalar_f64, &scalar_f32)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan Float32 scalar operand is not representable", null);
    }
    if (!tensor_map_operand_matches_output(tensor, source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (left_tensor != null && !tensor_map_operand_matches_output(left_tensor, source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (right_tensor != null && !tensor_map_operand_matches_output(right_tensor, source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (!tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_VULKAN) ||
        (left_tensor != null && !tensor_has_valid_device_storage(left_tensor, TENSOR_DEVICE_VULKAN)) ||
        (right_tensor != null && !tensor_has_valid_device_storage(right_tensor, TENSOR_DEVICE_VULKAN))) {
        return tensor_runtime_raise(interp, "map: Vulkan tensor backing storage is missing");
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    TensorVal* result_tensor = tensor_alloc_payload_base_checked(source.dtype, shape_slice, false, false, "map: failed to allocate Vulkan tensor metadata");
    if (result_tensor == null) return tensor_runtime_raise(interp, "map: failed to allocate Vulkan tensor metadata");
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (source.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_map_f64(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_f64,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_map_f32(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_f32,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    } else if (source.dtype == TENSOR_DTYPE_COMPLEX128) {
        Complex128Val scalar = mode == 1 ? source.map_left_complex128_scalar : source.map_right_complex128_scalar;
        status = omni_tensor_backend_vulkan_map_complex128(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar.real,
            scalar.imag,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    } else if (source.dtype == TENSOR_DTYPE_COMPLEX64) {
        Complex64Val scalar = mode == 1 ? source.map_left_complex64_scalar : source.map_right_complex64_scalar;
        status = omni_tensor_backend_vulkan_map_complex64(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar.real,
            scalar.imag,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        if (tensor_dtype_is_fixed_complex(source.dtype) && status == OMNI_TENSOR_VULKAN_DOMAIN_ERROR) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "map: complex division by zero", null);
        }
        if (tensor_dtype_is_fixed_complex(source.dtype) && status == OMNI_TENSOR_VULKAN_INVALID_ARGUMENT) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "map: complex result is not representable", null);
        }
        return tensor_vulkan_status_raise(interp, status, "map: Vulkan kernel failed");
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, "map: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_contract_try_device_value(TensorVal* source, Interp* interp) {
    if (source == null || source.kind != TENSOR_PAYLOAD_CONTRACT) return null;

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

    Value* err = tensor_expr_resolve_concrete_any_device(source.contract_left, &left_tensor, &left_value, interp, "contract: tensor backing storage is missing");
    if (err != null) return err;
    err = tensor_expr_resolve_concrete_any_device(source.contract_right, &right_tensor, &right_value, interp, "contract: tensor backing storage is missing");
    if (err != null) return err;

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* vulkan_result = tensor_contract_try_vulkan_value(
        left_tensor,
        right_tensor,
        source.contract_left_axes,
        source.contract_right_axes,
        source.contract_axis_count,
        shape_slice,
        interp
    );
    if (vulkan_result != null) return vulkan_result;
    Value* cuda_result = tensor_contract_try_cuda_value(
        left_tensor,
        right_tensor,
        source.contract_left_axes,
        source.contract_right_axes,
        source.contract_axis_count,
        shape_slice,
        interp
    );
    if (cuda_result != null) return cuda_result;

    if ((left_tensor != null && left_tensor.device != TENSOR_DEVICE_CPU) ||
        (right_tensor != null && right_tensor.device != TENSOR_DEVICE_CPU)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: non-CPU Tensor operands require supported backend contract execution", null);
    }
    return null;
}

fn Value* tensor_map_try_cuda_direct(
    Value* func,
    Value* left_source,
    TensorVal* left_source_tensor,
    Value* right_source,
    TensorVal* right_source_tensor,
    bool is_binary,
    TensorDType dtype,
    usz[] result_shape,
    Interp* interp
) {
    TensorVal* left_tensor = null;
    TensorVal* right_tensor = null;
    Value* left_value = null;
    Value* right_value = null;
    Value* err = null;
    defer if (left_value != null && left_value != left_source) {
        boundary_cleanup_materialized_value(null, left_value, interp.current_scope);
    }
    defer if (right_value != null && right_value != right_source) {
        boundary_cleanup_materialized_value(null, right_value, interp.current_scope);
    }

    bool touches_cuda = (left_source_tensor != null && tensor_expr_has_device(left_source, TENSOR_DEVICE_CUDA)) ||
        (is_binary && right_source_tensor != null && tensor_expr_has_device(right_source, TENSOR_DEVICE_CUDA));
    if (!touches_cuda) return null;

    uint op = 0;
    Value* preflight_err = tensor_map_cuda_callable_op_or_raise(func, is_binary, dtype, &op, interp);
    if (preflight_err != null) return preflight_err;

    if ((left_source_tensor != null && tensor_expr_has_non_cuda_device(left_source)) ||
        (is_binary && right_source_tensor != null && tensor_expr_has_non_cuda_device(right_source))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
    }
    if (!tensor_dtype_is_cuda_real(dtype) && !tensor_dtype_is_fixed_complex(dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if ((left_source_tensor != null && left_source_tensor.dtype != dtype) ||
        (is_binary && right_source_tensor != null && right_source_tensor.dtype != dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: CUDA tensor dtype mismatch", null);
    }

    if (left_source_tensor != null) {
        err = tensor_expr_resolve_concrete_any_device(left_source, &left_tensor, &left_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }
    if (is_binary && right_source_tensor != null) {
        err = tensor_expr_resolve_concrete_any_device(right_source, &right_tensor, &right_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }

    if (!is_binary) {
        if (left_tensor == null || left_tensor.device != TENSOR_DEVICE_CUDA) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA operands must remain CUDA-placed", null);
        }
        if (!tensor_dtype_is_cuda_real(dtype) && !tensor_dtype_is_fixed_complex(dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
        if (!tensor_map_operand_matches_shape(left_tensor, result_shape)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: CUDA result shape mismatch", null);
        }
        if (tensor_dtype_is_fixed_complex(dtype)) {
            return tensor_cuda_map_complex_unary_value(
                left_tensor,
                op,
                result_shape,
                interp,
                "map: failed to allocate CUDA tensor metadata",
                "map: failed to allocate CUDA tensor value",
                "map: CUDA currently supports dense row-major Complex128 and Complex64 unary operations",
                "map: CUDA tensor backing storage is missing",
                "map: CUDA complex unary kernel failed"
            );
        }
        return tensor_cuda_map_unary_value(
            left_tensor,
            op,
            result_shape,
            interp,
            "map: failed to allocate CUDA tensor metadata",
            "map: failed to allocate CUDA tensor value",
            "map: CUDA currently supports dense row-major Float64 and Float32 unary operations",
            "map: CUDA tensor backing storage is missing",
            "map: CUDA unary kernel failed"
        );
    }

    double left_scalar = 0.0;
    double right_scalar = 0.0;
    Complex128Val left_complex128_scalar;
    left_complex128_scalar.real = 0.0;
    left_complex128_scalar.imag = 0.0;
    Complex64Val left_complex64_scalar;
    left_complex64_scalar.real = 0.0f;
    left_complex64_scalar.imag = 0.0f;
    Complex128Val right_complex128_scalar;
    right_complex128_scalar.real = 0.0;
    right_complex128_scalar.imag = 0.0;
    Complex64Val right_complex64_scalar;
    right_complex64_scalar.real = 0.0f;
    right_complex64_scalar.imag = 0.0f;
    if (left_tensor == null) {
        bool scalar_ok = dtype == TENSOR_DTYPE_COMPLEX128
            ? fixed_complex_value_to_complex128(left_source, &left_complex128_scalar)
            : dtype == TENSOR_DTYPE_COMPLEX64
                ? fixed_complex_value_to_complex64(left_source, &left_complex64_scalar)
                : tensor_value_to_double(left_source, &left_scalar);
        if (!scalar_ok) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA scalar operand is not representable by the Tensor dtype", null);
        }
    }
    if (right_tensor == null) {
        bool scalar_ok = dtype == TENSOR_DTYPE_COMPLEX128
            ? fixed_complex_value_to_complex128(right_source, &right_complex128_scalar)
            : dtype == TENSOR_DTYPE_COMPLEX64
                ? fixed_complex_value_to_complex64(right_source, &right_complex64_scalar)
                : tensor_value_to_double(right_source, &right_scalar);
        if (!scalar_ok) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: CUDA scalar operand is not representable by the Tensor dtype", null);
        }
    }
    if (tensor_dtype_is_fixed_complex(dtype)) {
        return tensor_cuda_map_complex_binary_resolved(
            left_tensor,
            left_complex128_scalar,
            left_complex64_scalar,
            right_tensor,
            right_complex128_scalar,
            right_complex64_scalar,
            dtype,
            result_shape,
            op,
            interp,
            "map: failed to allocate CUDA tensor metadata",
            "map: failed to allocate CUDA tensor value",
            "map: CUDA currently supports dense row-major Complex128 and Complex64 binary operations",
            "map: CUDA tensor backing storage is missing",
            "map: CUDA complex kernel failed"
        );
    }
    return tensor_cuda_map_binary_resolved(
        left_tensor,
        left_scalar,
        right_tensor,
        right_scalar,
        dtype,
        result_shape,
        op,
        interp,
        "map: failed to allocate CUDA tensor metadata",
        "map: failed to allocate CUDA tensor value",
        "map: CUDA currently supports dense row-major Float64 and Float32 binary operations",
        "map: CUDA tensor backing storage is missing",
        "map: CUDA kernel failed"
    );
}

fn Value* tensor_map_try_vulkan_direct(
    Value* func,
    Value* left_source,
    TensorVal* left_source_tensor,
    Value* right_source,
    TensorVal* right_source_tensor,
    bool is_binary,
    TensorDType dtype,
    usz[] result_shape,
    Interp* interp
) {
    bool touches_vulkan = (left_source_tensor != null && tensor_expr_has_device(left_source, TENSOR_DEVICE_VULKAN)) ||
        (is_binary && right_source_tensor != null && tensor_expr_has_device(right_source, TENSOR_DEVICE_VULKAN));
    if (!touches_vulkan) return null;

    uint op = 0;
    Value* preflight_err = tensor_map_vulkan_callable_op_or_raise(func, is_binary, dtype, &op, interp);
    if (preflight_err != null) return preflight_err;

    return tensor_map_try_vulkan_direct_checked(
        left_source,
        left_source_tensor,
        right_source,
        right_source_tensor,
        is_binary,
        dtype,
        result_shape,
        op,
        interp
    );
}

fn Value* tensor_map_try_vulkan_direct_checked(
    Value* left_source,
    TensorVal* left_source_tensor,
    Value* right_source,
    TensorVal* right_source_tensor,
    bool is_binary,
    TensorDType dtype,
    usz[] result_shape,
    uint op,
    Interp* interp
) {
    TensorVal* left_tensor = null;
    TensorVal* right_tensor = null;
    Value* left_value = null;
    Value* right_value = null;
    Value* err = null;
    defer if (left_value != null && left_value != left_source) {
        boundary_cleanup_materialized_value(null, left_value, interp.current_scope);
    }
    defer if (right_value != null && right_value != right_source) {
        boundary_cleanup_materialized_value(null, right_value, interp.current_scope);
    }

    if ((left_source_tensor != null && tensor_expr_has_non_vulkan_device(left_source)) ||
        (is_binary && right_source_tensor != null && tensor_expr_has_non_vulkan_device(right_source))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (!tensor_dtype_is_vulkan_real(dtype) && !tensor_dtype_is_fixed_complex(dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if ((left_source_tensor != null && left_source_tensor.dtype != dtype) ||
        (is_binary && right_source_tensor != null && right_source_tensor.dtype != dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports matching Tensor storage dtypes", null);
    }

    if (left_source_tensor != null) {
        err = tensor_expr_resolve_concrete_any_device(left_source, &left_tensor, &left_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }
    if (is_binary && right_source_tensor != null) {
        err = tensor_expr_resolve_concrete_any_device(right_source, &right_tensor, &right_value, interp, "map: tensor backing storage is missing");
        if (err != null) return err;
    }

    if (!is_binary) {
        if (left_tensor == null || left_tensor.device != TENSOR_DEVICE_VULKAN) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
        }
        if (!tensor_dtype_is_vulkan_real(dtype) && !tensor_dtype_is_fixed_complex(dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
        if (!tensor_map_operand_matches_shape(left_tensor, result_shape)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
        }
        if (tensor_dtype_is_fixed_complex(dtype)) {
            return tensor_vulkan_map_complex_unary_value(
                left_tensor,
                op,
                result_shape,
                interp,
                "map: failed to allocate Vulkan tensor metadata",
                "map: failed to allocate Vulkan tensor value",
                "map: Vulkan currently supports dense row-major Complex128 and Complex64 unary operations",
                "map: Vulkan tensor backing storage is missing",
                "map: Vulkan complex unary kernel failed"
            );
        }
        return tensor_vulkan_map_unary_value(
            left_tensor,
            op,
            result_shape,
            interp,
            "map: failed to allocate Vulkan tensor metadata",
            "map: failed to allocate Vulkan tensor value",
            "map: Vulkan currently supports dense row-major Float64 and Float32 unary operations",
            "map: Vulkan tensor backing storage is missing",
            "map: Vulkan unary kernel failed"
        );
    }

    TensorVal* tensor = left_tensor != null ? left_tensor : right_tensor;
    double scalar_f64 = 0.0;
    float scalar_f32 = 0.0f;
    Complex128Val scalar_complex128;
    scalar_complex128.real = 0.0;
    scalar_complex128.imag = 0.0;
    Complex64Val scalar_complex64;
    scalar_complex64.real = 0.0f;
    scalar_complex64.imag = 0.0f;
    uint mode = 2;
    if (left_tensor == null) {
        bool scalar_ok = dtype == TENSOR_DTYPE_FLOAT32
            ? tensor_value_to_float32(left_source, &scalar_f32)
            : dtype == TENSOR_DTYPE_COMPLEX128
                ? fixed_complex_value_to_complex128(left_source, &scalar_complex128)
                : dtype == TENSOR_DTYPE_COMPLEX64
                    ? fixed_complex_value_to_complex64(left_source, &scalar_complex64)
                    : tensor_value_to_double(left_source, &scalar_f64);
        if (!scalar_ok) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan scalar operand is not representable by the Tensor dtype", null);
        }
        mode = 1;
    } else if (right_tensor == null) {
        bool scalar_ok = dtype == TENSOR_DTYPE_FLOAT32
            ? tensor_value_to_float32(right_source, &scalar_f32)
            : dtype == TENSOR_DTYPE_COMPLEX128
                ? fixed_complex_value_to_complex128(right_source, &scalar_complex128)
                : dtype == TENSOR_DTYPE_COMPLEX64
                    ? fixed_complex_value_to_complex64(right_source, &scalar_complex64)
                    : tensor_value_to_double(right_source, &scalar_f64);
        if (!scalar_ok) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan scalar operand is not representable by the Tensor dtype", null);
        }
        mode = 0;
    }

    if (tensor == null || tensor.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (left_tensor != null && left_tensor.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if (right_tensor != null && right_tensor.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
    }
    if ((!tensor_dtype_is_vulkan_real(dtype) && !tensor_dtype_is_fixed_complex(dtype)) || tensor.dtype != dtype ||
        (left_tensor != null && left_tensor.dtype != dtype) ||
        (right_tensor != null && right_tensor.dtype != dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports matching Tensor storage dtypes", null);
    }
    if (!tensor_map_operand_matches_shape(tensor, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (left_tensor != null && !tensor_map_operand_matches_shape(left_tensor, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (right_tensor != null && !tensor_map_operand_matches_shape(right_tensor, result_shape)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "map: Vulkan result shape mismatch", null);
    }
    if (!tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_VULKAN) ||
        (left_tensor != null && !tensor_has_valid_device_storage(left_tensor, TENSOR_DEVICE_VULKAN)) ||
        (right_tensor != null && !tensor_has_valid_device_storage(right_tensor, TENSOR_DEVICE_VULKAN))) {
        return tensor_runtime_raise(interp, "map: Vulkan tensor backing storage is missing");
    }

    TensorVal* result_tensor = tensor_alloc_payload_base_checked(dtype, result_shape, false, false, "map: failed to allocate Vulkan tensor metadata");
    if (result_tensor == null) return tensor_runtime_raise(interp, "map: failed to allocate Vulkan tensor metadata");
    result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    result_tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_map_f64(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_f64,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    } else if (dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_map_f32(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_f32,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    } else if (dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_map_complex128(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_complex128.real,
            scalar_complex128.imag,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    } else if (dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_map_complex64(
            left_tensor != null ? left_tensor.device_handle : null,
            right_tensor != null ? right_tensor.device_handle : null,
            result_tensor.byte_len,
            result_tensor.element_count,
            scalar_complex64.real,
            scalar_complex64.imag,
            mode,
            op,
            result_tensor.rank,
            left_tensor != null ? left_tensor.rank : 0,
            right_tensor != null ? right_tensor.rank : 0,
            result_tensor.shape,
            result_tensor.strides,
            left_tensor != null ? left_tensor.shape : null,
            left_tensor != null ? left_tensor.strides : null,
            right_tensor != null ? right_tensor.shape : null,
            right_tensor != null ? right_tensor.strides : null,
            &out_device
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(result_tensor);
        if (tensor_dtype_is_fixed_complex(dtype) && status == OMNI_TENSOR_VULKAN_DOMAIN_ERROR) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "map: complex division by zero", null);
        }
        if (tensor_dtype_is_fixed_complex(dtype) && status == OMNI_TENSOR_VULKAN_INVALID_ARGUMENT) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "map: complex result is not representable", null);
        }
        return tensor_vulkan_status_raise(interp, status, "map: Vulkan kernel failed");
    }
    result_tensor.device_handle = out_device;
    result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(result_tensor);
        return tensor_runtime_raise(interp, "map: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = result_tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_realize_vulkan_transpose_view_value(Value* source_value, TensorVal* view, Interp* interp) {
    if (source_value == null || view == null || view.kind != TENSOR_PAYLOAD_VIEW || view.device != TENSOR_DEVICE_VULKAN) {
        return tensor_runtime_raise(interp, "realize: Vulkan view metadata is missing");
    }
    if (view.rank != 2 || view.shape == null || view.strides == null || view.view_source == null || !is_tensor(view.view_source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan view materialization currently supports direct rank-2 transpose views", null);
    }
    if (view.storage_offset != 0 || view.storage_element_count < view.element_count) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan view materialization currently requires zero-offset backing storage", null);
    }
    if (view.byte_len > 0 && view.device_handle == null) {
        return tensor_runtime_raise(interp, "realize: Vulkan view backing storage is missing");
    }

    TensorVal* backing = view.view_source.tensor_val;
    if (
        backing == null ||
        backing.kind != TENSOR_PAYLOAD_CONCRETE ||
        backing.device != TENSOR_DEVICE_VULKAN ||
        backing.rank != 2 ||
        backing.shape == null ||
        backing.strides == null ||
        backing.dtype != view.dtype ||
        backing.shape[0] != view.shape[1] ||
        backing.shape[1] != view.shape[0] ||
        backing.strides[0] != view.strides[1] ||
        backing.strides[1] != view.strides[0] ||
        backing.storage_offset != 0 ||
        backing.device_handle != view.device_handle
    ) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan view materialization currently supports direct transpose views over dense concrete sources", null);
    }
    if (!tensor_has_valid_device_storage(backing, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "realize: Vulkan view source backing storage is missing");
    }

    usz[2] result_shape_buf = { view.shape[0], view.shape[1] };
    Value* result = matrix_transpose_try_vulkan_value(backing, result_shape_buf[0:2], interp);
    if (result != null) return result;
    return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: Vulkan view dtype is not supported by transpose materialization", null);
}

fn Value* tensor_realize_to_value(Value* source_value, Interp* interp) {
    if (!is_tensor(source_value)) return tensor_type_raise(interp, "realize: expected Tensor source");
    TensorVal* source = source_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "realize: source tensor metadata is missing");
    if (source.kind == TENSOR_PAYLOAD_CONCRETE) {
        if (source.device == TENSOR_DEVICE_CPU) {
            if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        } else if (!tensor_has_valid_device_storage(source, source.device)) {
            return tensor_runtime_raise(interp, "realize: source tensor backing storage is missing");
        }
        return source_value;
    }
    if (source.kind == TENSOR_PAYLOAD_MAP) {
        Value* vulkan_result = tensor_map_try_vulkan_value(source, interp);
        if (vulkan_result != null) return vulkan_result;
        Value* cuda_result = tensor_map_try_cuda_value(source, interp);
        if (cuda_result != null) return cuda_result;
    }
    if (source.kind == TENSOR_PAYLOAD_CONTRACT) {
        Value* device_result = tensor_contract_try_device_value(source, interp);
        if (device_result != null) return device_result;
    }
    if (source.kind == TENSOR_PAYLOAD_VIEW && source.device == TENSOR_DEVICE_VULKAN) {
        return tensor_realize_vulkan_transpose_view_value(source_value, source, interp);
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

fn bool tensor_expr_has_non_cpu_device(Value* source_value) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return false;

    TensorVal* source = source_value.tensor_val;
    switch (source.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return source.device != TENSOR_DEVICE_CPU;
        case TENSOR_PAYLOAD_MAP:
            if (source.map_left != null && tensor_expr_has_non_cpu_device(source.map_left)) return true;
            if (source.map_is_binary && source.map_right != null && tensor_expr_has_non_cpu_device(source.map_right)) return true;
            return false;
        case TENSOR_PAYLOAD_CONTRACT:
            return tensor_expr_has_non_cpu_device(source.contract_left) || tensor_expr_has_non_cpu_device(source.contract_right);
        case TENSOR_PAYLOAD_VIEW:
            return source.device != TENSOR_DEVICE_CPU || tensor_expr_has_non_cpu_device(source.view_source);
    }
}

fn bool tensor_expr_has_device(Value* source_value, TensorDeviceKind device) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return false;

    TensorVal* source = source_value.tensor_val;
    switch (source.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return source.device == device;
        case TENSOR_PAYLOAD_MAP:
            if (source.map_left != null && tensor_expr_has_device(source.map_left, device)) return true;
            if (source.map_is_binary && source.map_right != null && tensor_expr_has_device(source.map_right, device)) return true;
            return false;
        case TENSOR_PAYLOAD_CONTRACT:
            return tensor_expr_has_device(source.contract_left, device) || tensor_expr_has_device(source.contract_right, device);
        case TENSOR_PAYLOAD_VIEW:
            return source.device == device || tensor_expr_has_device(source.view_source, device);
    }
}

fn bool tensor_expr_has_non_cuda_device(Value* source_value) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return false;

    TensorVal* source = source_value.tensor_val;
    switch (source.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return source.device != TENSOR_DEVICE_CUDA;
        case TENSOR_PAYLOAD_MAP:
            if (source.map_left != null && tensor_expr_has_non_cuda_device(source.map_left)) return true;
            if (source.map_is_binary && source.map_right != null && tensor_expr_has_non_cuda_device(source.map_right)) return true;
            return false;
        case TENSOR_PAYLOAD_CONTRACT:
            return tensor_expr_has_non_cuda_device(source.contract_left) || tensor_expr_has_non_cuda_device(source.contract_right);
        case TENSOR_PAYLOAD_VIEW:
            return source.device != TENSOR_DEVICE_CUDA || tensor_expr_has_non_cuda_device(source.view_source);
    }
}

fn bool tensor_expr_has_non_vulkan_device(Value* source_value) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return false;

    TensorVal* source = source_value.tensor_val;
    switch (source.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return source.device != TENSOR_DEVICE_VULKAN;
        case TENSOR_PAYLOAD_MAP:
            if (source.map_left != null && tensor_expr_has_non_vulkan_device(source.map_left)) return true;
            if (source.map_is_binary && source.map_right != null && tensor_expr_has_non_vulkan_device(source.map_right)) return true;
            return false;
        case TENSOR_PAYLOAD_CONTRACT:
            return tensor_expr_has_non_vulkan_device(source.contract_left) || tensor_expr_has_non_vulkan_device(source.contract_right);
        case TENSOR_PAYLOAD_VIEW:
            return source.device != TENSOR_DEVICE_VULKAN || tensor_expr_has_non_vulkan_device(source.view_source);
    }
}

fn Value* tensor_expr_resolve_concrete_cpu(
    Value* source_value,
    TensorVal** out_tensor,
    Value** out_value,
    Interp* interp,
    char[] storage_label,
    char[] unsupported_label
) {
    if (out_tensor == null) return tensor_runtime_raise(interp, storage_label);
    *out_tensor = null;
    if (out_value != null) *out_value = null;
    if (!is_tensor(source_value)) return tensor_type_raise(interp, storage_label);

    TensorVal* source = source_value.tensor_val;
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, storage_label);
    if (tensor_expr_has_non_cpu_device(source_value)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_label, null);
    }

    Value* realized = source.kind == TENSOR_PAYLOAD_CONCRETE ? source_value : tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    if (!is_tensor(realized) || realized.tensor_val == null) return tensor_runtime_raise(interp, storage_label);

    TensorVal* tensor = realized.tensor_val;
    if (tensor.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_label, null);
    }
    if (!tensor_has_valid_storage(tensor)) {
        return tensor_runtime_raise(interp, storage_label);
    }

    *out_tensor = tensor;
    if (out_value != null) *out_value = realized;
    return null;
}

fn Value* tensor_eval_expr_into(TensorVal* source, TensorVal* dest, Interp* interp) {
    if (!tensor_has_valid_metadata(source)) return tensor_runtime_raise(interp, "realize: source tensor metadata is missing");
    Value* dest_check = tensor_require_cpu_destination(dest, interp);
    if (dest_check != null) return dest_check;
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
        case TENSOR_PAYLOAD_VIEW: {
            if (source.device != TENSOR_DEVICE_CPU || source.byte_len > 0 && source.data == null) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "realize: view materialization currently requires CPU backing storage", null);
            }
            if (!tensor_dense_row_major(dest)) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
            for (usz i = 0; i < source.element_count; i++) {
                usz storage_index = 0;
                if (!tensor_storage_offset_for_logical_index(source, i, &storage_index)) return tensor_runtime_raise(interp, "realize: view index offset is invalid");
                switch (source.dtype) {
                    case TENSOR_DTYPE_DOUBLE:
                        ((double*)dest.data)[i] = ((double*)source.data)[storage_index];
                    case TENSOR_DTYPE_FLOAT32:
                        ((float*)dest.data)[i] = ((float*)source.data)[storage_index];
                    case TENSOR_DTYPE_COMPLEX128:
                        ((Complex128Val*)dest.data)[i] = ((Complex128Val*)source.data)[storage_index];
                    case TENSOR_DTYPE_COMPLEX64:
                        ((Complex64Val*)dest.data)[i] = ((Complex64Val*)source.data)[storage_index];
                    case TENSOR_DTYPE_BIG_INTEGER: {
                        void** source_values = (void**)source.data;
                        void** dest_values = (void**)dest.data;
                        if (source_values[storage_index] == null) return tensor_runtime_raise(interp, "realize: tensor BigInteger element is missing");
                        dest_values[i] = omni_big_integer_clone(source_values[storage_index]);
                        if (dest_values[i] == null) return tensor_runtime_raise(interp, "realize: failed to allocate BigInteger tensor element");
                    }
                    case TENSOR_DTYPE_BIG_FLOAT: {
                        void** source_values = (void**)source.data;
                        void** dest_values = (void**)dest.data;
                        if (source_values[storage_index] == null) return tensor_runtime_raise(interp, "realize: tensor BigFloat element is missing");
                        dest_values[i] = omni_big_float_clone(source_values[storage_index]);
                        if (dest_values[i] == null) return tensor_runtime_raise(interp, "realize: failed to allocate BigFloat tensor element");
                    }
                    case TENSOR_DTYPE_BIG_COMPLEX: {
                        void** source_values = (void**)source.data;
                        void** dest_values = (void**)dest.data;
                        if (source_values[storage_index] == null) return tensor_runtime_raise(interp, "realize: tensor BigComplex element is missing");
                        dest_values[i] = omni_big_complex_clone(source_values[storage_index]);
                        if (dest_values[i] == null) return tensor_runtime_raise(interp, "realize: failed to allocate BigComplex tensor element");
                    }
                }
            }
            return null;
        }
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
                source.map_left_big_complex_scalar,
                source.map_left_complex128_scalar,
                source.map_left_complex64_scalar,
                right_tensor,
                source.map_right_scalar,
                source.map_right_big_integer_scalar,
                source.map_right_big_float_scalar,
                source.map_right_big_complex_scalar,
                source.map_right_complex128_scalar,
                source.map_right_complex64_scalar,
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
    TensorVal* source_tensor = tensor_value.tensor_val;
    Value* concrete_value = source_tensor != null && source_tensor.kind == TENSOR_PAYLOAD_VIEW
        ? tensor_value
        : tensor_realize_to_value(tensor_value, interp);
    if (concrete_value == null || concrete_value.tag == ERROR) return concrete_value;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    TensorVal* tensor = concrete_value.tensor_val;
    if (tensor == null) return tensor_runtime_raise(interp, "ref: tensor backing storage is missing");
    if (!tensor_has_valid_metadata(tensor)) return tensor_runtime_raise(interp, "ref: tensor metadata is missing");
    if (tensor.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ref: non-CPU Tensor requires explicit to-device cpu copy before indexing", null);
    }
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

    if (tensor.storage_offset > tensor.storage_element_count ||
        offset >= tensor.storage_element_count - tensor.storage_offset) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/axis-out-of-range", "ref: tensor index out of bounds", null);
    }
    usz storage_offset = tensor.storage_offset + offset;
    switch (tensor.dtype) {
        case TENSOR_DTYPE_DOUBLE:
            return make_double(interp, ((double*)tensor.data)[storage_offset]);
        case TENSOR_DTYPE_FLOAT32:
            return make_float32(interp, ((float*)tensor.data)[storage_offset]);
        case TENSOR_DTYPE_COMPLEX128:
            return make_complex128(interp, ((Complex128Val*)tensor.data)[storage_offset].real, ((Complex128Val*)tensor.data)[storage_offset].imag);
        case TENSOR_DTYPE_COMPLEX64:
            return make_complex64(interp, ((Complex64Val*)tensor.data)[storage_offset].real, ((Complex64Val*)tensor.data)[storage_offset].imag);
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** values = (void**)tensor.data;
            if (values[storage_offset] == null) return tensor_runtime_raise(interp, "ref: tensor BigInteger element is missing");
            void* cloned = omni_big_integer_clone(values[storage_offset]);
            return make_big_integer_from_owned_handle(interp, cloned, "ref: failed to allocate BigInteger tensor element");
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** values = (void**)tensor.data;
            if (values[storage_offset] == null) return tensor_runtime_raise(interp, "ref: tensor BigFloat element is missing");
            void* cloned = omni_big_float_clone(values[storage_offset]);
            return make_big_float_from_owned_handle(interp, cloned, "ref: failed to allocate BigFloat tensor element");
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** values = (void**)tensor.data;
            if (values[storage_offset] == null) return tensor_runtime_raise(interp, "ref: tensor BigComplex element is missing");
            void* cloned = omni_big_complex_clone(values[storage_offset]);
            return make_big_complex_from_owned_handle(interp, cloned, "ref: failed to allocate BigComplex tensor element");
        }
    }
}

fn bool tensor_storage_offset_for_logical_index(TensorVal* tensor, usz logical_index, usz* out_offset) {
    if (tensor == null || out_offset == null || !tensor_has_valid_metadata(tensor)) return false;
    if (logical_index >= tensor.element_count) return false;
    usz storage_offset = tensor.storage_offset;
    if (tensor.rank == 0) {
        *out_offset = storage_offset;
        return true;
    }
    if (tensor.shape == null || tensor.strides == null) return false;
    usz remaining = logical_index;
    for (usz i = tensor.rank; i > 0; i--) {
        usz axis = i - 1;
        usz dim = tensor.shape[axis];
        if (dim == 0) return false;
        usz coord = remaining % dim;
        remaining = remaining / dim;
        usz axis_offset = 0;
        if (!tensor_checked_mul_usz(coord, tensor.strides[axis], &axis_offset)) return false;
        if (axis_offset > usz.max - storage_offset) return false;
        storage_offset += axis_offset;
    }
    if (storage_offset >= tensor.storage_element_count) return false;
    *out_offset = storage_offset;
    return true;
}

fn Value* tensor_to_flat_array_value(Value* tensor_value, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "Array: expected Tensor");
    TensorVal* source_tensor = tensor_value.tensor_val;
    Value* concrete_value = source_tensor != null && source_tensor.kind == TENSOR_PAYLOAD_VIEW
        ? tensor_value
        : tensor_realize_to_value(tensor_value, interp);
    if (concrete_value == null || concrete_value.tag == ERROR) return concrete_value;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    TensorVal* tensor = concrete_value.tensor_val;
    if (!tensor_has_valid_metadata(tensor) || tensor.device != TENSOR_DEVICE_CPU || (tensor.byte_len > 0 && tensor.data == null)) return tensor_runtime_raise(interp, "Array: tensor backing storage is missing");
    Value* result = make_array_checked(interp, tensor.element_count, "Array: failed to allocate tensor conversion result");
    if (result == null || result.tag == ERROR) return result;
    if (result.tag != ARRAY || result.array_val == null) {
        return tensor_runtime_raise(interp, "Array: result array backing storage is missing");
    }

    for (usz i = 0; i < tensor.element_count; i++) {
        usz storage_index = 0;
        if (!tensor_storage_offset_for_logical_index(tensor, i, &storage_index)) return tensor_runtime_raise(interp, "Array: tensor index offset is invalid");
        Value* elem = null;
        switch (tensor.dtype) {
            case TENSOR_DTYPE_DOUBLE:
                elem = make_double(interp, ((double*)tensor.data)[storage_index]);
            case TENSOR_DTYPE_FLOAT32:
                elem = make_float32(interp, ((float*)tensor.data)[storage_index]);
            case TENSOR_DTYPE_COMPLEX128:
                elem = make_complex128(interp, ((Complex128Val*)tensor.data)[storage_index].real, ((Complex128Val*)tensor.data)[storage_index].imag);
            case TENSOR_DTYPE_COMPLEX64:
                elem = make_complex64(interp, ((Complex64Val*)tensor.data)[storage_index].real, ((Complex64Val*)tensor.data)[storage_index].imag);
            case TENSOR_DTYPE_BIG_INTEGER: {
                void** values = (void**)tensor.data;
                if (values[storage_index] == null) return tensor_runtime_raise(interp, "Array: tensor BigInteger element is missing");
                elem = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(values[storage_index]), "Array: failed to allocate BigInteger tensor element");
            }
            case TENSOR_DTYPE_BIG_FLOAT: {
                void** values = (void**)tensor.data;
                if (values[storage_index] == null) return tensor_runtime_raise(interp, "Array: tensor BigFloat element is missing");
                elem = make_big_float_from_owned_handle(interp, omni_big_float_clone(values[storage_index]), "Array: failed to allocate BigFloat tensor element");
            }
            case TENSOR_DTYPE_BIG_COMPLEX: {
                void** values = (void**)tensor.data;
                if (values[storage_index] == null) return tensor_runtime_raise(interp, "Array: tensor BigComplex element is missing");
                elem = make_big_complex_from_owned_handle(interp, omni_big_complex_clone(values[storage_index]), "Array: failed to allocate BigComplex tensor element");
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
    TensorVal* source_tensor = tensor_value.tensor_val;
    Value* concrete_value = source_tensor != null && source_tensor.kind == TENSOR_PAYLOAD_VIEW
        ? tensor_value
        : tensor_realize_to_value(tensor_value, interp);
    if (concrete_value == null || concrete_value.tag == ERROR) return concrete_value;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    TensorVal* tensor = concrete_value.tensor_val;
    if (!tensor_has_valid_metadata(tensor) || tensor.device != TENSOR_DEVICE_CPU || (tensor.byte_len > 0 && tensor.data == null)) return tensor_runtime_raise(interp, "List: tensor backing storage is missing");
    Value* result = make_nil(interp);
    usz remaining = tensor.element_count;
    while (remaining > 0) {
        remaining--;
        usz storage_index = 0;
        if (!tensor_storage_offset_for_logical_index(tensor, remaining, &storage_index)) return tensor_runtime_raise(interp, "List: tensor index offset is invalid");
        Value* elem = null;
        switch (tensor.dtype) {
            case TENSOR_DTYPE_DOUBLE:
                elem = make_double(interp, ((double*)tensor.data)[storage_index]);
            case TENSOR_DTYPE_FLOAT32:
                elem = make_float32(interp, ((float*)tensor.data)[storage_index]);
            case TENSOR_DTYPE_COMPLEX128:
                elem = make_complex128(interp, ((Complex128Val*)tensor.data)[storage_index].real, ((Complex128Val*)tensor.data)[storage_index].imag);
            case TENSOR_DTYPE_COMPLEX64:
                elem = make_complex64(interp, ((Complex64Val*)tensor.data)[storage_index].real, ((Complex64Val*)tensor.data)[storage_index].imag);
            case TENSOR_DTYPE_BIG_INTEGER: {
                void** values = (void**)tensor.data;
                if (values[storage_index] == null) return tensor_runtime_raise(interp, "List: tensor BigInteger element is missing");
                elem = make_big_integer_from_owned_handle(interp, omni_big_integer_clone(values[storage_index]), "List: failed to allocate BigInteger tensor element");
            }
            case TENSOR_DTYPE_BIG_FLOAT: {
                void** values = (void**)tensor.data;
                if (values[storage_index] == null) return tensor_runtime_raise(interp, "List: tensor BigFloat element is missing");
                elem = make_big_float_from_owned_handle(interp, omni_big_float_clone(values[storage_index]), "List: failed to allocate BigFloat tensor element");
            }
            case TENSOR_DTYPE_BIG_COMPLEX: {
                void** values = (void**)tensor.data;
                if (values[storage_index] == null) return tensor_runtime_raise(interp, "List: tensor BigComplex element is missing");
                elem = make_big_complex_from_owned_handle(interp, omni_big_complex_clone(values[storage_index]), "List: failed to allocate BigComplex tensor element");
            }
        }
        if (elem == null || elem.tag == ERROR) return elem;
        result = make_cons_or_error(interp, elem, result, "List: failed to allocate tensor conversion result");
        if (result == null || result.tag == ERROR) return result;
    }
    return result;
}

fn Value* tensor_big_complex_component_value(Value* tensor_value, bool imaginary, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, imaginary ? "imag-part: expected Tensor" : "real-part: expected Tensor");
    TensorVal* source = null;
    Value* concrete_value = null;
    Value* resolve_err = tensor_expr_resolve_concrete_any_device(
        tensor_value,
        &source,
        &concrete_value,
        interp,
        imaginary ? "imag-part: tensor backing storage is missing" : "real-part: tensor backing storage is missing"
    );
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            TensorDType result_dtype = source.dtype == TENSOR_DTYPE_COMPLEX128 ? TENSOR_DTYPE_DOUBLE : TENSOR_DTYPE_FLOAT32;
            return tensor_vulkan_map_complex_to_real_value(
                source,
                imaginary ? 4 : 3,
                result_dtype,
                shape_slice,
                interp,
                imaginary ? "imag-part: failed to allocate Vulkan tensor metadata" : "real-part: failed to allocate Vulkan tensor metadata",
                imaginary ? "imag-part: failed to allocate Vulkan tensor value" : "real-part: failed to allocate Vulkan tensor value",
                imaginary ? "imag-part: Vulkan currently supports dense row-major Complex128 and Complex64 tensors" : "real-part: Vulkan currently supports dense row-major Complex128 and Complex64 tensors",
                imaginary ? "imag-part: Vulkan tensor backing storage is missing" : "real-part: Vulkan tensor backing storage is missing",
                imaginary ? "imag-part: Vulkan complex component kernel failed" : "real-part: Vulkan complex component kernel failed"
            );
        }
        return tensor_vulkan_map_unary_value(
            source,
            imaginary ? 4 : 3,
            shape_slice,
            interp,
            imaginary ? "imag-part: failed to allocate Vulkan tensor metadata" : "real-part: failed to allocate Vulkan tensor metadata",
            imaginary ? "imag-part: failed to allocate Vulkan tensor value" : "real-part: failed to allocate Vulkan tensor value",
            imaginary ? "imag-part: Vulkan currently supports dense row-major Float64 and Float32 tensors" : "real-part: Vulkan currently supports dense row-major Float64 and Float32 tensors",
            imaginary ? "imag-part: Vulkan tensor backing storage is missing" : "real-part: Vulkan tensor backing storage is missing",
            imaginary ? "imag-part: Vulkan unary kernel failed" : "real-part: Vulkan unary kernel failed"
        );
    }
    if (source.device == TENSOR_DEVICE_CUDA) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            TensorDType result_dtype = source.dtype == TENSOR_DTYPE_COMPLEX128 ? TENSOR_DTYPE_DOUBLE : TENSOR_DTYPE_FLOAT32;
            return tensor_cuda_map_complex_to_real_value(
                source,
                imaginary ? 4 : 3,
                result_dtype,
                shape_slice,
                interp,
                imaginary ? "imag-part: failed to allocate CUDA tensor metadata" : "real-part: failed to allocate CUDA tensor metadata",
                imaginary ? "imag-part: failed to allocate CUDA tensor value" : "real-part: failed to allocate CUDA tensor value",
                imaginary ? "imag-part: CUDA currently supports dense row-major Complex128 and Complex64 tensors" : "real-part: CUDA currently supports dense row-major Complex128 and Complex64 tensors",
                imaginary ? "imag-part: CUDA tensor backing storage is missing" : "real-part: CUDA tensor backing storage is missing",
                imaginary ? "imag-part: CUDA complex component kernel failed" : "real-part: CUDA complex component kernel failed"
            );
        }
        return tensor_cuda_map_unary_value(
            source,
            imaginary ? 4 : 3,
            shape_slice,
            interp,
            imaginary ? "imag-part: failed to allocate CUDA tensor metadata" : "real-part: failed to allocate CUDA tensor metadata",
            imaginary ? "imag-part: failed to allocate CUDA tensor value" : "real-part: failed to allocate CUDA tensor value",
            imaginary ? "imag-part: CUDA currently supports dense row-major Float64 and Float32 tensors" : "real-part: CUDA currently supports dense row-major Float64 and Float32 tensors",
            imaginary ? "imag-part: CUDA tensor backing storage is missing" : "real-part: CUDA tensor backing storage is missing",
            imaginary ? "imag-part: CUDA unary kernel failed" : "real-part: CUDA unary kernel failed"
        );
    }

    if (!tensor_has_valid_storage(source)) {
        return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor backing storage is missing" : "real-part: tensor backing storage is missing");
    }

    if (source.dtype == TENSOR_DTYPE_COMPLEX128) {
        Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_DOUBLE, shape_slice, imaginary ? "imag-part: failed to allocate tensor result" : "real-part: failed to allocate tensor result");
        if (out_value == null || out_value.tag == ERROR) return out_value;
        bool keep_out = false;
        defer if (!keep_out) {
            boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
        }
        if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
            return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor result backing storage is missing" : "real-part: tensor result backing storage is missing");
        }
        Complex128Val* src_values = (Complex128Val*)source.data;
        double* dst_values = (double*)out_value.tensor_val.data;
        for (usz i = 0; i < source.element_count; i++) {
            dst_values[i] = imaginary ? src_values[i].imag : src_values[i].real;
        }
        keep_out = true;
        return out_value;
    }
    if (source.dtype == TENSOR_DTYPE_COMPLEX64) {
        Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, shape_slice, imaginary ? "imag-part: failed to allocate tensor result" : "real-part: failed to allocate tensor result");
        if (out_value == null || out_value.tag == ERROR) return out_value;
        bool keep_out = false;
        defer if (!keep_out) {
            boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
        }
        if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
            return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor result backing storage is missing" : "real-part: tensor result backing storage is missing");
        }
        Complex64Val* src_values = (Complex64Val*)source.data;
        float* dst_values = (float*)out_value.tensor_val.data;
        for (usz i = 0; i < source.element_count; i++) {
            dst_values[i] = imaginary ? src_values[i].imag : src_values[i].real;
        }
        keep_out = true;
        return out_value;
    }

    if (source.dtype != TENSOR_DTYPE_BIG_COMPLEX) {
        Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, imaginary ? "imag-part: failed to allocate tensor result" : "real-part: failed to allocate tensor result");
        if (out_value == null || out_value.tag == ERROR) return out_value;
        bool keep_out = false;
        defer if (!keep_out) {
            boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
        }
        if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
            return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor result backing storage is missing" : "real-part: tensor result backing storage is missing");
        }
        if (imaginary) {
            Value* zero = make_int(interp, 0);
            Value* err = tensor_fill_from_data_arg(out_value.tensor_val, zero, interp);
            if (err != null) return err;
        } else if (!tensor_copy_concrete_storage(out_value.tensor_val, source)) {
            return tensor_runtime_raise(interp, "real-part: failed to copy tensor storage");
        }
        keep_out = true;
        return out_value;
    }

    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_BIG_FLOAT, shape_slice, imaginary ? "imag-part: failed to allocate tensor result" : "real-part: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor result backing storage is missing" : "real-part: tensor result backing storage is missing");
    }

    void** src_values = (void**)source.data;
    void** dst_values = (void**)out_value.tensor_val.data;
    for (usz i = 0; i < source.element_count; i++) {
        if (src_values[i] == null) {
            return tensor_runtime_raise(interp, imaginary ? "imag-part: tensor BigComplex element is missing" : "real-part: tensor BigComplex element is missing");
        }
        void* component = imaginary ? omni_big_complex_imag_part(src_values[i]) : omni_big_complex_real_part(src_values[i]);
        if (component == null) {
            return raise_error(interp, imaginary ? "imag-part: failed to extract tensor component" : "real-part: failed to extract tensor component");
        }
        if (dst_values[i] != null) omni_big_float_free(dst_values[i]);
        dst_values[i] = component;
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_abs_value(Value* tensor_value, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "abs: expected Tensor");
    TensorVal* source = null;
    Value* concrete_value = null;
    Value* resolve_err = tensor_expr_resolve_concrete_any_device(tensor_value, &source, &concrete_value, interp, "abs: tensor backing storage is missing");
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            TensorDType result_dtype = source.dtype == TENSOR_DTYPE_COMPLEX128 ? TENSOR_DTYPE_DOUBLE : TENSOR_DTYPE_FLOAT32;
            return tensor_vulkan_map_complex_to_real_value(
                source,
                0,
                result_dtype,
                shape_slice,
                interp,
                "abs: failed to allocate Vulkan tensor metadata",
                "abs: failed to allocate Vulkan tensor value",
                "abs: Vulkan currently supports dense row-major Complex128 and Complex64 tensors",
                "abs: Vulkan tensor backing storage is missing",
                "abs: Vulkan complex magnitude kernel failed"
            );
        }
        return tensor_vulkan_map_unary_value(
            source,
            0,
            shape_slice,
            interp,
            "abs: failed to allocate Vulkan tensor metadata",
            "abs: failed to allocate Vulkan tensor value",
            "abs: Vulkan currently supports dense row-major Float64 and Float32 tensors",
            "abs: Vulkan tensor backing storage is missing",
            "abs: Vulkan unary kernel failed"
        );
    }
    if (source.device == TENSOR_DEVICE_CUDA) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            TensorDType result_dtype = source.dtype == TENSOR_DTYPE_COMPLEX128 ? TENSOR_DTYPE_DOUBLE : TENSOR_DTYPE_FLOAT32;
            return tensor_cuda_map_complex_to_real_value(
                source,
                0,
                result_dtype,
                shape_slice,
                interp,
                "abs: failed to allocate CUDA tensor metadata",
                "abs: failed to allocate CUDA tensor value",
                "abs: CUDA currently supports dense row-major Complex128 and Complex64 tensors",
                "abs: CUDA tensor backing storage is missing",
                "abs: CUDA complex magnitude kernel failed"
            );
        }
        return tensor_cuda_map_unary_value(
            source,
            0,
            shape_slice,
            interp,
            "abs: failed to allocate CUDA tensor metadata",
            "abs: failed to allocate CUDA tensor value",
            "abs: CUDA currently supports dense row-major Float64 and Float32 tensors",
            "abs: CUDA tensor backing storage is missing",
            "abs: CUDA unary kernel failed"
        );
    }

    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "abs: tensor backing storage is missing");
    TensorDType result_dtype = source.dtype == TENSOR_DTYPE_BIG_COMPLEX ? TENSOR_DTYPE_BIG_FLOAT
        : source.dtype == TENSOR_DTYPE_COMPLEX128 ? TENSOR_DTYPE_DOUBLE
        : source.dtype == TENSOR_DTYPE_COMPLEX64 ? TENSOR_DTYPE_FLOAT32
        : source.dtype;

    Value* out_value = make_tensor_uninitialized_checked(interp, result_dtype, shape_slice, "abs: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "abs: tensor result backing storage is missing");
    }

    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src_values = (double*)source.data;
            double* dst_values = (double*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = tensor_c_fabs(src_values[i]);
            }
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src_values = (float*)source.data;
            float* dst_values = (float*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = (float)tensor_c_fabs((double)src_values[i]);
            }
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val* src_values = (Complex128Val*)source.data;
            double* dst_values = (double*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = complex128_abs_value(src_values[i]);
            }
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val* src_values = (Complex64Val*)source.data;
            float* dst_values = (float*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = complex64_abs_value(src_values[i]);
            }
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "abs: tensor BigInteger element is missing");
                void* result = omni_big_integer_compare_i64(src_values[i], 0) < 0
                    ? omni_big_integer_neg(src_values[i])
                    : omni_big_integer_clone(src_values[i]);
                if (result == null) return raise_error(interp, "abs: failed to allocate BigInteger tensor element");
                if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "abs: tensor BigFloat element is missing");
                void* result = omni_big_float_abs(src_values[i]);
                if (result == null) return raise_error(interp, "abs: failed to allocate BigFloat tensor element");
                if (dst_values[i] != null) omni_big_float_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "abs: tensor BigComplex element is missing");
                void* result = omni_big_complex_abs(src_values[i]);
                if (result == null) return raise_error(interp, "abs: failed to allocate BigComplex tensor magnitude");
                if (dst_values[i] != null) omni_big_float_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_neg_value(Value* tensor_value, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "-: expected Tensor");
    TensorVal* source = null;
    Value* concrete_value = null;
    Value* resolve_err = tensor_expr_resolve_concrete_any_device(tensor_value, &source, &concrete_value, interp, "-: tensor backing storage is missing");
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            return tensor_vulkan_map_complex_unary_value(
                source,
                1,
                shape_slice,
                interp,
                "-: failed to allocate Vulkan tensor metadata",
                "-: failed to allocate Vulkan tensor value",
                "-: Vulkan currently supports dense row-major Complex128 and Complex64 tensors",
                "-: Vulkan tensor backing storage is missing",
                "-: Vulkan complex unary kernel failed"
            );
        }
        return tensor_vulkan_map_unary_value(
            source,
            1,
            shape_slice,
            interp,
            "-: failed to allocate Vulkan tensor metadata",
            "-: failed to allocate Vulkan tensor value",
            "-: Vulkan currently supports dense row-major Float64 and Float32 tensors",
            "-: Vulkan tensor backing storage is missing",
            "-: Vulkan unary kernel failed"
        );
    }
    if (source.device == TENSOR_DEVICE_CUDA) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            return tensor_cuda_map_complex_unary_value(
                source,
                1,
                shape_slice,
                interp,
                "-: failed to allocate CUDA tensor metadata",
                "-: failed to allocate CUDA tensor value",
                "-: CUDA currently supports dense row-major Complex128 and Complex64 tensors",
                "-: CUDA tensor backing storage is missing",
                "-: CUDA complex unary kernel failed"
            );
        }
        return tensor_cuda_map_unary_value(
            source,
            1,
            shape_slice,
            interp,
            "-: failed to allocate CUDA tensor metadata",
            "-: failed to allocate CUDA tensor value",
            "-: CUDA currently supports dense row-major Float64 and Float32 tensors",
            "-: CUDA tensor backing storage is missing",
            "-: CUDA unary kernel failed"
        );
    }

    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "-: tensor backing storage is missing");
    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "-: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "-: tensor result backing storage is missing");
    }

    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src_values = (double*)source.data;
            double* dst_values = (double*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = -src_values[i];
            }
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src_values = (float*)source.data;
            float* dst_values = (float*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i] = -src_values[i];
            }
        }
        case TENSOR_DTYPE_COMPLEX128: {
            Complex128Val* src_values = (Complex128Val*)source.data;
            Complex128Val* dst_values = (Complex128Val*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i].real = -src_values[i].real;
                dst_values[i].imag = -src_values[i].imag;
            }
        }
        case TENSOR_DTYPE_COMPLEX64: {
            Complex64Val* src_values = (Complex64Val*)source.data;
            Complex64Val* dst_values = (Complex64Val*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                dst_values[i].real = -src_values[i].real;
                dst_values[i].imag = -src_values[i].imag;
            }
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "-: tensor BigInteger element is missing");
                void* result = omni_big_integer_neg(src_values[i]);
                if (result == null) return raise_error(interp, "-: failed to allocate BigInteger tensor element");
                if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "-: tensor BigFloat element is missing");
                void* result = omni_big_float_neg(src_values[i]);
                if (result == null) return raise_error(interp, "-: failed to allocate BigFloat tensor element");
                if (dst_values[i] != null) omni_big_float_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "-: tensor BigComplex element is missing");
                void* result = omni_big_complex_neg(src_values[i]);
                if (result == null) return raise_error(interp, "-: failed to allocate BigComplex tensor element");
                if (dst_values[i] != null) omni_big_complex_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
    }

    keep_out = true;
    return out_value;
}

fn double tensor_apply_double_unary_math(double input, int big_float_op) {
    switch (big_float_op) {
        case BIG_FLOAT_MATH_SIN:
            return tensor_c_sin(input);
        case BIG_FLOAT_MATH_COS:
            return tensor_c_cos(input);
        case BIG_FLOAT_MATH_TAN:
            return tensor_c_tan(input);
        case BIG_FLOAT_MATH_ASIN:
            return tensor_c_asin(input);
        case BIG_FLOAT_MATH_ACOS:
            return tensor_c_acos(input);
        case BIG_FLOAT_MATH_ATAN:
            return tensor_c_atan(input);
        case BIG_FLOAT_MATH_SINH:
            return tensor_c_sinh(input);
        case BIG_FLOAT_MATH_COSH:
            return tensor_c_cosh(input);
        case BIG_FLOAT_MATH_TANH:
            return tensor_c_tanh(input);
        case BIG_FLOAT_MATH_EXP:
            return tensor_c_exp(input);
        case BIG_FLOAT_MATH_LOG:
        return tensor_c_log(input);
        case BIG_FLOAT_MATH_LOG10:
            return tensor_c_log10(input);
        case BIG_FLOAT_MATH_ERF:
            return tensor_c_erf(input);
        case BIG_FLOAT_MATH_ERFC:
            return tensor_c_erfc(input);
        case BIG_FLOAT_MATH_NORMAL_CDF:
            return 0.5 * tensor_c_erfc(-input * TENSOR_INV_SQRT_2);
        case BIG_FLOAT_MATH_SQRT:
            return tensor_c_sqrt(input);
        default:
            return input;
    }
}

fn Value* tensor_apply_double_unary_math_checked(double input, int big_float_op, double* out, Interp* interp) {
    if (out == null) return tensor_runtime_raise(interp, "math: tensor result storage is missing");
    if (big_float_op == BIG_FLOAT_MATH_NORMAL_QUANTILE) {
        double output = 0.0;
        int status = omni_boost_math_standard_normal_quantile(input, &output);
        switch (status) {
            case BOOST_MATH_STATUS_OK:
                *out = output;
                return null;
            case BOOST_MATH_STATUS_INVALID_ARGUMENT:
                return raise_error(interp, "stats/normal-quantile: expected finite numeric input");
            case BOOST_MATH_STATUS_DOMAIN_ERROR:
                return raise_error(interp, "stats/normal-quantile: probability must be between 0 and 1");
            case BOOST_MATH_STATUS_RANGE_ERROR:
                return raise_error(interp, "stats/normal-quantile: range error");
            default:
                return raise_error(interp, "stats/normal-quantile: evaluation error");
        }
    }
    *out = tensor_apply_double_unary_math(input, big_float_op);
    return null;
}

fn Value* tensor_unary_math_value(Value* tensor_value, int big_float_op, int big_complex_op, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "math: expected Tensor");
    TensorVal* source = null;
    Value* concrete_value = null;
    Value* resolve_err = tensor_expr_resolve_concrete_any_device(tensor_value, &source, &concrete_value, interp, "math: tensor backing storage is missing");
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        uint vulkan_op = 0;
        if (!tensor_unary_math_vulkan_op(big_float_op, &vulkan_op)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "math: Vulkan currently supports dense row-major Float64 and Float32 unary operations", null);
        }
        return tensor_vulkan_map_unary_value(
            source,
            vulkan_op,
            shape_slice,
            interp,
            "math: failed to allocate Vulkan tensor metadata",
            "math: failed to allocate Vulkan tensor value",
            "math: Vulkan currently supports dense row-major Float64 and Float32 unary operations",
            "math: Vulkan tensor backing storage is missing",
            "math: Vulkan unary kernel failed"
        );
    }
    if (source.device == TENSOR_DEVICE_CUDA) {
        uint cuda_op = 0;
        if (!tensor_unary_math_cuda_dispatch_op(big_float_op, &cuda_op)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "math: CUDA currently supports dense row-major Float64 and Float32 arithmetic and scientific unary operations", null);
        }
        return tensor_cuda_map_unary_value(
            source,
            cuda_op,
            shape_slice,
            interp,
            "math: failed to allocate CUDA tensor metadata",
            "math: failed to allocate CUDA tensor value",
            "math: CUDA currently supports dense row-major Float64 and Float32 arithmetic and scientific unary operations",
            "math: CUDA tensor backing storage is missing",
            "math: CUDA unary kernel failed"
        );
    }

    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "math: tensor backing storage is missing");
    TensorDType result_dtype = source.dtype == TENSOR_DTYPE_BIG_INTEGER ? TENSOR_DTYPE_DOUBLE : source.dtype;

    Value* out_value = make_tensor_uninitialized_checked(interp, result_dtype, shape_slice, "math: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "math: tensor result backing storage is missing");
    }

    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src_values = (double*)source.data;
            double* dst_values = (double*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                double result = 0.0;
                Value* err = tensor_apply_double_unary_math_checked(src_values[i], big_float_op, &result, interp);
                if (err != null) return err;
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src_values = (float*)source.data;
            float* dst_values = (float*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                double result = 0.0;
                Value* err = tensor_apply_double_unary_math_checked((double)src_values[i], big_float_op, &result, interp);
                if (err != null) return err;
                float narrowed = 0.0f;
                if (!tensor_double_to_float32(result, &narrowed)) {
                    return raise_error(interp, "math: tensor Float32 result out of supported range");
                }
                dst_values[i] = narrowed;
            }
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src_values = (void**)source.data;
            double* dst_values = (double*)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "math: tensor BigInteger element is missing");
                double input = 0.0;
                if (omni_big_integer_to_double(src_values[i], &input) == 0) {
                    return raise_error(interp, "math: BigInteger tensor element out of Float64 range");
                }
                double result = 0.0;
                Value* err = tensor_apply_double_unary_math_checked(input, big_float_op, &result, interp);
                if (err != null) return err;
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "math: tensor BigFloat element is missing");
                void* result = omni_big_float_unary_math(src_values[i], big_float_op);
                if (result == null && big_float_op == BIG_FLOAT_MATH_NORMAL_QUANTILE) {
                    return raise_error(interp, "stats/normal-quantile: probability must be between 0 and 1");
                }
                if (result == null) return raise_error(interp, "math: failed to evaluate BigFloat tensor element");
                if (dst_values[i] != null) omni_big_float_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            if (!tensor_unary_math_has_complex_op(big_complex_op)) {
                return raise_error(interp, "math: tensor dtype is not supported for this operation");
            }
            void** src_values = (void**)source.data;
            void** dst_values = (void**)out_value.tensor_val.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "math: tensor BigComplex element is missing");
                void* result = omni_big_complex_unary_math(src_values[i], big_complex_op);
                if (result == null) return raise_error(interp, "math: failed to evaluate BigComplex tensor element");
                if (dst_values[i] != null) omni_big_complex_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_COMPLEX128:
        case TENSOR_DTYPE_COMPLEX64:
            return raise_error(interp, "math: fixed-width complex Tensor scientific operations are not supported");
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_sqrt_value(Value* tensor_value, Interp* interp) {
    return tensor_unary_math_value(tensor_value, BIG_FLOAT_MATH_SQRT, BIG_COMPLEX_MATH_SQRT, interp);
}

fn double tensor_apply_double_rounding(double input, int op) {
    switch (op) {
        case BIG_FLOAT_ROUND_FLOOR:
            return tensor_c_floor(input);
        case BIG_FLOAT_ROUND_CEILING:
            return tensor_c_ceil(input);
        case BIG_FLOAT_ROUND_ROUND:
            return tensor_c_round(input);
        case BIG_FLOAT_ROUND_TRUNCATE:
            return input;
        default:
            return input;
    }
}

fn Value* tensor_store_integer_value(void** out_values, usz index, Value* integer_value, Interp* interp, char[] message) {
    void* handle = null;
    if (integer_value == null) return raise_error(interp, message);
    if (integer_value.tag == INT) {
        handle = omni_big_integer_from_i64(integer_value.int_val);
    } else if (integer_value.tag == BIG_INTEGER) {
        handle = omni_big_integer_clone(integer_value.big_integer_val);
    } else {
        return raise_error(interp, message);
    }
    if (handle == null) return raise_error(interp, message);
    if (out_values[index] != null) omni_big_integer_free(out_values[index]);
    out_values[index] = handle;
    return null;
}

fn Value* tensor_cuda_rounding_to_cpu_big_integer_value(TensorVal* source, int op, char[] range_message, Interp* interp) {
    if (source == null || source.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: expected CUDA Tensor source", null);
    }
    if (!tensor_dtype_is_cuda_real(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: CUDA supports Float64 and Float32 Tensor rounding", null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_CUDA)) {
        return tensor_runtime_raise(interp, "rounding: CUDA tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: CUDA rounding requires dense row-major Tensor storage", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_BIG_INTEGER, shape_slice, "rounding: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "rounding: tensor result backing storage is missing");
    }

    long* rounded_values = null;
    if (source.element_count > 0) {
        if (source.element_count > usz.max / long.sizeof) {
            return raise_error(interp, "rounding: tensor integer result storage is too large");
        }
        rounded_values = (long*)mem::malloc(long.sizeof * source.element_count);
        if (rounded_values == null) return raise_error(interp, "rounding: failed to allocate CUDA integer result buffer");
    }
    defer if (rounded_values != null) mem::free(rounded_values);

    uint cuda_op = (uint)op;
    int status = OMNI_TENSOR_CUDA_INVALID;
    if (source.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_cuda_round_i64_f64(
            source.device_handle,
            source.byte_len,
            source.element_count,
            cuda_op,
            rounded_values
        );
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_cuda_round_i64_f32(
            source.device_handle,
            source.byte_len,
            source.element_count,
            cuda_op,
            rounded_values
        );
    }
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        if (status == OMNI_TENSOR_CUDA_DOMAIN_ERROR) return raise_error(interp, range_message);
        if (status == OMNI_TENSOR_CUDA_INVALID_ARGUMENT) return raise_error(interp, "rounding: invalid CUDA rounding operation");
        return tensor_cuda_status_raise(interp, status, "rounding: CUDA rounding kernel failed");
    }

    void** dst_values = (void**)out_value.tensor_val.data;
    for (usz i = 0; i < source.element_count; i++) {
        void* result = omni_big_integer_from_i64(rounded_values[i]);
        if (result == null) return raise_error(interp, "rounding: failed to allocate BigInteger tensor element");
        if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
        dst_values[i] = result;
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_vulkan_rounding_to_cpu_big_integer_value(TensorVal* source, int op, char[] range_message, Interp* interp) {
    if (source == null || source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: expected Vulkan Tensor source", null);
    }
    if (!tensor_dtype_is_vulkan_real(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: Vulkan supports Float64 and Float32 Tensor rounding", null);
    }
    if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "rounding: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: Vulkan rounding requires dense row-major Tensor storage", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_BIG_INTEGER, shape_slice, "rounding: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "rounding: tensor result backing storage is missing");
    }

    long* rounded_values = null;
    if (source.element_count > 0) {
        if (source.element_count > usz.max / long.sizeof) {
            return raise_error(interp, "rounding: tensor integer result storage is too large");
        }
        rounded_values = (long*)mem::malloc(long.sizeof * source.element_count);
        if (rounded_values == null) return raise_error(interp, "rounding: failed to allocate Vulkan integer result buffer");
    }
    defer if (rounded_values != null) mem::free(rounded_values);

    uint vulkan_op = (uint)op;
    int status = OMNI_TENSOR_VULKAN_INVALID;
    if (source.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_round_i64_f64(
            source.device_handle,
            source.byte_len,
            source.element_count,
            vulkan_op,
            rounded_values
        );
    } else if (source.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_round_i64_f32(
            source.device_handle,
            source.byte_len,
            source.element_count,
            vulkan_op,
            rounded_values
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (status == OMNI_TENSOR_VULKAN_DOMAIN_ERROR) return raise_error(interp, range_message);
        if (status == OMNI_TENSOR_VULKAN_INVALID_ARGUMENT) return raise_error(interp, "rounding: invalid Vulkan rounding operation");
        return tensor_vulkan_status_raise(interp, status, "rounding: Vulkan rounding kernel failed");
    }

    void** dst_values = (void**)out_value.tensor_val.data;
    for (usz i = 0; i < source.element_count; i++) {
        void* result = omni_big_integer_from_i64(rounded_values[i]);
        if (result == null) return raise_error(interp, "rounding: failed to allocate BigInteger tensor element");
        if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
        dst_values[i] = result;
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_rounding_value(Value* tensor_value, int op, char[] range_message, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "rounding: expected Tensor");

    if (tensor_value.tensor_val == null || !tensor_has_valid_metadata(tensor_value.tensor_val)) {
        return tensor_runtime_raise(interp, "rounding: tensor backing storage is missing");
    }
    if (tensor_expr_has_device(tensor_value, TENSOR_DEVICE_CUDA)) {
        Value* realized = tensor_realize_to_value(tensor_value, interp);
        if (realized == null || realized.tag == ERROR) return realized;
        bool cleanup_realized = realized != tensor_value;
        defer if (cleanup_realized) {
            boundary_cleanup_materialized_value(null, realized, interp.current_scope);
        }
        if (!is_tensor(realized) || realized.tensor_val == null || !tensor_has_valid_metadata(realized.tensor_val)) {
            return tensor_runtime_raise(interp, "rounding: tensor backing storage is missing");
        }
        if (realized.tensor_val.device != TENSOR_DEVICE_CUDA) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: CUDA Tensor rounding must execute on CUDA", null);
        }
        return tensor_cuda_rounding_to_cpu_big_integer_value(realized.tensor_val, op, range_message, interp);
    }
    if (tensor_expr_has_device(tensor_value, TENSOR_DEVICE_VULKAN)) {
        Value* realized = tensor_realize_to_value(tensor_value, interp);
        if (realized == null || realized.tag == ERROR) return realized;
        bool cleanup_realized = realized != tensor_value;
        defer if (cleanup_realized) {
            boundary_cleanup_materialized_value(null, realized, interp.current_scope);
        }
        if (!is_tensor(realized) || realized.tensor_val == null || !tensor_has_valid_metadata(realized.tensor_val)) {
            return tensor_runtime_raise(interp, "rounding: tensor backing storage is missing");
        }
        if (realized.tensor_val.device != TENSOR_DEVICE_VULKAN) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "rounding: Vulkan Tensor rounding must execute on Vulkan", null);
        }
        return tensor_vulkan_rounding_to_cpu_big_integer_value(realized.tensor_val, op, range_message, interp);
    }

    Value* concrete_value = null;
    TensorVal* source = null;
    Value* resolve_err = tensor_expr_resolve_concrete_cpu(
        tensor_value,
        &source,
        &concrete_value,
        interp,
        "rounding: tensor backing storage is missing",
        "rounding: unsupported Tensor device"
    );
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }
    if (source.dtype == TENSOR_DTYPE_BIG_COMPLEX || tensor_dtype_is_fixed_complex(source.dtype)) {
        return raise_error(interp, "rounding: complex numbers are not supported");
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_BIG_INTEGER, shape_slice, "rounding: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "rounding: tensor result backing storage is missing");
    }

    void** dst_values = (void**)out_value.tensor_val.data;
    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* src_values = (double*)source.data;
            for (usz i = 0; i < source.element_count; i++) {
                double rounded = tensor_apply_double_rounding(src_values[i], op);
                long narrowed = 0;
                if (!math_try_double_to_int(rounded, &narrowed)) return raise_error(interp, range_message);
                void* result = omni_big_integer_from_i64(narrowed);
                if (result == null) return raise_error(interp, "rounding: failed to allocate BigInteger tensor element");
                if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* src_values = (float*)source.data;
            for (usz i = 0; i < source.element_count; i++) {
                double rounded = tensor_apply_double_rounding((double)src_values[i], op);
                long narrowed = 0;
                if (!math_try_double_to_int(rounded, &narrowed)) return raise_error(interp, range_message);
                void* result = omni_big_integer_from_i64(narrowed);
                if (result == null) return raise_error(interp, "rounding: failed to allocate BigInteger tensor element");
                if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** src_values = (void**)source.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "rounding: tensor BigInteger element is missing");
                void* result = omni_big_integer_clone(src_values[i]);
                if (result == null) return raise_error(interp, "rounding: failed to allocate BigInteger tensor element");
                if (dst_values[i] != null) omni_big_integer_free(dst_values[i]);
                dst_values[i] = result;
            }
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** src_values = (void**)source.data;
            for (usz i = 0; i < source.element_count; i++) {
                if (src_values[i] == null) return tensor_runtime_raise(interp, "rounding: tensor BigFloat element is missing");
                Value* arg = make_big_float_from_owned_handle(interp, omni_big_float_clone(src_values[i]), range_message);
                if (arg == null || arg.tag == ERROR) return arg;
                Value* rounded = big_float_round_to_integer_value(interp, arg, op, range_message);
                if (rounded == null || rounded.tag == ERROR) return rounded;
                Value* err = tensor_store_integer_value(dst_values, i, rounded, interp, "rounding: failed to allocate BigInteger tensor element");
                if (err != null) return err;
            }
        }
        case TENSOR_DTYPE_BIG_COMPLEX:
            return raise_error(interp, "rounding: complex numbers are not supported");
        case TENSOR_DTYPE_COMPLEX128:
        case TENSOR_DTYPE_COMPLEX64:
            return raise_error(interp, "rounding: complex numbers are not supported");
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_floor_value(Value* tensor_value, Interp* interp) {
    return tensor_rounding_value(tensor_value, BIG_FLOAT_ROUND_FLOOR, "floor: tensor integer result out of supported range", interp);
}

fn Value* tensor_ceiling_value(Value* tensor_value, Interp* interp) {
    return tensor_rounding_value(tensor_value, BIG_FLOAT_ROUND_CEILING, "ceiling: tensor integer result out of supported range", interp);
}

fn Value* tensor_round_value(Value* tensor_value, Interp* interp) {
    return tensor_rounding_value(tensor_value, BIG_FLOAT_ROUND_ROUND, "round: tensor integer result out of supported range", interp);
}

fn Value* tensor_truncate_value(Value* tensor_value, Interp* interp) {
    return tensor_rounding_value(tensor_value, BIG_FLOAT_ROUND_TRUNCATE, "truncate: tensor integer result out of supported range", interp);
}

fn TensorDType tensor_minmax_input_dtype(Value* value, TensorVal* tensor, TensorDType* dtype_out) {
    if (dtype_out == null) return TENSOR_DTYPE_DOUBLE;
    if (tensor != null) {
        *dtype_out = tensor.dtype;
        return *dtype_out;
    }
    if (is_big_complex(value)) {
        *dtype_out = TENSOR_DTYPE_BIG_COMPLEX;
    } else if (is_complex128(value)) {
        *dtype_out = TENSOR_DTYPE_COMPLEX128;
    } else if (is_complex64(value)) {
        *dtype_out = TENSOR_DTYPE_COMPLEX64;
    } else if (is_big_float(value)) {
        *dtype_out = TENSOR_DTYPE_BIG_FLOAT;
    } else if (is_big_integer(value) || is_int(value)) {
        *dtype_out = TENSOR_DTYPE_BIG_INTEGER;
    } else {
        *dtype_out = TENSOR_DTYPE_DOUBLE;
    }
    return *dtype_out;
}

fn TensorDType tensor_minmax_result_dtype(TensorDType left_dtype, TensorDType right_dtype) @inline {
    if (left_dtype == TENSOR_DTYPE_BIG_COMPLEX || right_dtype == TENSOR_DTYPE_BIG_COMPLEX) return TENSOR_DTYPE_BIG_COMPLEX;
    if (tensor_dtype_is_fixed_complex(left_dtype)) return left_dtype;
    if (tensor_dtype_is_fixed_complex(right_dtype)) return right_dtype;
    if (left_dtype == TENSOR_DTYPE_BIG_FLOAT || right_dtype == TENSOR_DTYPE_BIG_FLOAT) return TENSOR_DTYPE_BIG_FLOAT;
    if (left_dtype == TENSOR_DTYPE_DOUBLE || right_dtype == TENSOR_DTYPE_DOUBLE) return TENSOR_DTYPE_DOUBLE;
    if (left_dtype == TENSOR_DTYPE_FLOAT32 || right_dtype == TENSOR_DTYPE_FLOAT32) return TENSOR_DTYPE_FLOAT32;
    return TENSOR_DTYPE_BIG_INTEGER;
}

fn Value* tensor_minmax_value(Value* left_value, Value* right_value, bool is_min, char[] op_name, Interp* interp) {
    bool left_is_tensor = is_tensor(left_value);
    bool right_is_tensor = is_tensor(right_value);
    if (!left_is_tensor && !right_is_tensor) return tensor_type_raise(interp, "minmax: expected at least one Tensor input");

    TensorVal* left_source_tensor = left_is_tensor ? left_value.tensor_val : null;
    TensorVal* right_source_tensor = right_is_tensor ? right_value.tensor_val : null;
    if (left_is_tensor && !tensor_has_valid_metadata(left_source_tensor)) return tensor_runtime_raise(interp, "minmax: left tensor metadata is missing");
    if (right_is_tensor && !tensor_has_valid_metadata(right_source_tensor)) return tensor_runtime_raise(interp, "minmax: right tensor metadata is missing");

    bool touches_vulkan_source = (left_is_tensor && tensor_expr_has_device(left_value, TENSOR_DEVICE_VULKAN)) ||
        (right_is_tensor && tensor_expr_has_device(right_value, TENSOR_DEVICE_VULKAN));
    if (touches_vulkan_source) {
        TensorDType pre_left_dtype = TENSOR_DTYPE_DOUBLE;
        TensorDType pre_right_dtype = TENSOR_DTYPE_DOUBLE;
        tensor_minmax_input_dtype(left_value, left_source_tensor, &pre_left_dtype);
        tensor_minmax_input_dtype(right_value, right_source_tensor, &pre_right_dtype);
        if (pre_left_dtype == TENSOR_DTYPE_BIG_COMPLEX || pre_right_dtype == TENSOR_DTYPE_BIG_COMPLEX ||
            tensor_dtype_is_fixed_complex(pre_left_dtype) || tensor_dtype_is_fixed_complex(pre_right_dtype)) {
            return raise_error(interp, "minmax: complex numbers are not ordered");
        }
        if ((left_is_tensor && tensor_expr_has_non_vulkan_device(left_value)) ||
            (right_is_tensor && tensor_expr_has_non_vulkan_device(right_value))) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "minmax: Vulkan operands must remain Vulkan-placed", null);
        }

        TensorDType pre_result_dtype = tensor_minmax_result_dtype(pre_left_dtype, pre_right_dtype);
        if (!tensor_dtype_is_vulkan_real(pre_result_dtype) ||
            (left_source_tensor != null && left_source_tensor.dtype != pre_result_dtype) ||
            (right_source_tensor != null && right_source_tensor.dtype != pre_result_dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "minmax: Vulkan currently supports matching Float64 or Float32 Tensor storage", null);
        }
    }

    Value* left_concrete = left_value;
    if (left_is_tensor) {
        TensorVal* resolved_left = null;
        Value* resolved_left_value = null;
        Value* resolve_err = tensor_expr_resolve_concrete_any_device(left_value, &resolved_left, &resolved_left_value, interp, "minmax: left tensor backing storage is missing");
        if (resolve_err != null) return resolve_err;
        left_concrete = resolved_left_value;
    }
    bool cleanup_left = left_is_tensor && left_concrete != left_value;
    defer if (cleanup_left) {
        boundary_cleanup_materialized_value(null, left_concrete, interp.current_scope);
    }

    Value* right_concrete = right_value;
    if (right_is_tensor) {
        TensorVal* resolved_right = null;
        Value* resolved_right_value = null;
        Value* resolve_err = tensor_expr_resolve_concrete_any_device(right_value, &resolved_right, &resolved_right_value, interp, "minmax: right tensor backing storage is missing");
        if (resolve_err != null) return resolve_err;
        right_concrete = resolved_right_value;
    }
    bool cleanup_right = right_is_tensor && right_concrete != right_value;
    defer if (cleanup_right) {
        boundary_cleanup_materialized_value(null, right_concrete, interp.current_scope);
    }

    TensorVal* left_tensor = left_is_tensor ? left_concrete.tensor_val : null;
    TensorVal* right_tensor = right_is_tensor ? right_concrete.tensor_val : null;

    TensorDType left_dtype = TENSOR_DTYPE_DOUBLE;
    TensorDType right_dtype = TENSOR_DTYPE_DOUBLE;
    tensor_minmax_input_dtype(left_value, left_tensor, &left_dtype);
    tensor_minmax_input_dtype(right_value, right_tensor, &right_dtype);
    if (left_dtype == TENSOR_DTYPE_BIG_COMPLEX || right_dtype == TENSOR_DTYPE_BIG_COMPLEX ||
        tensor_dtype_is_fixed_complex(left_dtype) || tensor_dtype_is_fixed_complex(right_dtype)) {
        return raise_error(interp, "minmax: complex numbers are not ordered");
    }
    TensorDType result_dtype = tensor_minmax_result_dtype(left_dtype, right_dtype);

    usz* result_shape = null;
    usz result_rank = 0;
    Value* err = tensor_map_result_shape(left_tensor, right_tensor, true, interp, &result_shape, &result_rank);
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
    bool touches_vulkan = (left_tensor != null && left_tensor.device == TENSOR_DEVICE_VULKAN) ||
        (right_tensor != null && right_tensor.device == TENSOR_DEVICE_VULKAN);
    if (touches_vulkan) {
        if (left_tensor != null && left_tensor.device != TENSOR_DEVICE_VULKAN) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "minmax: Vulkan operands must remain Vulkan-placed", null);
        }
        if (right_tensor != null && right_tensor.device != TENSOR_DEVICE_VULKAN) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "minmax: Vulkan operands must remain Vulkan-placed", null);
        }
        if (!tensor_dtype_is_vulkan_real(result_dtype) ||
            (left_tensor != null && left_tensor.dtype != result_dtype) ||
            (right_tensor != null && right_tensor.dtype != result_dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "minmax: Vulkan currently supports matching Float64 or Float32 Tensor storage", null);
        }
        TensorVal* tensor = left_tensor != null ? left_tensor : right_tensor;
        if (tensor == null || tensor.device != TENSOR_DEVICE_VULKAN) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "minmax: Vulkan operands must remain Vulkan-placed", null);
        }
        if (!tensor_map_operand_matches_shape(tensor, shape_slice)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "minmax: Vulkan result shape mismatch", null);
        }
        if (left_tensor != null && !tensor_map_operand_matches_shape(left_tensor, shape_slice)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "minmax: Vulkan result shape mismatch", null);
        }
        if (right_tensor != null && !tensor_map_operand_matches_shape(right_tensor, shape_slice)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "minmax: Vulkan result shape mismatch", null);
        }
        if (!tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_VULKAN) ||
            (left_tensor != null && !tensor_has_valid_device_storage(left_tensor, TENSOR_DEVICE_VULKAN)) ||
            (right_tensor != null && !tensor_has_valid_device_storage(right_tensor, TENSOR_DEVICE_VULKAN))) {
            return tensor_runtime_raise(interp, "minmax: Vulkan tensor backing storage is missing");
        }

        double scalar_f64 = 0.0;
        float scalar_f32 = 0.0f;
        uint mode = 2;
        if (left_tensor == null) {
            bool scalar_ok = result_dtype == TENSOR_DTYPE_FLOAT32
                ? tensor_value_to_float32(left_value, &scalar_f32)
                : tensor_value_to_double(left_value, &scalar_f64);
            if (!scalar_ok) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "minmax: Vulkan scalar operand is not representable by the Tensor dtype", null);
            }
            mode = 1;
        } else if (right_tensor == null) {
            bool scalar_ok = result_dtype == TENSOR_DTYPE_FLOAT32
                ? tensor_value_to_float32(right_value, &scalar_f32)
                : tensor_value_to_double(right_value, &scalar_f64);
            if (!scalar_ok) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "minmax: Vulkan scalar operand is not representable by the Tensor dtype", null);
            }
            mode = 0;
        }

        TensorVal* result_tensor = tensor_alloc_payload_base_checked(result_dtype, shape_slice, false, false, "minmax: failed to allocate Vulkan tensor metadata");
        if (result_tensor == null) return tensor_runtime_raise(interp, "minmax: failed to allocate Vulkan tensor metadata");
        result_tensor.kind = TENSOR_PAYLOAD_CONCRETE;
        result_tensor.device = TENSOR_DEVICE_VULKAN;

        void* out_device = null;
        int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (result_dtype == TENSOR_DTYPE_DOUBLE) {
            status = omni_tensor_backend_vulkan_map_f64(
                left_tensor != null ? left_tensor.device_handle : null,
                right_tensor != null ? right_tensor.device_handle : null,
                result_tensor.byte_len,
                result_tensor.element_count,
                scalar_f64,
                mode,
                is_min ? 4 : 5,
                result_tensor.rank,
                left_tensor != null ? left_tensor.rank : 0,
                right_tensor != null ? right_tensor.rank : 0,
                result_tensor.shape,
                result_tensor.strides,
                left_tensor != null ? left_tensor.shape : null,
                left_tensor != null ? left_tensor.strides : null,
                right_tensor != null ? right_tensor.shape : null,
                right_tensor != null ? right_tensor.strides : null,
                &out_device
            );
        } else if (result_dtype == TENSOR_DTYPE_FLOAT32) {
            status = omni_tensor_backend_vulkan_map_f32(
                left_tensor != null ? left_tensor.device_handle : null,
                right_tensor != null ? right_tensor.device_handle : null,
                result_tensor.byte_len,
                result_tensor.element_count,
                scalar_f32,
                mode,
                is_min ? 4 : 5,
                result_tensor.rank,
                left_tensor != null ? left_tensor.rank : 0,
                right_tensor != null ? right_tensor.rank : 0,
                result_tensor.shape,
                result_tensor.strides,
                left_tensor != null ? left_tensor.shape : null,
                left_tensor != null ? left_tensor.strides : null,
                right_tensor != null ? right_tensor.shape : null,
                right_tensor != null ? right_tensor.strides : null,
                &out_device
            );
        }
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            tensor_free_payload(result_tensor);
            return tensor_vulkan_status_raise(interp, status, "minmax: Vulkan kernel failed");
        }
        result_tensor.device_handle = out_device;
        result_tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

        Value* result = interp.alloc_value();
        if (result == null) {
            tensor_free_payload(result_tensor);
            return tensor_runtime_raise(interp, "minmax: failed to allocate Vulkan tensor value");
        }
        result.tag = TENSOR;
        result.tensor_val = result_tensor;
        main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    if (left_tensor != null && !tensor_has_valid_storage(left_tensor)) return tensor_runtime_raise(interp, "minmax: left tensor backing storage is missing");
    if (right_tensor != null && !tensor_has_valid_storage(right_tensor)) return tensor_runtime_raise(interp, "minmax: right tensor backing storage is missing");

    Value* out_value = make_tensor_uninitialized_checked(interp, result_dtype, shape_slice, "minmax: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "minmax: tensor result backing storage is missing");
    }

    TensorVal* out_tensor = out_value.tensor_val;
    for (usz i = 0; i < out_tensor.element_count; i++) {
        Value* left_arg = left_value;
        Value* right_arg = right_value;
        if (left_tensor != null) {
            usz left_offset = 0;
            if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "minmax: tensor shape mismatch", null);
            }
            left_arg = tensor_value_at_offset(left_tensor, left_offset, interp, "minmax: left tensor element is missing");
            if (left_arg == null || left_arg.tag == ERROR) return left_arg;
        }
        if (right_tensor != null) {
            usz right_offset = 0;
            if (!tensor_map_operand_offset(right_tensor, out_tensor, i, &right_offset)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "minmax: tensor shape mismatch", null);
            }
            right_arg = tensor_value_at_offset(right_tensor, right_offset, interp, "minmax: right tensor element is missing");
            if (right_arg == null || right_arg.tag == ERROR) return right_arg;
        }

        int cmp = 0;
        if (!numeric_values_compare(left_arg, right_arg, &cmp)) {
            return raise_error(interp, "minmax: numeric comparison failed");
        }
        Value* chosen = is_min ? (cmp <= 0 ? left_arg : right_arg) : (cmp >= 0 ? left_arg : right_arg);
        if (result_dtype == TENSOR_DTYPE_BIG_COMPLEX) return raise_error(interp, "minmax: complex numbers are not ordered");

        if (result_dtype == TENSOR_DTYPE_DOUBLE) {
            double out = 0.0;
            if (!try_numeric_to_double(chosen, &out)) {
                return raise_error(interp, op_name);
            }
            ((double*)out_tensor.data)[i] = out;
        } else if (result_dtype == TENSOR_DTYPE_FLOAT32) {
            float out = 0.0f;
            if (!tensor_value_to_float32(chosen, &out)) {
                return raise_error(interp, op_name);
            }
            ((float*)out_tensor.data)[i] = out;
        } else if (result_dtype == TENSOR_DTYPE_BIG_INTEGER) {
            void* handle = big_integer_handle_from_value(chosen);
            if (handle == null) return raise_error(interp, op_name);
            void** dst = (void**)out_tensor.data;
            if (dst[i] != null) omni_big_integer_free(dst[i]);
            dst[i] = handle;
        } else if (result_dtype == TENSOR_DTYPE_BIG_FLOAT) {
            void* handle = big_float_handle_from_value(chosen);
            if (handle == null) return raise_error(interp, op_name);
            void** dst = (void**)out_tensor.data;
            if (dst[i] != null) omni_big_float_free(dst[i]);
            dst[i] = handle;
        }
    }

    keep_out = true;
    return out_value;
}

fn void* tensor_exact_integer_binary_result(
    Value* left_scalar,
    Value* right_scalar,
    void* left_tensor_handle,
    void* right_tensor_handle,
    int op
) {
    if (left_tensor_handle != null && right_tensor_handle != null) {
        return omni_big_integer_binary(left_tensor_handle, right_tensor_handle, op);
    }
    if (left_tensor_handle != null) {
        if (right_scalar == null) return null;
        if (right_scalar.tag == INT) return omni_big_integer_binary_right_i64(left_tensor_handle, right_scalar.int_val, op);
        if (right_scalar.tag == BIG_INTEGER && right_scalar.big_integer_val != null) {
            return omni_big_integer_binary(left_tensor_handle, right_scalar.big_integer_val, op);
        }
        return null;
    }
    if (right_tensor_handle != null) {
        if (left_scalar == null) return null;
        if (left_scalar.tag == INT) return omni_big_integer_binary_left_i64(left_scalar.int_val, right_tensor_handle, op);
        if (left_scalar.tag == BIG_INTEGER && left_scalar.big_integer_val != null) {
            return omni_big_integer_binary(left_scalar.big_integer_val, right_tensor_handle, op);
        }
        return null;
    }
    return null;
}

fn Value* tensor_exact_integer_binary_value(
    Value* left_value,
    Value* right_value,
    int op,
    char[] type_message,
    char[] missing_message,
    char[] shape_message,
    char[] oom_message,
    Interp* interp
) {
    bool left_is_tensor = is_tensor(left_value);
    bool right_is_tensor = is_tensor(right_value);
    if (!left_is_tensor && !right_is_tensor) return tensor_type_raise(interp, type_message);

    Value* left_concrete = left_value;
    TensorVal* left_tensor = null;
    if (left_is_tensor) {
        Value* resolve_err = tensor_expr_resolve_concrete_cpu(
            left_value,
            &left_tensor,
            &left_concrete,
            interp,
            "gcd/lcm: left tensor backing storage is missing",
            "gcd/lcm: unsupported left Tensor device"
        );
        if (resolve_err != null) return resolve_err;
    }
    bool cleanup_left = left_is_tensor && left_concrete != left_value;
    defer if (cleanup_left) {
        boundary_cleanup_materialized_value(null, left_concrete, interp.current_scope);
    }

    Value* right_concrete = right_value;
    TensorVal* right_tensor = null;
    if (right_is_tensor) {
        Value* resolve_err = tensor_expr_resolve_concrete_cpu(
            right_value,
            &right_tensor,
            &right_concrete,
            interp,
            "gcd/lcm: right tensor backing storage is missing",
            "gcd/lcm: unsupported right Tensor device"
        );
        if (resolve_err != null) return resolve_err;
    }
    bool cleanup_right = right_is_tensor && right_concrete != right_value;
    defer if (cleanup_right) {
        boundary_cleanup_materialized_value(null, right_concrete, interp.current_scope);
    }

    if (left_tensor == null && left_is_tensor) left_tensor = left_concrete.tensor_val;
    if (right_tensor == null && right_is_tensor) right_tensor = right_concrete.tensor_val;
    if (left_tensor != null && left_tensor.dtype != TENSOR_DTYPE_BIG_INTEGER) return raise_error(interp, type_message);
    if (right_tensor != null && right_tensor.dtype != TENSOR_DTYPE_BIG_INTEGER) return raise_error(interp, type_message);
    if (left_tensor == null && !is_exact_integer_value(left_value)) return raise_error(interp, type_message);
    if (right_tensor == null && !is_exact_integer_value(right_value)) return raise_error(interp, type_message);

    usz* result_shape = null;
    usz result_rank = 0;
    Value* err = tensor_map_result_shape(left_tensor, right_tensor, true, interp, &result_shape, &result_rank);
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_BIG_INTEGER, shape_slice, oom_message);
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "gcd/lcm: tensor result backing storage is missing");
    }

    TensorVal* out_tensor = out_value.tensor_val;
    void** out_values = (void**)out_tensor.data;
    void** left_values = left_tensor != null ? (void**)left_tensor.data : null;
    void** right_values = right_tensor != null ? (void**)right_tensor.data : null;
    for (usz i = 0; i < out_tensor.element_count; i++) {
        void* left_handle = null;
        void* right_handle = null;
        if (left_tensor != null) {
            usz left_offset = 0;
            if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", shape_message, null);
            }
            left_handle = left_values[left_offset];
        }
        if (right_tensor != null) {
            usz right_offset = 0;
            if (!tensor_map_operand_offset(right_tensor, out_tensor, i, &right_offset)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", shape_message, null);
            }
            right_handle = right_values[right_offset];
        }
        if ((left_tensor != null && left_handle == null) || (right_tensor != null && right_handle == null)) {
            return tensor_runtime_raise(interp, missing_message);
        }

        void* result = tensor_exact_integer_binary_result(
            left_tensor == null ? left_value : null,
            right_tensor == null ? right_value : null,
            left_handle,
            right_handle,
            op);
        if (result == null) return raise_error(interp, oom_message);
        if (out_values[i] != null) omni_big_integer_free(out_values[i]);
        out_values[i] = result;
    }

    keep_out = true;
    return out_value;
}

fn bool tensor_binary_numeric_input_dtype(Value* value, TensorVal* tensor, TensorDType* dtype_out) {
    if (dtype_out == null) return false;
    if (tensor != null) {
        switch (tensor.dtype) {
            case TENSOR_DTYPE_BIG_COMPLEX:
                *dtype_out = TENSOR_DTYPE_BIG_COMPLEX;
            case TENSOR_DTYPE_COMPLEX128:
                *dtype_out = TENSOR_DTYPE_COMPLEX128;
            case TENSOR_DTYPE_COMPLEX64:
                *dtype_out = TENSOR_DTYPE_COMPLEX64;
            case TENSOR_DTYPE_BIG_FLOAT:
                *dtype_out = TENSOR_DTYPE_BIG_FLOAT;
            case TENSOR_DTYPE_BIG_INTEGER:
            case TENSOR_DTYPE_DOUBLE:
                *dtype_out = TENSOR_DTYPE_DOUBLE;
            case TENSOR_DTYPE_FLOAT32:
                *dtype_out = TENSOR_DTYPE_FLOAT32;
        }
        return true;
    }
    if (!is_numeric_value(value)) return false;
    if (is_big_complex(value)) {
        *dtype_out = TENSOR_DTYPE_BIG_COMPLEX;
    } else if (is_complex128(value)) {
        *dtype_out = TENSOR_DTYPE_COMPLEX128;
    } else if (is_complex64(value)) {
        *dtype_out = TENSOR_DTYPE_COMPLEX64;
    } else if (is_big_float(value)) {
        *dtype_out = TENSOR_DTYPE_BIG_FLOAT;
    } else {
        *dtype_out = TENSOR_DTYPE_DOUBLE;
    }
    return true;
}

fn TensorDType tensor_pow_result_dtype(TensorDType left_dtype, TensorDType right_dtype) @inline {
    if (left_dtype == TENSOR_DTYPE_BIG_COMPLEX || right_dtype == TENSOR_DTYPE_BIG_COMPLEX) return TENSOR_DTYPE_BIG_COMPLEX;
    if (left_dtype == TENSOR_DTYPE_COMPLEX128 || right_dtype == TENSOR_DTYPE_COMPLEX128) return TENSOR_DTYPE_COMPLEX128;
    if (left_dtype == TENSOR_DTYPE_COMPLEX64 || right_dtype == TENSOR_DTYPE_COMPLEX64) return TENSOR_DTYPE_COMPLEX64;
    if (left_dtype == TENSOR_DTYPE_BIG_FLOAT || right_dtype == TENSOR_DTYPE_BIG_FLOAT) return TENSOR_DTYPE_BIG_FLOAT;
    return TENSOR_DTYPE_DOUBLE;
}

fn Value* tensor_value_at_offset(TensorVal* tensor, usz offset, Interp* interp, char[] message) {
    if (tensor == null || !tensor_has_valid_storage(tensor) || offset >= tensor.element_count) return tensor_runtime_raise(interp, message);
    if (tensor.storage_offset > tensor.storage_element_count ||
        offset >= tensor.storage_element_count - tensor.storage_offset) {
        return tensor_runtime_raise(interp, message);
    }
    usz storage_offset = tensor.storage_offset + offset;
    switch (tensor.dtype) {
        case TENSOR_DTYPE_DOUBLE:
            return make_double(interp, ((double*)tensor.data)[storage_offset]);
        case TENSOR_DTYPE_FLOAT32:
            return make_float32(interp, ((float*)tensor.data)[storage_offset]);
        case TENSOR_DTYPE_COMPLEX128:
            return make_complex128(interp, ((Complex128Val*)tensor.data)[storage_offset].real, ((Complex128Val*)tensor.data)[storage_offset].imag);
        case TENSOR_DTYPE_COMPLEX64:
            return make_complex64(interp, ((Complex64Val*)tensor.data)[storage_offset].real, ((Complex64Val*)tensor.data)[storage_offset].imag);
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** values = (void**)tensor.data;
            if (values[storage_offset] == null) return tensor_runtime_raise(interp, message);
            return make_big_integer_from_owned_handle(interp, omni_big_integer_clone(values[storage_offset]), message);
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** values = (void**)tensor.data;
            if (values[storage_offset] == null) return tensor_runtime_raise(interp, message);
            return make_big_float_from_owned_handle(interp, omni_big_float_clone(values[storage_offset]), message);
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** values = (void**)tensor.data;
            if (values[storage_offset] == null) return tensor_runtime_raise(interp, message);
            return make_big_complex_from_owned_handle(interp, omni_big_complex_clone(values[storage_offset]), message);
        }
    }
}

fn Value* tensor_pow_value(Value* left_value, Value* right_value, Interp* interp) {
    bool left_is_tensor = is_tensor(left_value);
    bool right_is_tensor = is_tensor(right_value);
    if (!left_is_tensor && !right_is_tensor) return tensor_type_raise(interp, "pow: expected at least one Tensor input");

    Value* left_concrete = left_value;
    TensorVal* left_tensor = null;
    if (left_is_tensor) {
        Value* resolve_err = tensor_expr_resolve_concrete_cpu(
            left_value,
            &left_tensor,
            &left_concrete,
            interp,
            "pow: left tensor backing storage is missing",
            "pow: unsupported left Tensor device"
        );
        if (resolve_err != null) return resolve_err;
    }
    bool cleanup_left = left_is_tensor && left_concrete != left_value;
    defer if (cleanup_left) {
        boundary_cleanup_materialized_value(null, left_concrete, interp.current_scope);
    }

    Value* right_concrete = right_value;
    TensorVal* right_tensor = null;
    if (right_is_tensor) {
        Value* resolve_err = tensor_expr_resolve_concrete_cpu(
            right_value,
            &right_tensor,
            &right_concrete,
            interp,
            "pow: right tensor backing storage is missing",
            "pow: unsupported right Tensor device"
        );
        if (resolve_err != null) return resolve_err;
    }
    bool cleanup_right = right_is_tensor && right_concrete != right_value;
    defer if (cleanup_right) {
        boundary_cleanup_materialized_value(null, right_concrete, interp.current_scope);
    }

    if (left_tensor == null && left_is_tensor) left_tensor = left_concrete.tensor_val;
    if (right_tensor == null && right_is_tensor) right_tensor = right_concrete.tensor_val;

    TensorDType left_dtype = TENSOR_DTYPE_DOUBLE;
    TensorDType right_dtype = TENSOR_DTYPE_DOUBLE;
    if (!tensor_binary_numeric_input_dtype(left_value, left_tensor, &left_dtype)) return primitive_arg_type_raise(interp, "pow", 0, "number or Tensor", left_value);
    if (!tensor_binary_numeric_input_dtype(right_value, right_tensor, &right_dtype)) return primitive_arg_type_raise(interp, "pow", 1, "number or Tensor", right_value);
    if (tensor_dtype_is_fixed_complex(left_dtype) || tensor_dtype_is_fixed_complex(right_dtype)) {
        return raise_error(interp, "pow: fixed-width complex numbers are not supported");
    }
    TensorDType result_dtype = tensor_pow_result_dtype(left_dtype, right_dtype);

    usz* result_shape = null;
    usz result_rank = 0;
    Value* err = tensor_map_result_shape(left_tensor, right_tensor, true, interp, &result_shape, &result_rank);
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, result_dtype, shape_slice, "pow: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "pow: tensor result backing storage is missing");
    }

    TensorVal* out_tensor = out_value.tensor_val;
    for (usz i = 0; i < out_tensor.element_count; i++) {
        Value* left_arg = left_value;
        Value* right_arg = right_value;
        if (left_tensor != null) {
            usz left_offset = 0;
            if (!tensor_map_operand_offset(left_tensor, out_tensor, i, &left_offset)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "pow: tensor shape mismatch", null);
            }
            left_arg = tensor_value_at_offset(left_tensor, left_offset, interp, "pow: left tensor element is missing");
            if (left_arg == null || left_arg.tag == ERROR) return left_arg;
        }
        if (right_tensor != null) {
            usz right_offset = 0;
            if (!tensor_map_operand_offset(right_tensor, out_tensor, i, &right_offset)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "pow: tensor shape mismatch", null);
            }
            right_arg = tensor_value_at_offset(right_tensor, right_offset, interp, "pow: right tensor element is missing");
            if (right_arg == null || right_arg.tag == ERROR) return right_arg;
        }

        switch (result_dtype) {
            case TENSOR_DTYPE_DOUBLE: {
                double base = 0.0;
                double exponent = 0.0;
                if (!try_numeric_to_double(left_arg, &base) || !try_numeric_to_double(right_arg, &exponent)) {
                    return raise_error(interp, "pow: tensor element out of Float64 range");
                }
                ((double*)out_tensor.data)[i] = tensor_c_pow(base, exponent);
            }
            case TENSOR_DTYPE_FLOAT32:
                return tensor_runtime_raise(interp, "pow: unreachable Float32 result dtype");
            case TENSOR_DTYPE_BIG_INTEGER:
                return tensor_runtime_raise(interp, "pow: unreachable BigInteger result dtype");
            case TENSOR_DTYPE_BIG_FLOAT: {
                Value* result = big_float_binary_math_values(interp, left_arg, right_arg, BIG_FLOAT_MATH_POW, "pow: BigFloat tensor evaluation error");
                if (result == null || result.tag == ERROR) return result;
                void** out = (void**)out_tensor.data;
                void* cloned = omni_big_float_clone(result.big_float_val);
                if (cloned == null) return raise_error(interp, "pow: failed to allocate BigFloat tensor element");
                if (out[i] != null) omni_big_float_free(out[i]);
                out[i] = cloned;
            }
            case TENSOR_DTYPE_BIG_COMPLEX: {
                Value* result = big_complex_binary_math_values(interp, left_arg, right_arg, BIG_COMPLEX_MATH_POW, "pow: BigComplex tensor evaluation error");
                if (result == null || result.tag == ERROR) return result;
                void** out = (void**)out_tensor.data;
                void* cloned = omni_big_complex_clone(result.big_complex_val);
                if (cloned == null) return raise_error(interp, "pow: failed to allocate BigComplex tensor element");
                if (out[i] != null) omni_big_complex_free(out[i]);
                out[i] = cloned;
            }
            case TENSOR_DTYPE_COMPLEX128:
                return tensor_runtime_raise(interp, "pow: unreachable Complex128 result dtype");
            case TENSOR_DTYPE_COMPLEX64:
                return tensor_runtime_raise(interp, "pow: unreachable Complex64 result dtype");
        }
    }

    keep_out = true;
    return out_value;
}

fn TensorDType tensor_atan2_result_dtype(TensorDType y_dtype, TensorDType x_dtype) @inline {
    if (y_dtype == TENSOR_DTYPE_BIG_FLOAT || x_dtype == TENSOR_DTYPE_BIG_FLOAT) return TENSOR_DTYPE_BIG_FLOAT;
    return TENSOR_DTYPE_DOUBLE;
}

fn Value* tensor_atan2_value(Value* y_value, Value* x_value, Interp* interp) {
    bool y_is_tensor = is_tensor(y_value);
    bool x_is_tensor = is_tensor(x_value);
    if (!y_is_tensor && !x_is_tensor) return tensor_type_raise(interp, "atan2: expected at least one Tensor input");

    Value* y_concrete = y_value;
    TensorVal* y_tensor = null;
    if (y_is_tensor) {
        Value* resolve_err = tensor_expr_resolve_concrete_cpu(
            y_value,
            &y_tensor,
            &y_concrete,
            interp,
            "atan2: y tensor backing storage is missing",
            "atan2: unsupported y Tensor device"
        );
        if (resolve_err != null) return resolve_err;
    }
    bool cleanup_y = y_is_tensor && y_concrete != y_value;
    defer if (cleanup_y) {
        boundary_cleanup_materialized_value(null, y_concrete, interp.current_scope);
    }

    Value* x_concrete = x_value;
    TensorVal* x_tensor = null;
    if (x_is_tensor) {
        Value* resolve_err = tensor_expr_resolve_concrete_cpu(
            x_value,
            &x_tensor,
            &x_concrete,
            interp,
            "atan2: x tensor backing storage is missing",
            "atan2: unsupported x Tensor device"
        );
        if (resolve_err != null) return resolve_err;
    }
    bool cleanup_x = x_is_tensor && x_concrete != x_value;
    defer if (cleanup_x) {
        boundary_cleanup_materialized_value(null, x_concrete, interp.current_scope);
    }

    if (y_tensor == null && y_is_tensor) y_tensor = y_concrete.tensor_val;
    if (x_tensor == null && x_is_tensor) x_tensor = x_concrete.tensor_val;

    TensorDType y_dtype = TENSOR_DTYPE_DOUBLE;
    TensorDType x_dtype = TENSOR_DTYPE_DOUBLE;
    if (!tensor_binary_numeric_input_dtype(y_value, y_tensor, &y_dtype)) return primitive_arg_type_raise(interp, "atan2", 0, "number or Tensor", y_value);
    if (!tensor_binary_numeric_input_dtype(x_value, x_tensor, &x_dtype)) return primitive_arg_type_raise(interp, "atan2", 1, "number or Tensor", x_value);
    if (y_dtype == TENSOR_DTYPE_BIG_COMPLEX || x_dtype == TENSOR_DTYPE_BIG_COMPLEX ||
        tensor_dtype_is_fixed_complex(y_dtype) || tensor_dtype_is_fixed_complex(x_dtype)) {
        return raise_error(interp, "atan2: complex numbers are not supported");
    }
    TensorDType result_dtype = tensor_atan2_result_dtype(y_dtype, x_dtype);

    usz* result_shape = null;
    usz result_rank = 0;
    Value* err = tensor_map_result_shape(y_tensor, x_tensor, true, interp, &result_shape, &result_rank);
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, result_dtype, shape_slice, "atan2: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "atan2: tensor result backing storage is missing");
    }

    TensorVal* out_tensor = out_value.tensor_val;
    for (usz i = 0; i < out_tensor.element_count; i++) {
        Value* y_arg = y_value;
        Value* x_arg = x_value;
        if (y_tensor != null) {
            usz y_offset = 0;
            if (!tensor_map_operand_offset(y_tensor, out_tensor, i, &y_offset)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "atan2: tensor shape mismatch", null);
            }
            y_arg = tensor_value_at_offset(y_tensor, y_offset, interp, "atan2: y tensor element is missing");
            if (y_arg == null || y_arg.tag == ERROR) return y_arg;
        }
        if (x_tensor != null) {
            usz x_offset = 0;
            if (!tensor_map_operand_offset(x_tensor, out_tensor, i, &x_offset)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "atan2: tensor shape mismatch", null);
            }
            x_arg = tensor_value_at_offset(x_tensor, x_offset, interp, "atan2: x tensor element is missing");
            if (x_arg == null || x_arg.tag == ERROR) return x_arg;
        }

        switch (result_dtype) {
            case TENSOR_DTYPE_DOUBLE: {
                double y = 0.0;
                double x = 0.0;
                if (!try_numeric_to_double(y_arg, &y) || !try_numeric_to_double(x_arg, &x)) {
                    return raise_error(interp, "atan2: tensor element out of Float64 range");
                }
                ((double*)out_tensor.data)[i] = tensor_c_atan2(y, x);
            }
            case TENSOR_DTYPE_FLOAT32:
                return tensor_runtime_raise(interp, "atan2: unreachable Float32 result dtype");
            case TENSOR_DTYPE_BIG_INTEGER:
                return tensor_runtime_raise(interp, "atan2: unreachable BigInteger result dtype");
            case TENSOR_DTYPE_BIG_FLOAT: {
                Value* result = big_float_binary_math_values(interp, y_arg, x_arg, BIG_FLOAT_MATH_ATAN2, "atan2: BigFloat tensor evaluation error");
                if (result == null || result.tag == ERROR) return result;
                void** out = (void**)out_tensor.data;
                void* cloned = omni_big_float_clone(result.big_float_val);
                if (cloned == null) return raise_error(interp, "atan2: failed to allocate BigFloat tensor element");
                if (out[i] != null) omni_big_float_free(out[i]);
                out[i] = cloned;
            }
            case TENSOR_DTYPE_BIG_COMPLEX:
                return tensor_runtime_raise(interp, "atan2: unreachable BigComplex result dtype");
            case TENSOR_DTYPE_COMPLEX128:
                return tensor_runtime_raise(interp, "atan2: unreachable Complex128 result dtype");
            case TENSOR_DTYPE_COMPLEX64:
                return tensor_runtime_raise(interp, "atan2: unreachable Complex64 result dtype");
        }
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_big_complex_conjugate_value(Value* tensor_value, Interp* interp) {
    if (!is_tensor(tensor_value)) return tensor_type_raise(interp, "conjugate: expected Tensor");
    TensorVal* source = null;
    Value* concrete_value = null;
    Value* resolve_err = tensor_expr_resolve_concrete_any_device(tensor_value, &source, &concrete_value, interp, "conjugate: tensor backing storage is missing");
    if (resolve_err != null) return resolve_err;
    bool cleanup_concrete_value = concrete_value != tensor_value;
    defer if (cleanup_concrete_value) {
        boundary_cleanup_materialized_value(null, concrete_value, interp.current_scope);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    if (source.device == TENSOR_DEVICE_VULKAN) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            return tensor_vulkan_map_complex_unary_value(
                source,
                5,
                shape_slice,
                interp,
                "conjugate: failed to allocate Vulkan tensor metadata",
                "conjugate: failed to allocate Vulkan tensor value",
                "conjugate: Vulkan currently supports dense row-major Complex128 and Complex64 tensors",
                "conjugate: Vulkan tensor backing storage is missing",
                "conjugate: Vulkan complex unary kernel failed"
            );
        }
        return tensor_vulkan_map_unary_value(
            source,
            3,
            shape_slice,
            interp,
            "conjugate: failed to allocate Vulkan tensor metadata",
            "conjugate: failed to allocate Vulkan tensor value",
            "conjugate: Vulkan currently supports dense row-major Float64 and Float32 tensors",
            "conjugate: Vulkan tensor backing storage is missing",
            "conjugate: Vulkan unary kernel failed"
        );
    }
    if (source.device == TENSOR_DEVICE_CUDA) {
        if (tensor_dtype_is_fixed_complex(source.dtype)) {
            return tensor_cuda_map_complex_unary_value(
                source,
                5,
                shape_slice,
                interp,
                "conjugate: failed to allocate CUDA tensor metadata",
                "conjugate: failed to allocate CUDA tensor value",
                "conjugate: CUDA currently supports dense row-major Complex128 and Complex64 tensors",
                "conjugate: CUDA tensor backing storage is missing",
                "conjugate: CUDA complex unary kernel failed"
            );
        }
        return tensor_cuda_map_unary_value(
            source,
            3,
            shape_slice,
            interp,
            "conjugate: failed to allocate CUDA tensor metadata",
            "conjugate: failed to allocate CUDA tensor value",
            "conjugate: CUDA currently supports dense row-major Float64 and Float32 tensors",
            "conjugate: CUDA tensor backing storage is missing",
            "conjugate: CUDA unary kernel failed"
        );
    }

    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "conjugate: tensor backing storage is missing");

    if (source.dtype == TENSOR_DTYPE_COMPLEX128) {
        Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX128, shape_slice, "conjugate: failed to allocate tensor result");
        if (out_value == null || out_value.tag == ERROR) return out_value;
        bool keep_out = false;
        defer if (!keep_out) {
            boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
        }
        if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
            return tensor_runtime_raise(interp, "conjugate: tensor result backing storage is missing");
        }
        Complex128Val* src_values = (Complex128Val*)source.data;
        Complex128Val* dst_values = (Complex128Val*)out_value.tensor_val.data;
        for (usz i = 0; i < source.element_count; i++) {
            dst_values[i].real = src_values[i].real;
            dst_values[i].imag = -src_values[i].imag;
        }
        keep_out = true;
        return out_value;
    }
    if (source.dtype == TENSOR_DTYPE_COMPLEX64) {
        Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_COMPLEX64, shape_slice, "conjugate: failed to allocate tensor result");
        if (out_value == null || out_value.tag == ERROR) return out_value;
        bool keep_out = false;
        defer if (!keep_out) {
            boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
        }
        if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
            return tensor_runtime_raise(interp, "conjugate: tensor result backing storage is missing");
        }
        Complex64Val* src_values = (Complex64Val*)source.data;
        Complex64Val* dst_values = (Complex64Val*)out_value.tensor_val.data;
        for (usz i = 0; i < source.element_count; i++) {
            dst_values[i].real = src_values[i].real;
            dst_values[i].imag = -src_values[i].imag;
        }
        keep_out = true;
        return out_value;
    }

    if (source.dtype != TENSOR_DTYPE_BIG_COMPLEX) {
        Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "conjugate: failed to allocate tensor result");
        if (out_value == null || out_value.tag == ERROR) return out_value;
        bool keep_out = false;
        defer if (!keep_out) {
            boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
        }
        if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
            return tensor_runtime_raise(interp, "conjugate: tensor result backing storage is missing");
        }
        if (!tensor_copy_concrete_storage(out_value.tensor_val, source)) {
            return tensor_runtime_raise(interp, "conjugate: failed to copy tensor storage");
        }
        keep_out = true;
        return out_value;
    }

    Value* out_value = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_BIG_COMPLEX, shape_slice, "conjugate: failed to allocate tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "conjugate: tensor result backing storage is missing");
    }

    void** src_values = (void**)source.data;
    void** dst_values = (void**)out_value.tensor_val.data;
    for (usz i = 0; i < source.element_count; i++) {
        if (src_values[i] == null) return tensor_runtime_raise(interp, "conjugate: tensor BigComplex element is missing");
        void* conjugated = omni_big_complex_conjugate(src_values[i]);
        if (conjugated == null) return raise_error(interp, "conjugate: failed to conjugate tensor element");
        if (dst_values[i] != null) omni_big_complex_free(dst_values[i]);
        dst_values[i] = conjugated;
    }

    keep_out = true;
    return out_value;
}

fn Value* tensor_construct_from_shape_and_data(Value* dtype_arg, Value* shape_arg, Value* data_arg, Interp* interp) {
    TensorDType dtype = TENSOR_DTYPE_DOUBLE;
    if (!tensor_dtype_arg_to_dtype(dtype_arg, interp, &dtype)) {
        return tensor_type_raise(interp, "Tensor: first argument must be Float64, Float32, Complex128, Complex64, BigInteger, BigFloat, or BigComplex");
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

    Value* materialized_data = null;
    Value* fill_data_arg = data_arg;
    if (data_arg != null && data_arg.tag == ITERATOR) {
        Value*[1] iterator_args = { data_arg };
        materialized_data = prim_to_array(iterator_args[0:1], null, interp);
        if (materialized_data == null) return tensor_runtime_raise(interp, "Tensor: failed to consume iterator data");
        if (materialized_data.tag == ERROR) return materialized_data;
        if (!is_array(materialized_data)) return tensor_runtime_raise(interp, "Tensor: iterator data did not produce an array");
        fill_data_arg = materialized_data;
    }
    defer if (materialized_data != null) {
        boundary_cleanup_materialized_value(null, materialized_data, interp.current_scope);
    }

    err = tensor_fill_from_data_arg(tensor.tensor_val, fill_data_arg, interp);
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

fn Value* tensor_construct_inferred_auto(Value* data_arg, Interp* interp) {
    return tensor_construct_inferred(data_arg, tensor_default_inferred_dtype(data_arg), interp);
}

fn Value* tensor_construct_from_iterator(Value* iterator_value, TensorDType dtype, Interp* interp) {
    Value*[1] iterator_args = { iterator_value };
    Value* materialized_data = prim_to_array(iterator_args[0:1], null, interp);
    if (materialized_data == null) return tensor_runtime_raise(interp, "Tensor: failed to consume iterator data");
    if (materialized_data.tag == ERROR) return materialized_data;
    if (!is_array(materialized_data)) return tensor_runtime_raise(interp, "Tensor: iterator data did not produce an array");
    defer boundary_cleanup_materialized_value(null, materialized_data, interp.current_scope);
    return tensor_construct_inferred(materialized_data, dtype, interp);
}

fn Value* tensor_construct_from_iterator_auto(Value* iterator_value, Interp* interp) {
    Value*[1] iterator_args = { iterator_value };
    Value* materialized_data = prim_to_array(iterator_args[0:1], null, interp);
    if (materialized_data == null) return tensor_runtime_raise(interp, "Tensor: failed to consume iterator data");
    if (materialized_data.tag == ERROR) return materialized_data;
    if (!is_array(materialized_data)) return tensor_runtime_raise(interp, "Tensor: iterator data did not produce an array");
    defer boundary_cleanup_materialized_value(null, materialized_data, interp.current_scope);
    return tensor_construct_inferred_auto(materialized_data, interp);
}

fn Value* prim_tensor(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 1) {
        if (args[0] != null && args[0].tag == ITERATOR) return tensor_construct_from_iterator_auto(args[0], interp);
        return tensor_construct_inferred_auto(args[0], interp);
    }
    if (args.len == 2) {
        TensorDType dtype = TENSOR_DTYPE_DOUBLE;
        if (tensor_dtype_arg_to_dtype(args[0], interp, &dtype)) {
            if (args[1] != null && args[1].tag == ITERATOR) return tensor_construct_from_iterator(args[1], dtype, interp);
            return tensor_construct_inferred(args[1], dtype, interp);
        }
        if (tensor_dtype_arg_to_dtype(args[1], interp, &dtype)) {
            if (args[0] != null && args[0].tag == ITERATOR) return tensor_construct_from_iterator(args[0], dtype, interp);
            return tensor_construct_inferred(args[0], dtype, interp);
        }
        return tensor_type_raise(interp, "Tensor: expected data with optional Float64, Float32, Complex128, Complex64, BigInteger, BigFloat, or BigComplex dtype");
    }
    if (args.len == 3) return tensor_construct_from_shape_and_data(args[0], args[1], args[2], interp);
    return primitive_arity_raise(interp, "Tensor", "1, 2, or 3 arguments: data [dtype] or dtype shape data-or-scalar", args.len);
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
    if (dest != null && dest.device == TENSOR_DEVICE_CUDA) {
        if (is_numeric_value(args[0])) {
            Value* err = tensor_fill_cuda_destination(dest, args[0], interp);
            if (err != null) return err;
            return args[1];
        }
        return tensor_copy_into_destination(args[0], args[1], interp);
    }
    if (dest != null && dest.device == TENSOR_DEVICE_VULKAN) {
        if (is_numeric_value(args[0])) {
            Value* err = tensor_fill_vulkan_destination(dest, args[0], interp);
            if (err != null) return err;
            return args[1];
        }
        return tensor_copy_into_destination(args[0], args[1], interp);
    }
    if (dest == null) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    Value* dest_check = tensor_require_cpu_destination(dest, interp);
    if (dest_check != null) return dest_check;

    if (is_numeric_value(args[0])) {
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

fn SymbolId tensor_device_symbol(TensorDeviceKind device, Interp* interp) {
    switch (device) {
        case TENSOR_DEVICE_CPU:
            return interp.symbols.intern("cpu");
        case TENSOR_DEVICE_CUDA:
            return interp.symbols.intern("cuda");
        case TENSOR_DEVICE_VULKAN:
            return interp.symbols.intern("vulkan");
    }
}

fn Value* prim_device(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "device", "1 argument: tensor", args.len);
    if (!is_tensor(args[0])) return raise_error(interp, "device: expected Tensor");
    TensorVal* tensor = args[0].tensor_val;
    if (tensor == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "device: tensor backing storage is missing");
    return make_symbol(interp, tensor_device_symbol(tensor.device, interp));
}

fn Value* tensor_usz_metadata_value(Interp* interp, usz value, char[] label) {
    if (value > (usz)long.max) return raise_error(interp, label);
    return make_int(interp, (long)value);
}

fn Value* tensor_metadata_usz_array(Interp* interp, usz* values, usz count, char[] oom_message, char[] range_message) {
    Value* out = make_array_checked(interp, count, oom_message);
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != ARRAY || out.array_val == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "tensor-layout: result array backing storage is missing", null);
    }
    for (usz i = 0; i < count; i++) {
        Value* item = tensor_usz_metadata_value(interp, values == null ? 0 : values[i], range_message);
        if (item == null || item.tag == ERROR) return item;
        out.array_val.items[i] = item;
    }
    out.array_val.length = count;
    return out;
}

fn Value* tensor_layout_set_field(Interp* interp, Value* out, char[] key_name, Value* value) {
    if (value == null || value.tag == ERROR) return value;
    if (!hashmap_set_checked(out.hashmap_val, make_symbol(interp, interp.symbols.intern(key_name)), value, interp)) {
        return raise_error(interp, "tensor-layout: out of memory while building result");
    }
    return null;
}

fn SymbolId tensor_payload_kind_symbol(TensorPayloadKind kind, Interp* interp) {
    switch (kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return interp.symbols.intern("concrete");
        case TENSOR_PAYLOAD_MAP:
            return interp.symbols.intern("map");
        case TENSOR_PAYLOAD_CONTRACT:
            return interp.symbols.intern("contract");
        case TENSOR_PAYLOAD_VIEW:
            return interp.symbols.intern("view");
    }
}

fn SymbolId tensor_write_policy_symbol(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return interp.symbols.intern("invalid");
    if (tensor.is_view) return tensor.mutable ? interp.symbols.intern("mutable-view") : interp.symbols.intern("read-only-view");
    return tensor.mutable ? interp.symbols.intern("mutable") : interp.symbols.intern("immutable");
}

fn SymbolId tensor_owner_symbol(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return interp.symbols.intern("invalid");
    if (tensor.is_view) return interp.symbols.intern("view-source");
    if (tensor.kind == TENSOR_PAYLOAD_CONCRETE) return interp.symbols.intern("self");
    return interp.symbols.intern("expression");
}

fn Value* prim_tensor_layout(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "tensor-layout", "1 argument: tensor", args.len);
    if (!is_tensor(args[0])) return raise_error(interp, "tensor-layout: expected Tensor");
    TensorVal* tensor = args[0].tensor_val;
    if (tensor == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "tensor-layout: tensor backing storage is missing", null);
    if (!tensor_has_valid_metadata(tensor)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "tensor-layout: tensor metadata is invalid", null);
    }

    Value* out = make_hashmap_checked(interp, 32, "tensor-layout: out of memory");
    if (out == null || out.tag == ERROR) return out;
    Value* err = null;

    err = tensor_layout_set_field(interp, out, "dtype", make_symbol(interp, tensor_dtype_symbol(tensor.dtype, interp)));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "device", make_symbol(interp, tensor_device_symbol(tensor.device, interp)));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "payload", make_symbol(interp, tensor_payload_kind_symbol(tensor.kind, interp)));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "layout", make_symbol(interp, tensor_dense_row_major(tensor) ? interp.symbols.intern("dense-row-major") : interp.symbols.intern("strided")));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "dense-row-major", make_symbol(interp, tensor_dense_row_major(tensor) ? interp.sym_true : interp.sym_false));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "shape", tensor_metadata_usz_array(interp, tensor.shape, tensor.rank, "tensor-layout: failed to allocate shape array", "tensor-layout: tensor dimension is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "strides", tensor_metadata_usz_array(interp, tensor.strides, tensor.rank, "tensor-layout: failed to allocate strides array", "tensor-layout: tensor stride is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "rank", tensor_usz_metadata_value(interp, tensor.rank, "tensor-layout: tensor rank is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "element-count", tensor_usz_metadata_value(interp, tensor.element_count, "tensor-layout: tensor element count is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "byte-length", tensor_usz_metadata_value(interp, tensor.byte_len, "tensor-layout: tensor byte length is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "storage-offset", tensor_usz_metadata_value(interp, tensor.storage_offset, "tensor-layout: tensor storage offset is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "storage-elements", tensor_usz_metadata_value(interp, tensor.storage_element_count, "tensor-layout: tensor storage element count is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "storage-bytes", tensor_usz_metadata_value(interp, tensor.storage_byte_len, "tensor-layout: tensor storage byte length is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "is-view", make_symbol(interp, tensor.is_view ? interp.sym_true : interp.sym_false));
    if (err != null) return err;
    bool owns_storage = tensor.kind == TENSOR_PAYLOAD_CONCRETE && !tensor.is_view;
    err = tensor_layout_set_field(interp, out, "owns-storage", make_symbol(interp, owns_storage ? interp.sym_true : interp.sym_false));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "owner", make_symbol(interp, tensor_owner_symbol(tensor, interp)));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "write-policy", make_symbol(interp, tensor_write_policy_symbol(tensor, interp)));
    if (err != null) return err;
    return out;
}

fn void tensor_cuda_device_finalizer(void* handle) {
    omni_tensor_backend_cuda_free(handle);
}

fn void tensor_vulkan_device_finalizer(void* handle) {
    omni_tensor_backend_vulkan_free(handle);
}

fn Value* tensor_backend_entry(
    Interp* interp,
    SymbolId name,
    bool available,
    SymbolId placement,
    SymbolId reason
) {
    Value* entry = make_hashmap_checked(interp, 16, "tensor-backends: out of memory");
    if (entry == null || entry.tag == ERROR) return entry;
    Value* available_value = available ? make_symbol(interp, interp.sym_true) : make_symbol(interp, interp.sym_false);
    if (!hashmap_set_checked(entry.hashmap_val, make_symbol(interp, interp.symbols.intern("name")), make_symbol(interp, name), interp) ||
        !hashmap_set_checked(entry.hashmap_val, make_symbol(interp, interp.symbols.intern("available")), available_value, interp) ||
        !hashmap_set_checked(entry.hashmap_val, make_symbol(interp, interp.symbols.intern("device")), make_symbol(interp, placement), interp)) {
        return raise_error(interp, "tensor-backends: out of memory while building backend entry");
    }
    if (reason != INVALID_SYMBOL_ID &&
        !hashmap_set_checked(entry.hashmap_val, make_symbol(interp, interp.symbols.intern("reason")), make_symbol(interp, reason), interp)) {
        return raise_error(interp, "tensor-backends: out of memory while building backend entry");
    }
    return entry;
}

fn Value* tensor_backend_set_bool_field(Interp* interp, Value* entry, char[] field_name, bool value) {
    if (entry == null || entry.tag == ERROR) return entry;
    Value* field_value = value ? make_symbol(interp, interp.sym_true) : make_symbol(interp, interp.sym_false);
    if (!hashmap_set_checked(entry.hashmap_val, make_symbol(interp, interp.symbols.intern(field_name)), field_value, interp)) {
        return raise_error(interp, "tensor-backends: out of memory while building backend entry");
    }
    return null;
}

fn Value* prim_tensor_backends(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 0) return primitive_arity_raise(interp, "tensor-backends", "no arguments", args.len);

    SymbolId cpu_sym = interp.symbols.intern("cpu");
    SymbolId cuda_sym = interp.symbols.intern("cuda");
    SymbolId cublas_sym = interp.symbols.intern("cublas");
    SymbolId vulkan_sym = interp.symbols.intern("vulkan");
    bool cuda_available = omni_tensor_backend_cuda_available() != 0;
    bool cuda_map_available = omni_tensor_backend_cuda_map_available() != 0;
    bool cuda_scientific_map_available = omni_tensor_backend_cuda_scientific_map_available() != 0;
    bool cuda_complex_map_available = omni_tensor_backend_cuda_complex_map_available() != 0;
    bool cuda_complex_matrix_available = omni_tensor_backend_cuda_complex_matrix_available() != 0;
    bool cuda_rounding_i64_available = omni_tensor_backend_cuda_rounding_i64_available() != 0;
    bool cublas_available = omni_tensor_backend_cublas_available() != 0;
    bool cublas_float32_available = omni_tensor_backend_cublas_float32_available() != 0;
    bool cublas_complex_available = omni_tensor_backend_cublas_complex_available() != 0;
    bool vulkan_available = omni_tensor_backend_vulkan_available() != 0;
    bool vulkan_float64_available = omni_tensor_backend_vulkan_float64_available() != 0;
    bool vulkan_rounding_i64_available = omni_tensor_backend_vulkan_rounding_i64_available() != 0;
    bool vulkan_float32_available = omni_tensor_backend_vulkan_float32_available() != 0;

    Value* vulkan_entry = tensor_backend_entry(
        interp,
        vulkan_sym,
        vulkan_available,
        vulkan_sym,
        vulkan_available ? INVALID_SYMBOL_ID : interp.symbols.intern("backend-unavailable")
    );
    if (vulkan_entry == null || vulkan_entry.tag == ERROR) return vulkan_entry;
    Value* err = tensor_backend_set_bool_field(interp, vulkan_entry, "float64", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "float32", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "complex128", vulkan_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "complex64", vulkan_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "elementwise-map-float64", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "elementwise-map-float32", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "elementwise-map-complex128", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "elementwise-map-complex64", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "contract-complex128", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "contract-complex64", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "matrix-structural-complex128", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "matrix-structural-complex64", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "matrix-numerical-complex128", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "matrix-numerical-complex64", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "scientific-map-float64", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "scientific-map-float32", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "rounding-big-integer", vulkan_rounding_i64_available);
    if (err != null) return err;
    Value* result = make_cons_or_error(interp, vulkan_entry, make_nil(interp), "tensor-backends: failed to allocate result list");
    if (result == null || result.tag == ERROR) return result;

    Value* cublas_entry = tensor_backend_entry(
        interp,
        cublas_sym,
        cublas_available,
        cuda_sym,
        cublas_available ? INVALID_SYMBOL_ID : interp.symbols.intern("backend-unavailable")
    );
    if (cublas_entry == null || cublas_entry.tag == ERROR) return cublas_entry;
    err = tensor_backend_set_bool_field(interp, cublas_entry, "float64", cublas_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cublas_entry, "float32", cublas_float32_available);
    if (err != null) return err;
    result = make_cons_or_error(interp, cublas_entry, result, "tensor-backends: failed to allocate result list");
    if (result == null || result.tag == ERROR) return result;

    Value* cuda_entry = tensor_backend_entry(
        interp,
        cuda_sym,
        cuda_available,
        cuda_sym,
        cuda_available ? INVALID_SYMBOL_ID : interp.symbols.intern("backend-unavailable")
    );
    if (cuda_entry == null || cuda_entry.tag == ERROR) return cuda_entry;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "float64", cuda_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "float32", cuda_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "complex128", cuda_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "complex64", cuda_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "elementwise-map-float64", cuda_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "elementwise-map-float32", cuda_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "elementwise-map-complex128", cuda_complex_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "elementwise-map-complex64", cuda_complex_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "contract-complex128", cublas_complex_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "contract-complex64", cublas_complex_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "matrix-structural-complex128", cuda_complex_matrix_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "matrix-structural-complex64", cuda_complex_matrix_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "matrix-numerical-complex128", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "matrix-numerical-complex64", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "scientific-map-float64", cuda_scientific_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "scientific-map-float32", cuda_scientific_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "rounding-big-integer", cuda_rounding_i64_available);
    if (err != null) return err;
    result = make_cons_or_error(interp, cuda_entry, result, "tensor-backends: failed to allocate result list");
    if (result == null || result.tag == ERROR) return result;

    Value* cpu_entry = tensor_backend_entry(interp, cpu_sym, true, cpu_sym, INVALID_SYMBOL_ID);
    if (cpu_entry == null || cpu_entry.tag == ERROR) return cpu_entry;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "elementwise-map-float64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "elementwise-map-float32", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "complex128", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "complex64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "elementwise-map-complex128", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "elementwise-map-complex64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "contract-complex128", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "contract-complex64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "matrix-structural-complex128", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "matrix-structural-complex64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "matrix-numerical-complex128", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "matrix-numerical-complex64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "scientific-map-float64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "scientific-map-float32", true);
    if (err != null) return err;
    result = make_cons_or_error(interp, cpu_entry, result, "tensor-backends: failed to allocate result list");
    return result;
}

fn Value* tensor_cuda_status_raise(Interp* interp, int status, char[] label) {
    switch (status) {
        case OMNI_TENSOR_CUDA_UNAVAILABLE:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unavailable", label, null);
        case OMNI_TENSOR_CUDA_ALLOCATION_FAILED:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-allocation-failed", label, null);
        case OMNI_TENSOR_CUDA_COPY_FAILED:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-copy-failed", label, null);
        case OMNI_TENSOR_CUDA_EXECUTION_FAILED:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-execution-failed", label, null);
        case OMNI_TENSOR_CUDA_DOMAIN_ERROR:
            return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", label, null);
        case OMNI_TENSOR_CUDA_INVALID_ARGUMENT:
            return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", label, null);
        case OMNI_TENSOR_CUDA_INVALID:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-invalid-state", label, null);
        default:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-failure", label, null);
    }
}

fn char[] tensor_vulkan_status_code_name(int status) {
    switch (status) {
        case OMNI_TENSOR_VULKAN_UNAVAILABLE:
            return "tensor/backend-unavailable";
        case OMNI_TENSOR_VULKAN_ALLOCATION_FAILED:
            return "tensor/backend-allocation-failed";
        case OMNI_TENSOR_VULKAN_COPY_FAILED:
            return "tensor/backend-copy-failed";
        case OMNI_TENSOR_VULKAN_UNSUPPORTED:
            return "tensor/backend-unsupported";
        case OMNI_TENSOR_VULKAN_EXECUTION_FAILED:
            return "tensor/backend-execution-failed";
        case OMNI_TENSOR_VULKAN_SINGULAR:
            return "tensor/singular-matrix";
        case OMNI_TENSOR_VULKAN_NO_CONVERGENCE:
            return "tensor/no-convergence";
        case OMNI_TENSOR_VULKAN_NOT_SYMMETRIC:
            return "tensor/not-symmetric";
        case OMNI_TENSOR_VULKAN_DOMAIN_ERROR:
            return "tensor/domain-error";
        case OMNI_TENSOR_VULKAN_INVALID_ARGUMENT:
            return "tensor/invalid-argument";
        case OMNI_TENSOR_VULKAN_INVALID:
            return "tensor/backend-invalid-state";
        default:
            return "tensor/backend-failure";
    }
}

fn Value* tensor_vulkan_status_raise(Interp* interp, int status, char[] label) {
    return raise_error_with_payload_names(interp, "tensor", tensor_vulkan_status_code_name(status), label, null);
}

fn Value* tensor_copy_cpu_to_cuda_value(Value* source_value, Interp* interp) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return tensor_type_raise(interp, "to-device: expected Tensor");
    TensorVal* source_meta = source_value.tensor_val;
    if (!tensor_has_valid_metadata(source_meta)) return tensor_runtime_raise(interp, "to-device: source tensor metadata is missing");
    if (source_meta.kind == TENSOR_PAYLOAD_VIEW || source_meta.is_view || !tensor_dense_row_major(source_meta)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: source Tensor views are not eligible for CUDA copy", null);
    }

    Value* realized = tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    if (!is_tensor(realized) || realized.tensor_val == null) {
        return tensor_runtime_raise(interp, "to-device: realized Tensor backing storage is missing");
    }

    TensorVal* source = realized.tensor_val;
    if (source.device == TENSOR_DEVICE_CUDA) {
        if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_CUDA)) {
            return tensor_runtime_raise(interp, "to-device: source tensor backing storage is missing");
        }
        return realized;
    }
    if (source.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: unsupported source device", null);
    }
    if (!tensor_has_valid_storage(source)) {
        return tensor_runtime_raise(interp, "to-device: source tensor backing storage is missing");
    }
    if (!tensor_concrete_zero_offset_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: source Tensor must be zero-offset dense row-major for CUDA copy", null);
    }
    if (!tensor_dtype_is_cuda_storage(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: CUDA currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    TensorVal* tensor = tensor_alloc_payload_base_checked(source.dtype, shape_slice, source.mutable, false, "to-device: failed to allocate CUDA tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "to-device: failed to allocate CUDA tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_CUDA;

    void* device_handle = null;
    int status = omni_tensor_backend_cuda_copy_to_device(source.data, source.byte_len, &device_handle);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_cuda_status_raise(interp, status, "to-device: CUDA backend unavailable");
    }
    tensor.device_handle = device_handle;
    tensor.device_finalizer = device_handle != null ? &tensor_cuda_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "to-device: failed to allocate CUDA tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_copy_cpu_to_vulkan_value(Value* source_value, Interp* interp) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return tensor_type_raise(interp, "to-device: expected Tensor");
    TensorVal* source_meta = source_value.tensor_val;
    if (!tensor_has_valid_metadata(source_meta)) return tensor_runtime_raise(interp, "to-device: source tensor metadata is missing");
    bool source_is_view = source_meta.kind == TENSOR_PAYLOAD_VIEW || source_meta.is_view;
    if (source_is_view && source_meta.device != TENSOR_DEVICE_CPU && source_meta.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: Vulkan copy currently supports CPU-backed or Vulkan transpose-view materialization", null);
    }
    if (!source_is_view && !tensor_dense_row_major(source_meta)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: source Tensor must be dense row-major for Vulkan copy", null);
    }

    Value* realized = tensor_realize_to_value(source_value, interp);
    if (realized == null || realized.tag == ERROR) return realized;
    if (!is_tensor(realized) || realized.tensor_val == null) {
        return tensor_runtime_raise(interp, "to-device: realized Tensor backing storage is missing");
    }

    TensorVal* source = realized.tensor_val;
    if (source.device == TENSOR_DEVICE_VULKAN) {
        if (!tensor_has_valid_device_storage(source, TENSOR_DEVICE_VULKAN)) {
            return tensor_runtime_raise(interp, "to-device: source tensor backing storage is missing");
        }
        return realized;
    }
    if (source.device != TENSOR_DEVICE_CPU) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: unsupported source device", null);
    }
    if (!tensor_has_valid_storage(source)) {
        return tensor_runtime_raise(interp, "to-device: source tensor backing storage is missing");
    }
    if (!tensor_concrete_zero_offset_dense_row_major(source)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: source Tensor must be zero-offset dense row-major for Vulkan copy", null);
    }
    if (!tensor_dtype_is_vulkan_storage(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    TensorVal* tensor = tensor_alloc_payload_base_checked(source.dtype, shape_slice, source.mutable, false, "to-device: failed to allocate Vulkan tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "to-device: failed to allocate Vulkan tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* device_handle = null;
    int status = omni_tensor_backend_vulkan_copy_to_device(source.data, source.byte_len, &device_handle);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "to-device: Vulkan device copy failed");
    }
    tensor.device_handle = device_handle;
    tensor.device_finalizer = device_handle != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "to-device: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_copy_device_to_cpu_value(Value* source_value, Interp* interp) {
    if (!is_tensor(source_value) || source_value.tensor_val == null) return tensor_type_raise(interp, "to-device: expected Tensor");
    TensorVal* source = source_value.tensor_val;
    if (source.device == TENSOR_DEVICE_CPU) {
        Value* realized = tensor_realize_to_value(source_value, interp);
        if (realized == null || realized.tag == ERROR) return realized;
        if (!is_tensor(realized) || realized.tensor_val == null) {
            return tensor_runtime_raise(interp, "to-device: realized Tensor backing storage is missing");
        }
        if (realized.tensor_val.device == TENSOR_DEVICE_CPU) return realized;
        return tensor_copy_device_to_cpu_value(realized, interp);
    }
    if (!tensor_has_valid_metadata(source)) {
        return tensor_runtime_raise(interp, "to-device: source tensor metadata is missing");
    }
    if (source.kind != TENSOR_PAYLOAD_CONCRETE) {
        Value* realized = tensor_realize_to_value(source_value, interp);
        if (realized == null || realized.tag == ERROR) return realized;
        if (!is_tensor(realized) || realized.tensor_val == null) {
            return tensor_runtime_raise(interp, "to-device: realized Tensor backing storage is missing");
        }
        if (realized.tensor_val.device == TENSOR_DEVICE_CPU) return realized;
        return tensor_copy_device_to_cpu_value(realized, interp);
    }
    if (source.device != TENSOR_DEVICE_CUDA && source.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: unsupported source device", null);
    }
    if (source.device == TENSOR_DEVICE_CUDA && !tensor_dtype_is_cuda_storage(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: CUDA device placement currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if (source.device == TENSOR_DEVICE_VULKAN && !tensor_dtype_is_vulkan_storage(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "to-device: Vulkan device placement currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
    }
    if (!tensor_has_valid_device_storage(source, source.device)) {
        return tensor_runtime_raise(interp, "to-device: source tensor backing storage is missing");
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "to-device: failed to allocate CPU tensor result");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    if (out_value.tag != TENSOR || out_value.tensor_val == null || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "to-device: CPU tensor result backing storage is missing");
    }

    if (source.device == TENSOR_DEVICE_CUDA) {
        int status = omni_tensor_backend_cuda_copy_to_host(source.device_handle, source.byte_len, out_value.tensor_val.data);
        if (status != OMNI_TENSOR_CUDA_SUCCESS) {
            return tensor_cuda_status_raise(interp, status, "to-device: CUDA host copy failed");
        }
        return out_value;
    }

    int status = omni_tensor_backend_vulkan_copy_to_host(source.device_handle, source.byte_len, out_value.tensor_val.data);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return tensor_vulkan_status_raise(interp, status, "to-device: Vulkan host copy failed");
    }
    return out_value;
}

fn Value* tensor_contract_try_vulkan_value(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    usz[] result_shape,
    Interp* interp
) {
    if (left == null || right == null) return null;
    bool touches_vulkan = left.device == TENSOR_DEVICE_VULKAN || right.device == TENSOR_DEVICE_VULKAN;
    if (!touches_vulkan) return null;
    if (left.device != TENSOR_DEVICE_VULKAN || right.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: Vulkan operands must both be Vulkan-placed", null);
    }
    if ((!tensor_dtype_is_vulkan_real(left.dtype) && !tensor_dtype_is_fixed_complex(left.dtype)) || left.dtype != right.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: Vulkan currently supports matching Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (axis_count > 0 && (left_axes == null || right_axes == null)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: Vulkan currently supports matching Float64 or Float32 explicit-axis contraction", null);
    }
    if (!tensor_has_valid_device_storage(left, TENSOR_DEVICE_VULKAN) || !tensor_has_valid_device_storage(right, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "contract: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(left) || !tensor_dense_row_major(right)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: Vulkan currently supports dense row-major matching Float64 or Float32 tensors", null);
    }

    usz expected_rank = (left.rank - axis_count) + (right.rank - axis_count);
    if (result_shape.len != expected_rank) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: Vulkan result shape mismatch", null);
    }
    usz result_axis = 0;
    for (usz axis = 0; axis < left.rank; axis++) {
        if (tensor_axis_is_listed(axis, left_axes, axis_count)) continue;
        if (result_axis >= result_shape.len || result_shape[result_axis] != left.shape[axis]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: Vulkan result shape mismatch", null);
        }
        result_axis++;
    }
    for (usz axis = 0; axis < right.rank; axis++) {
        if (tensor_axis_is_listed(axis, right_axes, axis_count)) continue;
        if (result_axis >= result_shape.len || result_shape[result_axis] != right.shape[axis]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: Vulkan result shape mismatch", null);
        }
        result_axis++;
    }

    bool rank1_dot = left.dtype == TENSOR_DTYPE_DOUBLE && axis_count == 1 && tensor_rank1_contiguous(left) && tensor_rank1_contiguous(right);
    if (rank1_dot) {
        if (left_axes[0] != 0 || right_axes[0] != 0 || result_shape.len != 0) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: Vulkan currently supports rank-1 Float64 dot", null);
        }
        if (left.shape[0] != right.shape[0]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: contracted dimensions must match", null);
        }
        if (left.shape[0] > 0) {
            TensorVal* tensor = tensor_alloc_payload_base_checked(TENSOR_DTYPE_DOUBLE, result_shape, false, false, "contract: failed to allocate Vulkan tensor metadata");
            if (tensor == null) return tensor_runtime_raise(interp, "contract: failed to allocate Vulkan tensor metadata");
            tensor.kind = TENSOR_PAYLOAD_CONCRETE;
            tensor.device = TENSOR_DEVICE_VULKAN;

            void* out_device = null;
            int status = omni_tensor_backend_vulkan_contract_dot_f64(
                left.device_handle,
                right.device_handle,
                left.shape[0],
                &out_device
            );
            if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
                tensor_free_payload(tensor);
                return tensor_vulkan_status_raise(interp, status, "contract: Vulkan rank-1 dot kernel failed");
            }
            tensor.device_handle = out_device;
            tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

            Value* result = interp.alloc_value();
            if (result == null) {
                tensor_free_payload(tensor);
                return tensor_runtime_raise(interp, "contract: failed to allocate Vulkan tensor value");
            }
            result.tag = TENSOR;
            result.tensor_val = tensor;
            main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
            return result;
        }
    }

    TensorVal* tensor = tensor_alloc_payload_base_checked(left.dtype, result_shape, false, false, "contract: failed to allocate Vulkan tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "contract: failed to allocate Vulkan tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (left.dtype == TENSOR_DTYPE_DOUBLE) {
        status = omni_tensor_backend_vulkan_contract_f64(
            left.device_handle,
            right.device_handle,
            left.rank,
            right.rank,
            axis_count,
            left_axes,
            right_axes,
            tensor.element_count,
            result_shape.len,
            left.shape,
            left.strides,
            right.shape,
            right.strides,
            tensor.shape,
            tensor.strides,
            &out_device
        );
    } else if (left.dtype == TENSOR_DTYPE_FLOAT32) {
        status = omni_tensor_backend_vulkan_contract_f32(
            left.device_handle,
            right.device_handle,
            left.rank,
            right.rank,
            axis_count,
            left_axes,
            right_axes,
            tensor.element_count,
            result_shape.len,
            left.shape,
            left.strides,
            right.shape,
            right.strides,
            tensor.shape,
            tensor.strides,
            &out_device
        );
    } else if (left.dtype == TENSOR_DTYPE_COMPLEX128) {
        status = omni_tensor_backend_vulkan_contract_complex128(
            left.device_handle,
            right.device_handle,
            left.rank,
            right.rank,
            axis_count,
            left_axes,
            right_axes,
            tensor.element_count,
            result_shape.len,
            left.shape,
            left.strides,
            right.shape,
            right.strides,
            tensor.shape,
            tensor.strides,
            &out_device
        );
    } else if (left.dtype == TENSOR_DTYPE_COMPLEX64) {
        status = omni_tensor_backend_vulkan_contract_complex64(
            left.device_handle,
            right.device_handle,
            left.rank,
            right.rank,
            axis_count,
            left_axes,
            right_axes,
            tensor.element_count,
            result_shape.len,
            left.shape,
            left.strides,
            right.shape,
            right.strides,
            tensor.shape,
            tensor.strides,
            &out_device
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "contract: Vulkan contract kernel failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "contract: failed to allocate Vulkan tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* tensor_contract_try_cuda_value(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    usz[] result_shape,
    Interp* interp
) {
    if (left == null || right == null) return null;
    bool touches_cuda = left.device == TENSOR_DEVICE_CUDA || right.device == TENSOR_DEVICE_CUDA;
    if (!touches_cuda) return null;
    if (left.device != TENSOR_DEVICE_CUDA || right.device != TENSOR_DEVICE_CUDA) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA operands must both be CUDA-placed", null);
    }
    if (axis_count != 1 || left_axes == null || right_axes == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports rank-2 Float64 or Float32 single-axis contraction", null);
    }
    usz left_axis = left_axes[0];
    usz right_axis = right_axes[0];
    if (left_axis > 1 || right_axis > 1) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports rank-2 Float64 or Float32 single-axis contraction", null);
    }
    if ((!tensor_dtype_is_cuda_real(left.dtype) && !tensor_dtype_is_fixed_complex(left.dtype)) || left.dtype != right.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports matching Float64, Float32, Complex128, or Complex64 Tensor storage", null);
    }
    if (tensor_dtype_is_fixed_complex(left.dtype) && omni_tensor_backend_cublas_complex_available() == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA fixed-width complex contract helper is unavailable", null);
    }

    int cuda_contract_kind = 0;
    usz m = 0;
    usz n = 0;
    usz k = 0;
    int trans_left = 0;
    int trans_right = 0;
    usz matrix_rows = 0;
    usz matrix_cols = 0;
    usz matrix_leading_dim = 0;
    int trans_matrix = 0;
    TensorVal* matrix_tensor = null;
    TensorVal* vector_tensor = null;
    bool cuda_contract_zero_fill = false;

    if (tensor_rank1_contiguous(left) && tensor_rank1_contiguous(right)) {
        if (left_axis != 0 || right_axis != 0) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports rank-1/rank-1 Float64 or Float32 single-axis contraction", null);
        }
        usz contracted = left.shape[0];
        if (right.shape[0] != contracted || result_shape.len != 0) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: CUDA result shape mismatch", null);
        }
        cuda_contract_zero_fill = contracted == 0;
        matrix_rows = 1;
        matrix_cols = contracted;
        matrix_leading_dim = contracted;
        matrix_tensor = left;
        vector_tensor = right;
        cuda_contract_kind = 2;
    } else if (tensor_rank2_row_major(left) && tensor_rank2_row_major(right)) {
        usz left_free_axis = left_axis == 0 ? 1 : 0;
        usz right_free_axis = right_axis == 0 ? 1 : 0;
        m = left.shape[left_free_axis];
        k = left.shape[left_axis];
        n = right.shape[right_free_axis];
        if (right.shape[right_axis] != k || result_shape.len != 2 || result_shape[0] != m || result_shape[1] != n) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: CUDA result shape mismatch", null);
        }
        cuda_contract_zero_fill = k == 0;
        trans_left = left_axis == 0 ? 1 : 0;
        trans_right = right_axis == 1 ? 1 : 0;
        cuda_contract_kind = 1;
    } else if (tensor_rank2_row_major(left) && tensor_rank1_contiguous(right)) {
        if (left_axis > 1 || right_axis != 0) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports rank-2/rank-1 Float64 or Float32 single-axis contraction", null);
        }
        matrix_rows = left.shape[0];
        matrix_cols = left.shape[1];
        usz contracted = left.shape[left_axis];
        usz result_len = left.shape[left_axis == 0 ? 1 : 0];
        if (right.shape[0] != contracted || result_shape.len != 1 || result_shape[0] != result_len) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: CUDA result shape mismatch", null);
        }
        cuda_contract_zero_fill = contracted == 0;
        trans_matrix = left_axis == 0 ? 1 : 0;
        matrix_tensor = left;
        vector_tensor = right;
        matrix_leading_dim = left.strides[0];
        cuda_contract_kind = 2;
    } else if (tensor_rank1_contiguous(left) && tensor_rank2_row_major(right)) {
        if (left_axis != 0 || right_axis > 1) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports rank-1/rank-2 Float64 or Float32 single-axis contraction", null);
        }
        matrix_rows = right.shape[0];
        matrix_cols = right.shape[1];
        usz contracted = right.shape[right_axis];
        usz result_len = right.shape[right_axis == 0 ? 1 : 0];
        if (left.shape[0] != contracted || result_shape.len != 1 || result_shape[0] != result_len) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "contract: CUDA result shape mismatch", null);
        }
        cuda_contract_zero_fill = contracted == 0;
        trans_matrix = right_axis == 0 ? 1 : 0;
        matrix_tensor = right;
        vector_tensor = left;
        matrix_leading_dim = right.strides[0];
        cuda_contract_kind = 2;
    } else {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "contract: CUDA currently supports dense rank-1/rank-1, rank-2/rank-2, rank-2/rank-1, and rank-1/rank-2 Float64 or Float32 tensors", null);
    }

    TensorVal* tensor = tensor_alloc_payload_base_checked(left.dtype, result_shape, false, false, "contract: failed to allocate CUDA tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "contract: failed to allocate CUDA tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_CUDA;

    void* out_device = null;
    int status = omni_tensor_backend_cuda_alloc(tensor.byte_len, &out_device);
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_cuda_status_raise(interp, status, "contract: CUDA output allocation failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_cuda_device_finalizer : null;

    if (tensor.element_count == 0) {
        status = OMNI_TENSOR_CUDA_SUCCESS;
    } else if (cuda_contract_zero_fill) {
        if (left.dtype == TENSOR_DTYPE_DOUBLE) {
            status = omni_tensor_backend_cuda_fill_f64(out_device, tensor.element_count, 0.0);
        } else if (left.dtype == TENSOR_DTYPE_FLOAT32) {
            status = omni_tensor_backend_cuda_fill_f32(out_device, tensor.element_count, 0.0f);
        } else if (left.dtype == TENSOR_DTYPE_COMPLEX128) {
            status = omni_tensor_backend_cuda_fill_f64(out_device, tensor.element_count * 2, 0.0);
        } else {
            status = omni_tensor_backend_cuda_fill_f32(out_device, tensor.element_count * 2, 0.0f);
        }
    } else if (left.dtype == TENSOR_DTYPE_DOUBLE) {
        if (cuda_contract_kind == 1) {
            status = omni_tensor_backend_cublas_dgemm_rowmajor(
                m,
                n,
                k,
                trans_left,
                trans_right,
                left.device_handle,
                left.strides[0],
                right.device_handle,
                right.strides[0],
                out_device,
                tensor.strides[0]
            );
        } else {
            status = omni_tensor_backend_cublas_dgemv_rowmajor(
                matrix_rows,
                matrix_cols,
                trans_matrix,
                matrix_tensor.device_handle,
                matrix_leading_dim,
                vector_tensor.device_handle,
                out_device
            );
        }
    } else if (left.dtype == TENSOR_DTYPE_FLOAT32) {
        if (cuda_contract_kind == 1) {
            status = omni_tensor_backend_cublas_sgemm_rowmajor(
                m,
                n,
                k,
                trans_left,
                trans_right,
                left.device_handle,
                left.strides[0],
                right.device_handle,
                right.strides[0],
                out_device,
                tensor.strides[0]
            );
        } else {
            status = omni_tensor_backend_cublas_sgemv_rowmajor(
                matrix_rows,
                matrix_cols,
                trans_matrix,
                matrix_tensor.device_handle,
                matrix_leading_dim,
                vector_tensor.device_handle,
                out_device
            );
        }
    } else if (left.dtype == TENSOR_DTYPE_COMPLEX128) {
        if (cuda_contract_kind == 1) {
            status = omni_tensor_backend_cublas_zgemm_rowmajor(
                m,
                n,
                k,
                trans_left,
                trans_right,
                left.device_handle,
                left.strides[0],
                right.device_handle,
                right.strides[0],
                out_device,
                tensor.strides[0]
            );
        } else {
            status = omni_tensor_backend_cublas_zgemv_rowmajor(
                matrix_rows,
                matrix_cols,
                trans_matrix,
                matrix_tensor.device_handle,
                matrix_leading_dim,
                vector_tensor.device_handle,
                out_device
            );
        }
    } else {
        if (cuda_contract_kind == 1) {
            status = omni_tensor_backend_cublas_cgemm_rowmajor(
                m,
                n,
                k,
                trans_left,
                trans_right,
                left.device_handle,
                left.strides[0],
                right.device_handle,
                right.strides[0],
                out_device,
                tensor.strides[0]
            );
        } else {
            status = omni_tensor_backend_cublas_cgemv_rowmajor(
                matrix_rows,
                matrix_cols,
                trans_matrix,
                matrix_tensor.device_handle,
                matrix_leading_dim,
                vector_tensor.device_handle,
                out_device
            );
        }
    }
    if (status != OMNI_TENSOR_CUDA_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_cuda_status_raise(interp, status, "contract: cuBLAS execution failed");
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "contract: failed to allocate CUDA tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* prim_to_device(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return primitive_arity_raise(interp, "to-device", "2 arguments: tensor device", args.len);
    if (!is_tensor(args[0])) return raise_error(interp, "to-device: expected Tensor");
    if (!is_symbol(args[1])) return raise_error(interp, "to-device: device target must be a symbol");

    SymbolId cpu_sym = interp.symbols.intern("cpu");
    SymbolId cuda_sym = interp.symbols.intern("cuda");
    SymbolId vulkan_sym = interp.symbols.intern("vulkan");
    if (args[1].sym_val == cpu_sym) {
        return tensor_copy_device_to_cpu_value(args[0], interp);
    }
    if (args[1].sym_val == cuda_sym) {
        return tensor_copy_cpu_to_cuda_value(args[0], interp);
    }
    if (args[1].sym_val == vulkan_sym) {
        return tensor_copy_cpu_to_vulkan_value(args[0], interp);
    }
    return raise_error_with_payload_names(
        interp,
        "tensor",
        "tensor/backend-unsupported",
        "to-device: unsupported device target",
        null
    );
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
