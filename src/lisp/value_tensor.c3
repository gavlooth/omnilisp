module lisp;

import std::core::mem;
import main;

bool g_value_force_make_tensor_struct_alloc_fail = false;
bool g_value_force_make_tensor_shape_alloc_fail = false;
bool g_value_force_make_tensor_strides_alloc_fail = false;
bool g_value_force_make_tensor_data_alloc_fail = false;
int g_tensor_force_expr_edge_copy_fail_nth = 0;
int g_tensor_force_expr_edge_escape_fail_nth = 0;

fn bool tensor_should_force_expr_edge_fail(int* counter) @inline {
    if (counter == null || *counter <= 0) return false;
    int next = *counter - 1;
    *counter = next;
    return next == 0;
}

fn bool tensor_checked_mul_usz(usz a, usz b, usz* out) @inline {
    if (out == null) return false;
    if (b != 0 && a > usz.max / b) return false;
    *out = a * b;
    return true;
}

fn usz tensor_dtype_size(TensorDType dtype) @inline {
    switch (dtype) {
        case TENSOR_DTYPE_DOUBLE:
            return double.sizeof;
        case TENSOR_DTYPE_FLOAT32:
            return float.sizeof;
        case TENSOR_DTYPE_COMPLEX128:
            return Complex128Val.sizeof;
        case TENSOR_DTYPE_COMPLEX64:
            return Complex64Val.sizeof;
        case TENSOR_DTYPE_BIG_INTEGER:
            return void*.sizeof;
        case TENSOR_DTYPE_BIG_FLOAT:
            return void*.sizeof;
        case TENSOR_DTYPE_BIG_COMPLEX:
            return void*.sizeof;
    }
}

fn char[] tensor_dtype_name(TensorDType dtype) @inline {
    switch (dtype) {
        case TENSOR_DTYPE_DOUBLE:
            return "Float64";
        case TENSOR_DTYPE_FLOAT32:
            return "Float32";
        case TENSOR_DTYPE_COMPLEX128:
            return "Complex128";
        case TENSOR_DTYPE_COMPLEX64:
            return "Complex64";
        case TENSOR_DTYPE_BIG_INTEGER:
            return "BigInteger";
        case TENSOR_DTYPE_BIG_FLOAT:
            return "BigFloat";
        case TENSOR_DTYPE_BIG_COMPLEX:
            return "BigComplex";
    }
}

fn SymbolId tensor_dtype_symbol(TensorDType dtype, Interp* interp) @inline {
    switch (dtype) {
        case TENSOR_DTYPE_DOUBLE:
            return interp.sym_Double;
        case TENSOR_DTYPE_FLOAT32:
            return interp.sym_Float32;
        case TENSOR_DTYPE_COMPLEX128:
            return interp.sym_Complex128;
        case TENSOR_DTYPE_COMPLEX64:
            return interp.sym_Complex64;
        case TENSOR_DTYPE_BIG_INTEGER:
            return interp.sym_BigInteger;
        case TENSOR_DTYPE_BIG_FLOAT:
            return interp.sym_BigFloat;
        case TENSOR_DTYPE_BIG_COMPLEX:
            return interp.sym_BigComplex;
    }
}

fn void tensor_big_integer_clear_data(TensorVal* tensor) {
    if (tensor == null || tensor.dtype != TENSOR_DTYPE_BIG_INTEGER || tensor.data == null) return;
    void** values = (void**)tensor.data;
    for (usz i = 0; i < tensor.element_count; i++) {
        if (values[i] != null) {
            omni_big_integer_free(values[i]);
            values[i] = null;
        }
    }
}

fn void tensor_big_float_clear_data(TensorVal* tensor) {
    if (tensor == null || tensor.dtype != TENSOR_DTYPE_BIG_FLOAT || tensor.data == null) return;
    void** values = (void**)tensor.data;
    for (usz i = 0; i < tensor.element_count; i++) {
        if (values[i] != null) {
            omni_big_float_free(values[i]);
            values[i] = null;
        }
    }
}

fn void tensor_big_complex_clear_data(TensorVal* tensor) {
    if (tensor == null || tensor.dtype != TENSOR_DTYPE_BIG_COMPLEX || tensor.data == null) return;
    void** values = (void**)tensor.data;
    for (usz i = 0; i < tensor.element_count; i++) {
        if (values[i] != null) {
            omni_big_complex_free(values[i]);
            values[i] = null;
        }
    }
}

fn void tensor_map_big_integer_scalar_clear(TensorVal* tensor) {
    if (tensor == null || tensor.kind != TENSOR_PAYLOAD_MAP || tensor.dtype != TENSOR_DTYPE_BIG_INTEGER) return;
    if (tensor.map_left_big_integer_scalar != null) {
        omni_big_integer_free(tensor.map_left_big_integer_scalar);
        tensor.map_left_big_integer_scalar = null;
    }
    if (tensor.map_right_big_integer_scalar != null) {
        omni_big_integer_free(tensor.map_right_big_integer_scalar);
        tensor.map_right_big_integer_scalar = null;
    }
}

fn void tensor_map_big_float_scalar_clear(TensorVal* tensor) {
    if (tensor == null || tensor.kind != TENSOR_PAYLOAD_MAP || tensor.dtype != TENSOR_DTYPE_BIG_FLOAT) return;
    if (tensor.map_left_big_float_scalar != null) {
        omni_big_float_free(tensor.map_left_big_float_scalar);
        tensor.map_left_big_float_scalar = null;
    }
    if (tensor.map_right_big_float_scalar != null) {
        omni_big_float_free(tensor.map_right_big_float_scalar);
        tensor.map_right_big_float_scalar = null;
    }
}

fn void tensor_map_big_complex_scalar_clear(TensorVal* tensor) {
    if (tensor == null || tensor.kind != TENSOR_PAYLOAD_MAP || tensor.dtype != TENSOR_DTYPE_BIG_COMPLEX) return;
    if (tensor.map_left_big_complex_scalar != null) {
        omni_big_complex_free(tensor.map_left_big_complex_scalar);
        tensor.map_left_big_complex_scalar = null;
    }
    if (tensor.map_right_big_complex_scalar != null) {
        omni_big_complex_free(tensor.map_right_big_complex_scalar);
        tensor.map_right_big_complex_scalar = null;
    }
}

fn void tensor_free_payload(TensorVal* tensor) {
    if (tensor == null) return;
    tensor_map_big_integer_scalar_clear(tensor);
    tensor_map_big_float_scalar_clear(tensor);
    tensor_map_big_complex_scalar_clear(tensor);
    if (tensor.device_handle != null && !tensor.is_view) {
        if (tensor.device_finalizer != null) tensor.device_finalizer(tensor.device_handle);
        tensor.device_handle = null;
        tensor.device_finalizer = null;
    }
    if (tensor.shape != null) {
        mem::free(tensor.shape);
        tensor.shape = null;
    }
    if (tensor.strides != null) {
        mem::free(tensor.strides);
        tensor.strides = null;
    }
    if (!tensor.is_view && tensor.data != null && tensor.device == TENSOR_DEVICE_CPU) {
        if (tensor.kind == TENSOR_PAYLOAD_CONCRETE) {
            tensor_big_integer_clear_data(tensor);
            tensor_big_float_clear_data(tensor);
            tensor_big_complex_clear_data(tensor);
        }
        mem::free(tensor.data);
        tensor.data = null;
    }
    if (tensor.contract_left_axes != null) {
        mem::free(tensor.contract_left_axes);
        tensor.contract_left_axes = null;
    }
    if (tensor.contract_right_axes != null) {
        mem::free(tensor.contract_right_axes);
        tensor.contract_right_axes = null;
    }
    mem::free(tensor);
}

fn bool tensor_shape_product(usz[] shape, usz* out_count) {
    if (out_count == null) return false;
    usz count = 1;
    for (usz i = 0; i < shape.len; i++) {
        if (!tensor_checked_mul_usz(count, shape[i], &count)) return false;
    }
    *out_count = count;
    return true;
}

fn bool tensor_fill_row_major_strides(usz[] shape, usz* strides) {
    if (shape.len > 0 && strides == null) return false;
    usz stride = 1;
    for (usz i = shape.len; i > 0; i--) {
        usz idx = i - 1;
        strides[idx] = stride;
        if (!tensor_checked_mul_usz(stride, shape[idx], &stride)) return false;
    }
    return true;
}

fn TensorVal* tensor_alloc_payload_base_checked(
    TensorDType dtype,
    usz[] shape,
    bool mutable,
    bool allocate_data,
    char[] oom_message
) {
    (void)oom_message;
    TensorVal* tensor = g_value_force_make_tensor_struct_alloc_fail
        ? null
        : (TensorVal*)mem::malloc(TensorVal.sizeof);
    if (tensor == null) return null;
    *tensor = {};

    usz element_count = 0;
    if (!tensor_shape_product(shape, &element_count)) {
        tensor_free_payload(tensor);
        return null;
    }
    usz dtype_size = tensor_dtype_size(dtype);
    usz byte_len = 0;
    if (!tensor_checked_mul_usz(element_count, dtype_size, &byte_len)) {
        tensor_free_payload(tensor);
        return null;
    }

    tensor.dtype = dtype;
    tensor.kind = allocate_data ? TENSOR_PAYLOAD_CONCRETE : TENSOR_PAYLOAD_MAP;
    tensor.device = TENSOR_DEVICE_CPU;
    tensor.rank = shape.len;
    tensor.element_count = element_count;
    tensor.byte_len = byte_len;
    tensor.storage_offset = 0;
    tensor.storage_element_count = element_count;
    tensor.storage_byte_len = byte_len;
    tensor.mutable = mutable;
    tensor.is_view = false;

    if (shape.len > 0) {
        if (shape.len > usz.max / usz.sizeof) {
            tensor_free_payload(tensor);
            return null;
        }
        tensor.shape = g_value_force_make_tensor_shape_alloc_fail
            ? null
            : (usz*)mem::malloc(usz.sizeof * shape.len);
        if (tensor.shape == null) {
            tensor_free_payload(tensor);
            return null;
        }
        tensor.strides = g_value_force_make_tensor_strides_alloc_fail
            ? null
            : (usz*)mem::malloc(usz.sizeof * shape.len);
        if (tensor.strides == null) {
            tensor_free_payload(tensor);
            return null;
        }
        for (usz i = 0; i < shape.len; i++) {
            tensor.shape[i] = shape[i];
        }
        if (!tensor_fill_row_major_strides(shape, tensor.strides)) {
            tensor_free_payload(tensor);
            return null;
        }
    }

    if (allocate_data && byte_len > 0) {
        tensor.data = g_value_force_make_tensor_data_alloc_fail
            ? null
            : mem::malloc(byte_len);
        if (tensor.data == null) {
            tensor_free_payload(tensor);
            return null;
        }
        switch (dtype) {
            case TENSOR_DTYPE_DOUBLE: {
                double* values = (double*)tensor.data;
                for (usz i = 0; i < element_count; i++) values[i] = 0.0;
            }
            case TENSOR_DTYPE_FLOAT32: {
                float* values = (float*)tensor.data;
                for (usz i = 0; i < element_count; i++) values[i] = 0.0f;
            }
            case TENSOR_DTYPE_COMPLEX128: {
                Complex128Val* values = (Complex128Val*)tensor.data;
                for (usz i = 0; i < element_count; i++) {
                    values[i].real = 0.0;
                    values[i].imag = 0.0;
                }
            }
            case TENSOR_DTYPE_COMPLEX64: {
                Complex64Val* values = (Complex64Val*)tensor.data;
                for (usz i = 0; i < element_count; i++) {
                    values[i].real = 0.0f;
                    values[i].imag = 0.0f;
                }
            }
            case TENSOR_DTYPE_BIG_INTEGER:
            case TENSOR_DTYPE_BIG_FLOAT:
            case TENSOR_DTYPE_BIG_COMPLEX: {
                void** values = (void**)tensor.data;
                for (usz i = 0; i < element_count; i++) values[i] = null;
            }
        }
    }
    return tensor;
}

fn bool tensor_clone_big_integer_data(TensorVal* dst, TensorVal* src) {
    if (dst == null || src == null || dst.dtype != TENSOR_DTYPE_BIG_INTEGER || src.dtype != TENSOR_DTYPE_BIG_INTEGER) return false;
    if (src.element_count == 0) return true;
    if (dst.data == null || src.data == null) return false;
    void** dst_values = (void**)dst.data;
    void** src_values = (void**)src.data;
    for (usz i = 0; i < src.element_count; i++) {
        if (src_values[i] == null) return false;
        dst_values[i] = omni_big_integer_clone(src_values[i]);
        if (dst_values[i] == null) return false;
    }
    return true;
}

fn bool tensor_clone_big_float_data(TensorVal* dst, TensorVal* src) {
    if (dst == null || src == null || dst.dtype != TENSOR_DTYPE_BIG_FLOAT || src.dtype != TENSOR_DTYPE_BIG_FLOAT) return false;
    if (src.element_count == 0) return true;
    if (dst.data == null || src.data == null) return false;
    void** dst_values = (void**)dst.data;
    void** src_values = (void**)src.data;
    for (usz i = 0; i < src.element_count; i++) {
        if (src_values[i] == null) return false;
        dst_values[i] = omni_big_float_clone(src_values[i]);
        if (dst_values[i] == null) return false;
    }
    return true;
}

fn bool tensor_clone_big_complex_data(TensorVal* dst, TensorVal* src) {
    if (dst == null || src == null || dst.dtype != TENSOR_DTYPE_BIG_COMPLEX || src.dtype != TENSOR_DTYPE_BIG_COMPLEX) return false;
    if (src.element_count == 0) return true;
    if (dst.data == null || src.data == null) return false;
    void** dst_values = (void**)dst.data;
    void** src_values = (void**)src.data;
    for (usz i = 0; i < src.element_count; i++) {
        if (src_values[i] == null) return false;
        dst_values[i] = omni_big_complex_clone(src_values[i]);
        if (dst_values[i] == null) return false;
    }
    return true;
}

fn bool tensor_copy_concrete_storage(TensorVal* dest, TensorVal* source) {
    if (dest == null || source == null || dest.dtype != source.dtype) return false;
    if (dest.element_count != source.element_count || dest.byte_len != source.byte_len) return false;
    if (!tensor_dense_row_major(dest) || !tensor_dense_row_major(source)) return false;
    if (source.byte_len == 0) return true;
    if (dest.data == null || source.data == null) return false;
    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE:
        case TENSOR_DTYPE_FLOAT32:
        case TENSOR_DTYPE_COMPLEX128:
        case TENSOR_DTYPE_COMPLEX64:
            mem::copy(dest.data, source.data, source.byte_len);
            return true;
        case TENSOR_DTYPE_BIG_INTEGER: {
            void** staged = (void**)mem::malloc(source.byte_len);
            if (staged == null) return false;
            for (usz i = 0; i < source.element_count; i++) staged[i] = null;
            void** source_values = (void**)source.data;
            bool ok = true;
            for (usz i = 0; i < source.element_count; i++) {
                if (source_values[i] == null) {
                    ok = false;
                    break;
                }
                staged[i] = omni_big_integer_clone(source_values[i]);
                if (staged[i] == null) {
                    ok = false;
                    break;
                }
            }
            if (!ok) {
                for (usz i = 0; i < source.element_count; i++) {
                    if (staged[i] != null) omni_big_integer_free(staged[i]);
                }
                mem::free(staged);
                return false;
            }
            tensor_big_integer_clear_data(dest);
            void** dest_values = (void**)dest.data;
            for (usz i = 0; i < source.element_count; i++) dest_values[i] = staged[i];
            mem::free(staged);
            return true;
        }
        case TENSOR_DTYPE_BIG_FLOAT: {
            void** staged = (void**)mem::malloc(source.byte_len);
            if (staged == null) return false;
            for (usz i = 0; i < source.element_count; i++) staged[i] = null;
            void** source_values = (void**)source.data;
            bool ok = true;
            for (usz i = 0; i < source.element_count; i++) {
                if (source_values[i] == null) {
                    ok = false;
                    break;
                }
                staged[i] = omni_big_float_clone(source_values[i]);
                if (staged[i] == null) {
                    ok = false;
                    break;
                }
            }
            if (!ok) {
                for (usz i = 0; i < source.element_count; i++) {
                    if (staged[i] != null) omni_big_float_free(staged[i]);
                }
                mem::free(staged);
                return false;
            }
            tensor_big_float_clear_data(dest);
            void** dest_values = (void**)dest.data;
            for (usz i = 0; i < source.element_count; i++) dest_values[i] = staged[i];
            mem::free(staged);
            return true;
        }
        case TENSOR_DTYPE_BIG_COMPLEX: {
            void** staged = (void**)mem::malloc(source.byte_len);
            if (staged == null) return false;
            for (usz i = 0; i < source.element_count; i++) staged[i] = null;
            void** source_values = (void**)source.data;
            bool ok = true;
            for (usz i = 0; i < source.element_count; i++) {
                if (source_values[i] == null) {
                    ok = false;
                    break;
                }
                staged[i] = omni_big_complex_clone(source_values[i]);
                if (staged[i] == null) {
                    ok = false;
                    break;
                }
            }
            if (!ok) {
                for (usz i = 0; i < source.element_count; i++) {
                    if (staged[i] != null) omni_big_complex_free(staged[i]);
                }
                mem::free(staged);
                return false;
            }
            tensor_big_complex_clear_data(dest);
            void** dest_values = (void**)dest.data;
            for (usz i = 0; i < source.element_count; i++) dest_values[i] = staged[i];
            mem::free(staged);
            return true;
        }
    }
}

fn TensorVal* tensor_alloc_payload_checked(
    TensorDType dtype,
    usz[] shape,
    bool mutable,
    char[] oom_message
) {
    return tensor_alloc_payload_base_checked(dtype, shape, mutable, true, oom_message);
}

fn TensorVal* tensor_alloc_expression_payload_checked(
    TensorDType dtype,
    usz[] shape,
    TensorPayloadKind kind,
    char[] oom_message
) {
    TensorVal* tensor = tensor_alloc_payload_base_checked(dtype, shape, false, false, oom_message);
    if (tensor != null) {
        tensor.kind = kind;
        tensor.storage_offset = 0;
        tensor.storage_element_count = 0;
        tensor.storage_byte_len = 0;
    }
    return tensor;
}
