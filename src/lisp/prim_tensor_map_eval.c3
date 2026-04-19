module lisp;

import std::core::mem;
import main;

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
