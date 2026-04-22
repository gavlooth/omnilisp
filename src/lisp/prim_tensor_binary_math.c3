module lisp;

import std::core::mem;
import main;

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
    if (left_tensor != null && left_tensor.dtype != TENSOR_DTYPE_BIG_INTEGER) return tensor_raise(interp, "tensor/invalid-argument", type_message);
    if (right_tensor != null && right_tensor.dtype != TENSOR_DTYPE_BIG_INTEGER) return tensor_raise(interp, "tensor/invalid-argument", type_message);
    if (left_tensor == null && !is_exact_integer_value(left_value)) return tensor_raise(interp, "tensor/invalid-argument", type_message);
    if (right_tensor == null && !is_exact_integer_value(right_value)) return tensor_raise(interp, "tensor/invalid-argument", type_message);

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
        if (result == null) return tensor_raise(interp, "tensor/invalid-argument", oom_message);
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
        return tensor_raise(interp, "tensor/invalid-argument", "pow: fixed-width complex numbers are not supported");
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
                    return tensor_raise(interp, "tensor/invalid-argument", "pow: tensor element out of Float64 range");
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
                if (cloned == null) return tensor_raise(interp, "tensor/out-of-memory", "pow: failed to allocate BigFloat tensor element");
                if (out[i] != null) omni_big_float_free(out[i]);
                out[i] = cloned;
            }
            case TENSOR_DTYPE_BIG_COMPLEX: {
                Value* result = big_complex_binary_math_values(interp, left_arg, right_arg, BIG_COMPLEX_MATH_POW, "pow: BigComplex tensor evaluation error");
                if (result == null || result.tag == ERROR) return result;
                void** out = (void**)out_tensor.data;
                void* cloned = omni_big_complex_clone(result.big_complex_val);
                if (cloned == null) return tensor_raise(interp, "tensor/out-of-memory", "pow: failed to allocate BigComplex tensor element");
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
        return tensor_raise(interp, "tensor/invalid-argument", "atan2: complex numbers are not supported");
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
                    return tensor_raise(interp, "tensor/invalid-argument", "atan2: tensor element out of Float64 range");
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
                if (cloned == null) return tensor_raise(interp, "tensor/out-of-memory", "atan2: failed to allocate BigFloat tensor element");
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
        if (conjugated == null) return tensor_raise(interp, "tensor/invalid-argument", "conjugate: failed to conjugate tensor element");
        if (dst_values[i] != null) omni_big_complex_free(dst_values[i]);
        dst_values[i] = conjugated;
    }

    keep_out = true;
    return out_value;
}
