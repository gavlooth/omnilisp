module lisp;

import std::core::mem;
import main;

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
            return tensor_raise(interp, "tensor/invalid-argument", "minmax: complex numbers are not ordered");
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
        return tensor_raise(interp, "tensor/invalid-argument", "minmax: complex numbers are not ordered");
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
        if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
            return make_runtime_out_of_memory_error(interp, "Tensor: failed to register result destructor");
        }
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
            return tensor_raise(interp, "tensor/invalid-argument", "minmax: numeric comparison failed");
        }
        Value* chosen = is_min ? (cmp <= 0 ? left_arg : right_arg) : (cmp >= 0 ? left_arg : right_arg);
        if (result_dtype == TENSOR_DTYPE_BIG_COMPLEX) return tensor_raise(interp, "tensor/invalid-argument", "minmax: complex numbers are not ordered");

        if (result_dtype == TENSOR_DTYPE_DOUBLE) {
            double out = 0.0;
            if (!try_numeric_to_double(chosen, &out)) {
                return tensor_raise(interp, "tensor/invalid-argument", op_name);
            }
            ((double*)out_tensor.data)[i] = out;
        } else if (result_dtype == TENSOR_DTYPE_FLOAT32) {
            float out = 0.0f;
            if (!tensor_value_to_float32(chosen, &out)) {
                return tensor_raise(interp, "tensor/invalid-argument", op_name);
            }
            ((float*)out_tensor.data)[i] = out;
        } else if (result_dtype == TENSOR_DTYPE_BIG_INTEGER) {
            void* handle = big_integer_handle_from_value(chosen);
            if (handle == null) return tensor_raise(interp, "tensor/invalid-argument", op_name);
            void** dst = (void**)out_tensor.data;
            if (dst[i] != null) omni_big_integer_free(dst[i]);
            dst[i] = handle;
        } else if (result_dtype == TENSOR_DTYPE_BIG_FLOAT) {
            void* handle = big_float_handle_from_value(chosen);
            if (handle == null) return tensor_raise(interp, "tensor/invalid-argument", op_name);
            void** dst = (void**)out_tensor.data;
            if (dst[i] != null) omni_big_float_free(dst[i]);
            dst[i] = handle;
        }
    }

    keep_out = true;
    return out_value;
}
