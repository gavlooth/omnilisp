module lisp;

import std::core::mem;
import main;

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
    if (tensor == null) return runtime_raise(interp, "runtime/out-of-memory", "map: failed to allocate tensor expression");
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
        return runtime_raise(interp, "runtime/out-of-memory", "map: failed to allocate tensor expression");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    if (!scope_register_value_dtor_or_cleanup(interp.current_scope, result)) {
        return make_runtime_out_of_memory_error(interp, "map: failed to register tensor expression destructor");
    }
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

<*
@require interp != null
*>
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
    bool touches_vulkan = (left_tensor != null && tensor_expr_has_device(args[1], TENSOR_DEVICE_VULKAN)) ||
        (args.len == 3 && right_tensor != null && tensor_expr_has_device(args[2], TENSOR_DEVICE_VULKAN));
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
    if (touches_vulkan) {
        uint vulkan_op = 0;
        Value* preflight_err = tensor_map_vulkan_callable_op_or_raise(func, args.len == 3, result_dtype, &vulkan_op, interp);
        if (preflight_err != null) return preflight_err;
        if ((left_tensor != null && tensor_expr_has_non_vulkan_device(args[1])) ||
            (args.len == 3 && right_tensor != null && tensor_expr_has_non_vulkan_device(args[2]))) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan operands must remain Vulkan-placed", null);
        }
        if (!tensor_dtype_is_vulkan_real(result_dtype) && !tensor_dtype_is_fixed_complex(result_dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: Vulkan currently supports Float64, Float32, Complex128, and Complex64 Tensor storage", null);
        }
        if ((left_tensor != null && left_tensor.dtype != result_dtype) ||
            (args.len == 3 && right_tensor != null && right_tensor.dtype != result_dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "map: Vulkan tensor dtype mismatch", null);
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
        true,
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
    if (touches_vulkan) {
        return make_tensor_map_expr(func, args[1], left_tensor, args.len == 3 ? args[2] : null, right_tensor, args.len == 3, result_dtype, shape_slice, interp);
    }
    if ((left_tensor != null && left_tensor.device != TENSOR_DEVICE_CPU) ||
        (right_tensor != null && right_tensor.device != TENSOR_DEVICE_CPU)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "map: unsupported device operands", null);
    }
    return make_tensor_map_expr(func, args[1], left_tensor, args.len == 3 ? args[2] : null, right_tensor, args.len == 3, result_dtype, shape_slice, interp);
}
