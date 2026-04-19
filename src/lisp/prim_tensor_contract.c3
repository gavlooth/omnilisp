module lisp;

import std::core::mem;
import main;

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
