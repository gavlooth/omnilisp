module lisp;

import std::core::mem;
import main;

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
        if (tensor.shape[axis] > (usz)long.max) return runtime_raise(interp, "runtime/evaluation-error", "ref: tensor dimension is too large to index");

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
