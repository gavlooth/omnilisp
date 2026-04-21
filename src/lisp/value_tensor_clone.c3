module lisp;

import std::core::mem;
import main;

fn TensorVal* tensor_clone_payload(TensorVal* src) {
    if (src == null) return null;
    bool clone_view_payload = src.kind == TENSOR_PAYLOAD_VIEW && src.is_view;
    bool clone_cuda_device_payload = tensor_has_valid_device_storage(src, TENSOR_DEVICE_CUDA);
    bool clone_vulkan_device_payload = tensor_has_valid_device_storage(src, TENSOR_DEVICE_VULKAN) &&
        (src.device_handle == null || src.device_finalizer == &tensor_vulkan_device_finalizer);
    if ((src.device != TENSOR_DEVICE_CPU || src.device_handle != null) && !clone_view_payload && !clone_cuda_device_payload && !clone_vulkan_device_payload) return null;
    TensorVal* tensor = (TensorVal*)mem::malloc(TensorVal.sizeof);
    if (tensor == null) return null;
    *tensor = *src;
    tensor.shape = null;
    tensor.strides = null;
    tensor.data = null;
    tensor.device_handle = null;
    tensor.device_finalizer = null;
    tensor.is_view = false;
    tensor.storage_offset = 0;
    if (src.kind == TENSOR_PAYLOAD_VIEW) {
        tensor.storage_offset = src.storage_offset;
        tensor.storage_element_count = src.storage_element_count;
        tensor.storage_byte_len = src.storage_byte_len;
    } else {
        tensor.storage_element_count = tensor.kind == TENSOR_PAYLOAD_CONCRETE ? src.element_count : 0;
        tensor.storage_byte_len = tensor.kind == TENSOR_PAYLOAD_CONCRETE ? src.byte_len : 0;
    }
    tensor.map_left_big_integer_scalar = null;
    tensor.map_right_big_integer_scalar = null;
    tensor.map_left_big_float_scalar = null;
    tensor.map_right_big_float_scalar = null;
    tensor.map_left_big_complex_scalar = null;
    tensor.map_right_big_complex_scalar = null;
    tensor.contract_left_axes = null;
    tensor.contract_right_axes = null;
    tensor.view_source = null;
    bool tensor_committed = false;
    defer {
        if (!tensor_committed) tensor_free_payload(tensor);
    }

    if (src.rank > 0) {
        if (src.shape == null || src.strides == null) {
            return null;
        }
        if (src.rank > usz.max / usz.sizeof) {
            return null;
        }
        tensor.shape = (usz*)mem::malloc(usz.sizeof * src.rank);
        if (tensor.shape == null) {
            return null;
        }
        tensor.strides = (usz*)mem::malloc(usz.sizeof * src.rank);
        if (tensor.strides == null) {
            return null;
        }
        mem::copy(tensor.shape, src.shape, usz.sizeof * src.rank);
        mem::copy(tensor.strides, src.strides, usz.sizeof * src.rank);
    }

    if (src.kind == TENSOR_PAYLOAD_VIEW) {
        tensor.data = src.data;
        tensor.device_handle = src.device_handle;
        tensor.device_finalizer = null;
        tensor.is_view = true;
        tensor.mutable = false;
        tensor_committed = true;
        return tensor;
    }

    if (clone_vulkan_device_payload) {
        if (src.byte_len > 0 && src.device_handle == null) {
            return null;
        }
        tensor.device_handle = omni_tensor_backend_vulkan_retain(src.device_handle);
        if (src.byte_len > 0 && tensor.device_handle == null) {
            return null;
        }
        tensor.device_finalizer = tensor.device_handle != null ? &tensor_vulkan_device_finalizer : null;
        tensor_committed = true;
        return tensor;
    }

    if (clone_cuda_device_payload) {
        if (src.byte_len > 0 && src.device_handle == null) {
            return null;
        }
        if (src.byte_len > 0) {
            void* device_handle = null;
            int status = omni_tensor_backend_cuda_alloc(src.byte_len, &device_handle);
            if (status != OMNI_TENSOR_CUDA_SUCCESS || device_handle == null) {
                return null;
            }
            status = omni_tensor_backend_cuda_copy_device_to_existing_device(src.device_handle, src.byte_len, device_handle);
            if (status != OMNI_TENSOR_CUDA_SUCCESS) {
                omni_tensor_backend_cuda_free(device_handle);
                return null;
            }
            tensor.device_handle = device_handle;
        }
        tensor.device_finalizer = tensor.device_handle != null ? &tensor_cuda_device_finalizer : null;
        tensor_committed = true;
        return tensor;
    }

    if (src.kind == TENSOR_PAYLOAD_CONCRETE && src.byte_len > 0) {
        if (src.data == null) {
            return null;
        }
        tensor.data = mem::malloc(src.byte_len);
        if (tensor.data == null) {
            return null;
        }
        switch (src.dtype) {
            case TENSOR_DTYPE_DOUBLE:
            case TENSOR_DTYPE_FLOAT32:
            case TENSOR_DTYPE_COMPLEX128:
            case TENSOR_DTYPE_COMPLEX64:
                mem::copy(tensor.data, src.data, src.byte_len);
            case TENSOR_DTYPE_BIG_INTEGER: {
                void** values = (void**)tensor.data;
                for (usz i = 0; i < src.element_count; i++) values[i] = null;
                if (!tensor_clone_big_integer_data(tensor, src)) {
                    return null;
                }
            }
            case TENSOR_DTYPE_BIG_FLOAT: {
                void** values = (void**)tensor.data;
                for (usz i = 0; i < src.element_count; i++) values[i] = null;
                if (!tensor_clone_big_float_data(tensor, src)) {
                    return null;
                }
            }
            case TENSOR_DTYPE_BIG_COMPLEX: {
                void** values = (void**)tensor.data;
                for (usz i = 0; i < src.element_count; i++) values[i] = null;
                if (!tensor_clone_big_complex_data(tensor, src)) {
                    return null;
                }
            }
        }
    }
    if (src.kind == TENSOR_PAYLOAD_MAP && src.dtype == TENSOR_DTYPE_BIG_INTEGER) {
        if (src.map_left_big_integer_scalar != null) {
            tensor.map_left_big_integer_scalar = omni_big_integer_clone(src.map_left_big_integer_scalar);
            if (tensor.map_left_big_integer_scalar == null) {
                return null;
            }
        }
        if (src.map_right_big_integer_scalar != null) {
            tensor.map_right_big_integer_scalar = omni_big_integer_clone(src.map_right_big_integer_scalar);
            if (tensor.map_right_big_integer_scalar == null) {
                return null;
            }
        }
    }
    if (src.kind == TENSOR_PAYLOAD_MAP && src.dtype == TENSOR_DTYPE_BIG_FLOAT) {
        if (src.map_left_big_float_scalar != null) {
            tensor.map_left_big_float_scalar = omni_big_float_clone(src.map_left_big_float_scalar);
            if (tensor.map_left_big_float_scalar == null) {
                return null;
            }
        }
        if (src.map_right_big_float_scalar != null) {
            tensor.map_right_big_float_scalar = omni_big_float_clone(src.map_right_big_float_scalar);
            if (tensor.map_right_big_float_scalar == null) {
                return null;
            }
        }
    }
    if (src.kind == TENSOR_PAYLOAD_MAP && src.dtype == TENSOR_DTYPE_BIG_COMPLEX) {
        if (src.map_left_big_complex_scalar != null) {
            tensor.map_left_big_complex_scalar = omni_big_complex_clone(src.map_left_big_complex_scalar);
            if (tensor.map_left_big_complex_scalar == null) {
                return null;
            }
        }
        if (src.map_right_big_complex_scalar != null) {
            tensor.map_right_big_complex_scalar = omni_big_complex_clone(src.map_right_big_complex_scalar);
            if (tensor.map_right_big_complex_scalar == null) {
                return null;
            }
        }
    }
    if (src.contract_axis_count > 0) {
        if (src.contract_axis_count > usz.max / usz.sizeof ||
            src.contract_left_axes == null ||
            src.contract_right_axes == null) {
            return null;
        }
        tensor.contract_left_axes = (usz*)mem::malloc(usz.sizeof * src.contract_axis_count);
        if (tensor.contract_left_axes == null) {
            return null;
        }
        tensor.contract_right_axes = (usz*)mem::malloc(usz.sizeof * src.contract_axis_count);
        if (tensor.contract_right_axes == null) {
            return null;
        }
        mem::copy(tensor.contract_left_axes, src.contract_left_axes, usz.sizeof * src.contract_axis_count);
        mem::copy(tensor.contract_right_axes, src.contract_right_axes, usz.sizeof * src.contract_axis_count);
    }
    tensor_committed = true;
    return tensor;
}
