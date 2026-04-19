module lisp;

import std::core::mem;
import main;

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

const double TENSOR_INV_SQRT_2 = 0.70710678118654752440084436210484903928;

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
