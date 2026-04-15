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
        case TENSOR_DTYPE_BIG_INTEGER:
            return void*.sizeof;
        case TENSOR_DTYPE_BIG_FLOAT:
            return void*.sizeof;
    }
}

fn char[] tensor_dtype_name(TensorDType dtype) @inline {
    switch (dtype) {
        case TENSOR_DTYPE_DOUBLE:
            return "Double";
        case TENSOR_DTYPE_BIG_INTEGER:
            return "BigInteger";
        case TENSOR_DTYPE_BIG_FLOAT:
            return "BigFloat";
    }
}

fn SymbolId tensor_dtype_symbol(TensorDType dtype, Interp* interp) @inline {
    switch (dtype) {
        case TENSOR_DTYPE_DOUBLE:
            return interp.sym_Double;
        case TENSOR_DTYPE_BIG_INTEGER:
            return interp.sym_BigInteger;
        case TENSOR_DTYPE_BIG_FLOAT:
            return interp.sym_BigFloat;
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

fn void tensor_free_payload(TensorVal* tensor) {
    if (tensor == null) return;
    tensor_map_big_integer_scalar_clear(tensor);
    tensor_map_big_float_scalar_clear(tensor);
    if (tensor.shape != null) {
        mem::free(tensor.shape);
        tensor.shape = null;
    }
    if (tensor.strides != null) {
        mem::free(tensor.strides);
        tensor.strides = null;
    }
    if (!tensor.is_view && tensor.data != null) {
        if (tensor.kind == TENSOR_PAYLOAD_CONCRETE) {
            tensor_big_integer_clear_data(tensor);
            tensor_big_float_clear_data(tensor);
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
    tensor.rank = shape.len;
    tensor.element_count = element_count;
    tensor.byte_len = byte_len;
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
            case TENSOR_DTYPE_BIG_INTEGER:
            case TENSOR_DTYPE_BIG_FLOAT: {
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

fn bool tensor_copy_concrete_storage(TensorVal* dest, TensorVal* source) {
    if (dest == null || source == null || dest.dtype != source.dtype) return false;
    if (dest.element_count != source.element_count || dest.byte_len != source.byte_len) return false;
    if (source.byte_len == 0) return true;
    if (dest.data == null || source.data == null) return false;
    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE:
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
    if (tensor != null) tensor.kind = kind;
    return tensor;
}

fn TensorVal* tensor_clone_payload(TensorVal* src) {
    if (src == null) return null;
    TensorVal* tensor = (TensorVal*)mem::malloc(TensorVal.sizeof);
    if (tensor == null) return null;
    *tensor = *src;
    tensor.shape = null;
    tensor.strides = null;
    tensor.data = null;
    tensor.is_view = false;
    tensor.map_left_big_integer_scalar = null;
    tensor.map_right_big_integer_scalar = null;
    tensor.map_left_big_float_scalar = null;
    tensor.map_right_big_float_scalar = null;
    tensor.contract_left_axes = null;
    tensor.contract_right_axes = null;

    if (src.rank > 0) {
        if (src.shape == null || src.strides == null) {
            tensor_free_payload(tensor);
            return null;
        }
        if (src.rank > usz.max / usz.sizeof) {
            tensor_free_payload(tensor);
            return null;
        }
        tensor.shape = (usz*)mem::malloc(usz.sizeof * src.rank);
        if (tensor.shape == null) {
            tensor_free_payload(tensor);
            return null;
        }
        tensor.strides = (usz*)mem::malloc(usz.sizeof * src.rank);
        if (tensor.strides == null) {
            tensor_free_payload(tensor);
            return null;
        }
        mem::copy(tensor.shape, src.shape, usz.sizeof * src.rank);
        mem::copy(tensor.strides, src.strides, usz.sizeof * src.rank);
    }

    if (src.kind == TENSOR_PAYLOAD_CONCRETE && src.byte_len > 0) {
        if (src.data == null) {
            tensor_free_payload(tensor);
            return null;
        }
        tensor.data = mem::malloc(src.byte_len);
        if (tensor.data == null) {
            tensor_free_payload(tensor);
            return null;
        }
        switch (src.dtype) {
            case TENSOR_DTYPE_DOUBLE:
                mem::copy(tensor.data, src.data, src.byte_len);
            case TENSOR_DTYPE_BIG_INTEGER: {
                void** values = (void**)tensor.data;
                for (usz i = 0; i < src.element_count; i++) values[i] = null;
                if (!tensor_clone_big_integer_data(tensor, src)) {
                    tensor_free_payload(tensor);
                    return null;
                }
            }
            case TENSOR_DTYPE_BIG_FLOAT: {
                void** values = (void**)tensor.data;
                for (usz i = 0; i < src.element_count; i++) values[i] = null;
                if (!tensor_clone_big_float_data(tensor, src)) {
                    tensor_free_payload(tensor);
                    return null;
                }
            }
        }
    }
    if (src.kind == TENSOR_PAYLOAD_MAP && src.dtype == TENSOR_DTYPE_BIG_INTEGER) {
        if (src.map_left_big_integer_scalar != null) {
            tensor.map_left_big_integer_scalar = omni_big_integer_clone(src.map_left_big_integer_scalar);
            if (tensor.map_left_big_integer_scalar == null) {
                tensor_free_payload(tensor);
                return null;
            }
        }
        if (src.map_right_big_integer_scalar != null) {
            tensor.map_right_big_integer_scalar = omni_big_integer_clone(src.map_right_big_integer_scalar);
            if (tensor.map_right_big_integer_scalar == null) {
                tensor_free_payload(tensor);
                return null;
            }
        }
    }
    if (src.kind == TENSOR_PAYLOAD_MAP && src.dtype == TENSOR_DTYPE_BIG_FLOAT) {
        if (src.map_left_big_float_scalar != null) {
            tensor.map_left_big_float_scalar = omni_big_float_clone(src.map_left_big_float_scalar);
            if (tensor.map_left_big_float_scalar == null) {
                tensor_free_payload(tensor);
                return null;
            }
        }
        if (src.map_right_big_float_scalar != null) {
            tensor.map_right_big_float_scalar = omni_big_float_clone(src.map_right_big_float_scalar);
            if (tensor.map_right_big_float_scalar == null) {
                tensor_free_payload(tensor);
                return null;
            }
        }
    }
    if (src.contract_axis_count > 0) {
        if (src.contract_axis_count > usz.max / usz.sizeof ||
            src.contract_left_axes == null ||
            src.contract_right_axes == null) {
            tensor_free_payload(tensor);
            return null;
        }
        tensor.contract_left_axes = (usz*)mem::malloc(usz.sizeof * src.contract_axis_count);
        if (tensor.contract_left_axes == null) {
            tensor_free_payload(tensor);
            return null;
        }
        tensor.contract_right_axes = (usz*)mem::malloc(usz.sizeof * src.contract_axis_count);
        if (tensor.contract_right_axes == null) {
            tensor_free_payload(tensor);
            return null;
        }
        mem::copy(tensor.contract_left_axes, src.contract_left_axes, usz.sizeof * src.contract_axis_count);
        mem::copy(tensor.contract_right_axes, src.contract_right_axes, usz.sizeof * src.contract_axis_count);
    }
    return tensor;
}

fn Value* tensor_copy_expr_child_to_parent(Value* child, Interp* interp, BoundaryCopyFault* fault_out, char[] message) {
    if (child != null && tensor_should_force_expr_edge_fail(&g_tensor_force_expr_edge_copy_fail_nth)) {
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null ? null : raise_error(interp, message);
    }
    Value* copied = copy_to_parent_with_fault(child, interp, fault_out);
    if (child != null && (copied == null || is_error(copied))) {
        if (fault_out != null && *fault_out == BOUNDARY_COPY_FAULT_NONE) {
            *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        }
        return fault_out != null
            ? null
            : (copied == null ? raise_error(interp, message) : copied);
    }
    return copied;
}

fn bool tensor_copy_expr_edges_to_parent(TensorVal* dst, TensorVal* src, Interp* interp, BoundaryCopyFault* fault_out = null) {
    if (dst == null || src == null) return true;
    switch (src.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return true;
        case TENSOR_PAYLOAD_MAP:
            dst.map_func = tensor_copy_expr_child_to_parent(src.map_func, interp, fault_out, "boundary: failed to copy tensor map function");
            if (src.map_func != null && (dst.map_func == null || is_error(dst.map_func))) return false;
            dst.map_left = tensor_copy_expr_child_to_parent(src.map_left, interp, fault_out, "boundary: failed to copy tensor map left input");
            if (src.map_left != null && (dst.map_left == null || is_error(dst.map_left))) return false;
            dst.map_right = tensor_copy_expr_child_to_parent(src.map_right, interp, fault_out, "boundary: failed to copy tensor map right input");
            if (src.map_right != null && (dst.map_right == null || is_error(dst.map_right))) return false;
            return true;
        case TENSOR_PAYLOAD_CONTRACT:
            dst.contract_left = tensor_copy_expr_child_to_parent(src.contract_left, interp, fault_out, "boundary: failed to copy tensor contract left input");
            if (src.contract_left != null && (dst.contract_left == null || is_error(dst.contract_left))) return false;
            dst.contract_right = tensor_copy_expr_child_to_parent(src.contract_right, interp, fault_out, "boundary: failed to copy tensor contract right input");
            if (src.contract_right != null && (dst.contract_right == null || is_error(dst.contract_right))) return false;
            return true;
    }
}

fn void tensor_cleanup_materialized_expr_edges(
    TensorVal* src,
    TensorVal* dst,
    main::ScopeRegion* target_scope,
    bool escape_lane = false
) {
    if (src == null || dst == null) return;
    switch (src.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return;
        case TENSOR_PAYLOAD_MAP:
            Value* copied_func = dst.map_func;
            boundary_cleanup_materialized_value(src.map_func, copied_func, target_scope, escape_lane);
            dst.map_func = null;

            Value* copied_left = dst.map_left;
            if (copied_left != copied_func) {
                boundary_cleanup_materialized_value(src.map_left, copied_left, target_scope, escape_lane);
            }
            dst.map_left = null;

            Value* copied_right = dst.map_right;
            if (copied_right != copied_func && copied_right != copied_left) {
                boundary_cleanup_materialized_value(src.map_right, copied_right, target_scope, escape_lane);
            }
            dst.map_right = null;
            return;
        case TENSOR_PAYLOAD_CONTRACT:
            Value* copied_left = dst.contract_left;
            boundary_cleanup_materialized_value(src.contract_left, copied_left, target_scope, escape_lane);
            dst.contract_left = null;

            Value* copied_right = dst.contract_right;
            if (copied_right != copied_left) {
                boundary_cleanup_materialized_value(src.contract_right, copied_right, target_scope, escape_lane);
            }
            dst.contract_right = null;
            return;
    }
}

fn void boundary_cleanup_materialized_tensor(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope,
    bool escape_lane
) @inline {
    if (src == null || copied == null || src.tag != TENSOR || copied.tag != TENSOR) return;
    tensor_cleanup_materialized_expr_edges(src.tensor_val, copied.tensor_val, target_scope, escape_lane);
}

fn Value* tensor_promote_expr_child_to_escape(Value* child, Interp* interp, char[] message) {
    if (child != null && tensor_should_force_expr_edge_fail(&g_tensor_force_expr_edge_escape_fail_nth)) {
        return raise_error(interp, message);
    }
    if (child != null && interp != null && interp.current_scope != null &&
        child.scope_gen == interp.current_scope.escape_generation) {
        return child;
    }
    Value* promoted = promote_to_escape(child, interp);
    if (child != null && (promoted == null || is_error(promoted))) {
        return promoted == null ? raise_error(interp, message) : promoted;
    }
    return promoted;
}

fn bool tensor_promote_expr_edges_to_escape(TensorVal* dst, TensorVal* src, Interp* interp) {
    if (dst == null || src == null) return true;
    switch (src.kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return true;
        case TENSOR_PAYLOAD_MAP:
            dst.map_func = tensor_promote_expr_child_to_escape(src.map_func, interp, "boundary: failed to promote tensor map function");
            if (src.map_func != null && (dst.map_func == null || is_error(dst.map_func))) return false;
            dst.map_left = tensor_promote_expr_child_to_escape(src.map_left, interp, "boundary: failed to promote tensor map left input");
            if (src.map_left != null && (dst.map_left == null || is_error(dst.map_left))) return false;
            dst.map_right = tensor_promote_expr_child_to_escape(src.map_right, interp, "boundary: failed to promote tensor map right input");
            if (src.map_right != null && (dst.map_right == null || is_error(dst.map_right))) return false;
            return true;
        case TENSOR_PAYLOAD_CONTRACT:
            dst.contract_left = tensor_promote_expr_child_to_escape(src.contract_left, interp, "boundary: failed to promote tensor contract left input");
            if (src.contract_left != null && (dst.contract_left == null || is_error(dst.contract_left))) return false;
            dst.contract_right = tensor_promote_expr_child_to_escape(src.contract_right, interp, "boundary: failed to promote tensor contract right input");
            if (src.contract_right != null && (dst.contract_right == null || is_error(dst.contract_right))) return false;
            return true;
    }
}

fn Value* make_tensor_double_uninitialized_checked(Interp* interp, usz[] shape, char[] oom_message) {
    TensorVal* tensor = tensor_alloc_payload_checked(TENSOR_DTYPE_DOUBLE, shape, true, oom_message);
    if (tensor == null) return raise_error(interp, oom_message);

    Value* v = interp.alloc_value();
    if (v == null) {
        tensor_free_payload(tensor);
        return raise_error(interp, oom_message);
    }
    v.tag = TENSOR;
    v.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    return v;
}

fn Value* make_tensor_uninitialized_checked(Interp* interp, TensorDType dtype, usz[] shape, char[] oom_message) {
    if (dtype == TENSOR_DTYPE_DOUBLE) return make_tensor_double_uninitialized_checked(interp, shape, oom_message);
    TensorVal* tensor = tensor_alloc_payload_checked(dtype, shape, true, oom_message);
    if (tensor == null) return raise_error(interp, oom_message);

    Value* v = interp.alloc_value();
    if (v == null) {
        tensor_free_payload(tensor);
        return raise_error(interp, oom_message);
    }
    v.tag = TENSOR;
    v.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)v, &scope_dtor_value);
    return v;
}

fn Value* make_tensor_double_uninitialized(Interp* interp, usz[] shape) {
    return make_tensor_double_uninitialized_checked(interp, shape, "Tensor: out of memory");
}

fn Value* copy_tensor_to_parent(Value* v, Interp* interp, BoundaryCopyFault* fault_out = null) {
    if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NONE;

    TensorVal* dst = tensor_clone_payload(v.tensor_val);
    if (v.tensor_val != null && dst == null) {
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate tensor copy");
    }
    if (!tensor_copy_expr_edges_to_parent(dst, v.tensor_val, interp, fault_out)) {
        tensor_cleanup_materialized_expr_edges(v.tensor_val, dst, interp.current_scope);
        tensor_free_payload(dst);
        return fault_out != null ? null : raise_error(interp, "boundary: failed to copy tensor expression edges");
    }

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_cleanup_materialized_expr_edges(v.tensor_val, dst, interp.current_scope);
        tensor_free_payload(dst);
        if (fault_out != null) *fault_out = BOUNDARY_COPY_FAULT_NULL_RESULT;
        return fault_out != null ? null : raise_error(interp, "boundary: failed to allocate tensor copy value");
    }
    result.tag = TENSOR;
    result.tensor_val = dst;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* promote_escape_tensor_shared_wrapper(Value* v, Interp* interp, PromotionContext* ctx) {
    if (!promote_escape_can_mutate(v, ctx)) return v;

    TensorVal* dst = tensor_clone_payload(v.tensor_val);
    if (v.tensor_val != null && dst == null) {
        return raise_error(interp, "boundary: failed to allocate tensor escape copy");
    }
    if (!tensor_promote_expr_edges_to_escape(dst, v.tensor_val, interp)) {
        tensor_cleanup_materialized_expr_edges(v.tensor_val, dst, interp.current_scope, true);
        tensor_free_payload(dst);
        return raise_error(interp, "boundary: failed to promote tensor expression edges");
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        tensor_cleanup_materialized_expr_edges(v.tensor_val, dst, interp.current_scope, true);
        tensor_free_payload(dst);
        return raise_error(interp, "boundary: failed to allocate tensor escape copy value");
    }
    result.tag = TENSOR;
    result.tensor_val = dst;
    main::scope_register_dtor_escape(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* root_store_clone_tensor_to_scope(
    Value* src,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (src == null || interp == null || target_scope == null) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);

    TensorVal* dst = tensor_clone_payload(src.tensor_val);
    if (src.tensor_val != null && dst == null) return null;
    if (!tensor_promote_expr_edges_to_escape(dst, src.tensor_val, interp)) {
        tensor_cleanup_materialized_expr_edges(src.tensor_val, dst, target_scope, true);
        tensor_free_payload(dst);
        return null;
    }

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        tensor_cleanup_materialized_expr_edges(src.tensor_val, dst, target_scope, true);
        tensor_free_payload(dst);
        return null;
    }
    result.tag = TENSOR;
    result.tensor_val = dst;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}
