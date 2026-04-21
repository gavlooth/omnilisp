module lisp;

import std::core::mem;
import main;

fn bool tensor_value_storage_aliases(Value* source_value, TensorVal* dest) {
    if (source_value == null || dest == null || dest.data == null || !is_tensor(source_value)) return false;
    TensorVal* source = source_value.tensor_val;
    if (source == null) return false;
    if (source.alias_origin != null && dest.alias_origin != null && source.alias_origin == dest.alias_origin) return true;
    return tensor_has_valid_storage(source) && source.byte_len > 0 && source.data == dest.data;
}

fn bool tensor_expr_storage_aliases(Value* source_expr, TensorVal* dest) {
    if (source_expr == null || dest == null || dest.data == null || !is_tensor(source_expr)) return false;

    TensorVal* source = source_expr.tensor_val;
    if (source == null || source.kind == TENSOR_PAYLOAD_CONCRETE) {
        return tensor_value_storage_aliases(source_expr, dest);
    }

    switch (source.kind) {
        case TENSOR_PAYLOAD_MAP:
            return tensor_expr_storage_aliases(source.map_left, dest) || tensor_expr_storage_aliases(source.map_right, dest);
        case TENSOR_PAYLOAD_CONTRACT:
            return tensor_expr_storage_aliases(source.contract_left, dest) || tensor_expr_storage_aliases(source.contract_right, dest);
        case TENSOR_PAYLOAD_VIEW:
            return tensor_expr_storage_aliases(source.view_source, dest);
        default:
            return false;
    }
}

fn bool tensor_contract_sources_alias_destination(TensorVal* source, TensorVal* dest) {
    if (source == null || dest == null || source.kind != TENSOR_PAYLOAD_CONTRACT) return false;
    return tensor_expr_storage_aliases(source.contract_left, dest) || tensor_expr_storage_aliases(source.contract_right, dest);
}

fn bool tensor_rank2_row_major(TensorVal* tensor) @inline {
    return tensor != null &&
        tensor.rank == 2 &&
        tensor.storage_offset == 0 &&
        tensor.shape != null &&
        tensor.strides != null &&
        tensor.strides[1] == 1 &&
        tensor.strides[0] == tensor.shape[1];
}

fn bool tensor_rank1_contiguous(TensorVal* tensor) @inline {
    return tensor != null &&
        tensor.rank == 1 &&
        tensor.storage_offset == 0 &&
        tensor.shape != null &&
        tensor.strides != null &&
        tensor.strides[0] == 1;
}

fn bool tensor_dense_row_major(TensorVal* tensor) {
    if (tensor == null || !tensor_has_valid_metadata(tensor)) return false;
    if (tensor.storage_offset != 0) return false;
    if (tensor.rank == 0) return true;
    usz expected_stride = 1;
    for (usz i = tensor.rank; i > 0; i--) {
        usz axis = i - 1;
        if (tensor.strides[axis] != expected_stride) return false;
        if (tensor.shape[axis] != 0 && expected_stride > usz.max / tensor.shape[axis]) return false;
        expected_stride *= tensor.shape[axis];
    }
    return true;
}

fn bool tensor_concrete_zero_offset_dense_row_major(TensorVal* tensor) {
    return tensor != null &&
        tensor.kind == TENSOR_PAYLOAD_CONCRETE &&
        !tensor.is_view &&
        tensor.storage_offset == 0 &&
        tensor_dense_row_major(tensor);
}

fn bool tensor_rank0_scalar(TensorVal* tensor) @inline {
    return tensor != null &&
        tensor.rank == 0 &&
        tensor.element_count == 1 &&
        tensor.data != null;
}

fn bool tensor_usz_fits_blas_int(usz value) @inline {
    return value <= 2147483647;
}

fn bool tensor_contract_try_blas_ddot(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    TensorVal* out_tensor
) {
    if (axis_count != 1 || left_axes == null || right_axes == null) return false;
    if (!tensor_rank1_contiguous(left) || !tensor_rank1_contiguous(right) || !tensor_rank0_scalar(out_tensor)) return false;
    if (left_axes[0] != 0 || right_axes[0] != 0) return false;
    usz n = left.shape[0];
    if (n == 0 || right.shape[0] != n || !tensor_usz_fits_blas_int(n)) return false;
    return omni_tensor_backend_blas_ddot(
        n,
        (double*)left.data,
        (double*)right.data,
        (double*)out_tensor.data
    ) != 0;
}

fn bool tensor_contract_try_blas_dger(
    TensorVal* left,
    TensorVal* right,
    usz axis_count,
    TensorVal* out_tensor
) {
    if (axis_count != 0) return false;
    if (!tensor_rank1_contiguous(left) || !tensor_rank1_contiguous(right) || !tensor_rank2_row_major(out_tensor)) return false;
    usz m = left.shape[0];
    usz n = right.shape[0];
    if (m == 0 || n == 0 || out_tensor.shape[0] != m || out_tensor.shape[1] != n) return false;
    if (!tensor_usz_fits_blas_int(m) || !tensor_usz_fits_blas_int(n) || !tensor_usz_fits_blas_int(out_tensor.strides[0])) return false;
    return omni_tensor_backend_blas_dger(
        m,
        n,
        (double*)left.data,
        (double*)right.data,
        (double*)out_tensor.data,
        out_tensor.strides[0]
    ) != 0;
}

fn bool tensor_contract_try_blas_dgemm(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    TensorVal* out_tensor
) {
    if (axis_count != 1 || left_axes == null || right_axes == null) return false;
    if (!tensor_rank2_row_major(left) || !tensor_rank2_row_major(right) || !tensor_rank2_row_major(out_tensor)) return false;

    usz left_axis = left_axes[0];
    usz right_axis = right_axes[0];
    if (left_axis > 1 || right_axis > 1) return false;

    usz left_free_axis = left_axis == 0 ? 1 : 0;
    usz right_free_axis = right_axis == 0 ? 1 : 0;
    usz m = left.shape[left_free_axis];
    usz k = left.shape[left_axis];
    usz n = right.shape[right_free_axis];
    if (m == 0 || n == 0 || k == 0) return false;
    if (right.shape[right_axis] != k || out_tensor.shape[0] != m || out_tensor.shape[1] != n) return false;
    if (!tensor_usz_fits_blas_int(m) || !tensor_usz_fits_blas_int(n) || !tensor_usz_fits_blas_int(k) ||
        !tensor_usz_fits_blas_int(left.strides[0]) || !tensor_usz_fits_blas_int(right.strides[0]) ||
        !tensor_usz_fits_blas_int(out_tensor.strides[0])) {
        return false;
    }

    int trans_a = left_axis == 0 ? 1 : 0;
    int trans_b = right_axis == 1 ? 1 : 0;
    return omni_tensor_backend_blas_dgemm(
        m,
        n,
        k,
        trans_a,
        trans_b,
        (double*)left.data,
        left.strides[0],
        (double*)right.data,
        right.strides[0],
        (double*)out_tensor.data,
        out_tensor.strides[0]
    ) != 0;
}

fn bool tensor_contract_try_blas_dgemv(
    TensorVal* left,
    TensorVal* right,
    usz* left_axes,
    usz* right_axes,
    usz axis_count,
    TensorVal* out_tensor
) {
    if (axis_count != 1 || left_axes == null || right_axes == null) return false;

    usz left_axis = left_axes[0];
    usz right_axis = right_axes[0];

    if (tensor_rank2_row_major(left) && tensor_rank1_contiguous(right) && tensor_rank1_contiguous(out_tensor)) {
        if (left_axis > 1 || right_axis != 0) return false;
        usz rows = left.shape[0];
        usz cols = left.shape[1];
        usz contracted = left.shape[left_axis];
        usz result_len = left.shape[left_axis == 0 ? 1 : 0];
        if (contracted == 0 || result_len == 0 || right.shape[0] != contracted || out_tensor.shape[0] != result_len) return false;
        if (!tensor_usz_fits_blas_int(rows) || !tensor_usz_fits_blas_int(cols) || !tensor_usz_fits_blas_int(left.strides[0])) return false;
        int trans_a = left_axis == 0 ? 1 : 0;
        return omni_tensor_backend_blas_dgemv(
            rows,
            cols,
            trans_a,
            (double*)left.data,
            left.strides[0],
            (double*)right.data,
            (double*)out_tensor.data
        ) != 0;
    }

    if (tensor_rank1_contiguous(left) && tensor_rank2_row_major(right) && tensor_rank1_contiguous(out_tensor)) {
        if (left_axis != 0 || right_axis > 1) return false;
        usz rows = right.shape[0];
        usz cols = right.shape[1];
        usz contracted = right.shape[right_axis];
        usz result_len = right.shape[right_axis == 0 ? 1 : 0];
        if (contracted == 0 || result_len == 0 || left.shape[0] != contracted || out_tensor.shape[0] != result_len) return false;
        if (!tensor_usz_fits_blas_int(rows) || !tensor_usz_fits_blas_int(cols) || !tensor_usz_fits_blas_int(right.strides[0])) return false;
        int trans_a = right_axis == 0 ? 1 : 0;
        return omni_tensor_backend_blas_dgemv(
            rows,
            cols,
            trans_a,
            (double*)right.data,
            right.strides[0],
            (double*)left.data,
            (double*)out_tensor.data
        ) != 0;
    }

    return false;
}
