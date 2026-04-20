module lisp;

import std::core::mem;
import main;

extern fn double ml_attention_c_exp(double x) @extern("exp");
extern fn double ml_attention_c_sqrt(double x) @extern("sqrt");

fn Value* ml_attention_parse_scale(Value* value, usz head_dim, double* out_scale, Interp* interp) {
    if (out_scale == null) return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: scale parser state is missing");
    *out_scale = 0.0;
    if (head_dim == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/scaled-dot-product-attention: head dimension must be non-empty", null);
    }
    double scale = 1.0 / ml_attention_c_sqrt((double)head_dim);
    if (value != null) {
        if (!try_numeric_to_double(value, &scale)) {
            return tensor_type_raise(interp, "ml/scaled-dot-product-attention: scale must be a positive finite number");
        }
    }
    if (!conversion_double_is_finite(scale) || scale <= 0.0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "ml/scaled-dot-product-attention: scale must be positive and finite", null);
    }
    *out_scale = scale;
    return null;
}

fn Value* ml_attention_validate(
    TensorVal* query,
    TensorVal* key,
    TensorVal* value,
    TensorVal* mask,
    bool has_mask,
    Interp* interp,
    usz** out_shape,
    usz* out_rank,
    usz* out_batch_count,
    usz* out_query_len,
    usz* out_key_len,
    usz* out_head_dim,
    usz* out_value_dim,
    usz* out_mask_kind
) {
    if (out_shape == null || out_rank == null || out_batch_count == null ||
        out_query_len == null || out_key_len == null || out_head_dim == null ||
        out_value_dim == null || out_mask_kind == null) {
        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: shape state is missing");
    }
    *out_shape = null;
    *out_rank = 0;
    *out_batch_count = 0;
    *out_query_len = 0;
    *out_key_len = 0;
    *out_head_dim = 0;
    *out_value_dim = 0;
    *out_mask_kind = 0;

    if (query == null || key == null || value == null || (has_mask && mask == null)) {
        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: tensor metadata is missing");
    }
    if (query.dtype != key.dtype || query.dtype != value.dtype || (has_mask && query.dtype != mask.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "ml/scaled-dot-product-attention: tensor dtype mismatch", null);
    }
    if (!ml_reduction_supported_dtype(query.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/scaled-dot-product-attention: currently supports Float64 and Float32 tensors", null);
    }
    if (query.device != key.device || query.device != value.device || (has_mask && query.device != mask.device)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/scaled-dot-product-attention: tensors must be on the same device", null);
    }
    if (query.rank < 2 || key.rank != query.rank || value.rank != query.rank) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/scaled-dot-product-attention: expected query/key/value rank >= 2 with matching ranks", null);
    }

    usz rank = query.rank;
    usz query_len = query.shape[rank - 2];
    usz key_len = key.shape[rank - 2];
    usz head_dim = query.shape[rank - 1];
    usz value_dim = value.shape[rank - 1];
    if (query_len == 0 || key_len == 0 || head_dim == 0 || value_dim == 0 || key.shape[rank - 1] != head_dim || value.shape[rank - 2] != key_len) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/scaled-dot-product-attention: query, key, and value shapes are incompatible", null);
    }

    usz batch_count = 1;
    for (usz axis = 0; axis + 2 < rank; axis++) {
        if (query.shape[axis] != key.shape[axis] || query.shape[axis] != value.shape[axis]) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/scaled-dot-product-attention: batch prefix shapes must match", null);
        }
        if (query.shape[axis] == 0 || !tensor_checked_mul_usz(batch_count, query.shape[axis], &batch_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/scaled-dot-product-attention: result shape is too large", null);
        }
    }

    usz mask_kind = 0;
    if (has_mask) {
        if (mask.rank == 2) {
            if (mask.shape[0] != query_len || mask.shape[1] != key_len) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/scaled-dot-product-attention: mask shape must match query/key score shape", null);
            }
            mask_kind = 1;
        } else if (mask.rank == rank) {
            for (usz axis = 0; axis + 2 < rank; axis++) {
                if (mask.shape[axis] != query.shape[axis]) {
                    return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/scaled-dot-product-attention: mask batch prefix must match query", null);
                }
            }
            if (mask.shape[rank - 2] != query_len || mask.shape[rank - 1] != key_len) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/scaled-dot-product-attention: mask shape must match query/key score shape", null);
            }
            mask_kind = 2;
        } else {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/scaled-dot-product-attention: mask must have rank 2 or match query rank", null);
        }
    }

    if (rank > usz.max / usz.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/scaled-dot-product-attention: result rank is too large", null);
    }
    usz* result_shape = (usz*)mem::malloc(usz.sizeof * rank);
    if (result_shape == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/scaled-dot-product-attention: failed to allocate result shape", null);
    }
    for (usz axis = 0; axis + 2 < rank; axis++) result_shape[axis] = query.shape[axis];
    result_shape[rank - 2] = query_len;
    result_shape[rank - 1] = value_dim;

    *out_shape = result_shape;
    *out_rank = rank;
    *out_batch_count = batch_count;
    *out_query_len = query_len;
    *out_key_len = key_len;
    *out_head_dim = head_dim;
    *out_value_dim = value_dim;
    *out_mask_kind = mask_kind;
    return null;
}

fn bool ml_attention_read(TensorVal* tensor, usz logical_index, double* out_value) {
    return ml_reduction_read(tensor, logical_index, out_value);
}

fn Value* ml_attention_eval_cpu(
    TensorVal* query,
    TensorVal* key,
    TensorVal* value,
    TensorVal* mask,
    usz mask_kind,
    double scale,
    usz batch_count,
    usz query_len,
    usz key_len,
    usz head_dim,
    usz value_dim,
    TensorVal* out,
    Interp* interp
) {
    if (out == null || query == null || key == null || value == null) {
        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: tensor backing storage is missing");
    }
    usz score_bytes = 0;
    if (!tensor_checked_mul_usz(key_len, double.sizeof, &score_bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/scaled-dot-product-attention: score accumulator is too large", null);
    }
    double* scores = (double*)mem::malloc(score_bytes);
    if (scores == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/scaled-dot-product-attention: failed to allocate score accumulator", null);
    }
    defer mem::free(scores);

    usz out_count = out.element_count;
    usz out_bytes = 0;
    if (!tensor_checked_mul_usz(out_count, double.sizeof, &out_bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/scaled-dot-product-attention: output accumulator is too large", null);
    }
    double* values = (double*)mem::malloc(out_bytes);
    if (values == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/scaled-dot-product-attention: failed to allocate output accumulator", null);
    }
    defer mem::free(values);

    for (usz batch = 0; batch < batch_count; batch++) {
        for (usz q = 0; q < query_len; q++) {
            double max_score = 0.0;
            for (usz k = 0; k < key_len; k++) {
                double dot = 0.0;
                for (usz d = 0; d < head_dim; d++) {
                    double qv = 0.0;
                    double kv = 0.0;
                    if (!ml_attention_read(query, ((batch * query_len + q) * head_dim) + d, &qv) ||
                        !ml_attention_read(key, ((batch * key_len + k) * head_dim) + d, &kv)) {
                        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: tensor index offset is invalid");
                    }
                    dot += qv * kv;
                }
                double score = dot * scale;
                if (mask_kind != 0) {
                    usz mask_index = mask_kind == 1 ? (q * key_len + k) : ((batch * query_len + q) * key_len + k);
                    double mask_value = 0.0;
                    if (!ml_attention_read(mask, mask_index, &mask_value)) {
                        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: mask index offset is invalid");
                    }
                    score += mask_value;
                }
                scores[k] = score;
                if (k == 0 || score > max_score) max_score = score;
            }

            double exp_sum = 0.0;
            for (usz k = 0; k < key_len; k++) {
                scores[k] = ml_attention_c_exp(scores[k] - max_score);
                exp_sum += scores[k];
            }
            if (!conversion_double_is_finite(exp_sum) || exp_sum <= 0.0) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/numeric-overflow", "ml/scaled-dot-product-attention: softmax denominator is not representable", null);
            }

            for (usz vd = 0; vd < value_dim; vd++) {
                double acc = 0.0;
                for (usz k = 0; k < key_len; k++) {
                    double vv = 0.0;
                    if (!ml_attention_read(value, ((batch * key_len + k) * value_dim) + vd, &vv)) {
                        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: value tensor index offset is invalid");
                    }
                    acc += (scores[k] / exp_sum) * vv;
                }
                values[((batch * query_len + q) * value_dim) + vd] = acc;
            }
        }
    }

    return ml_reduction_write_result(out, values, interp, "ml/scaled-dot-product-attention: Float32 result is not representable");
}

fn Value* ml_attention_eval_vulkan(
    TensorVal* query,
    TensorVal* key,
    TensorVal* value,
    TensorVal* mask,
    usz mask_kind,
    double scale,
    usz[] result_shape,
    usz batch_count,
    usz query_len,
    usz key_len,
    usz head_dim,
    usz value_dim,
    Interp* interp
) {
    if (query == null || key == null || value == null) {
        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: Vulkan tensor metadata is missing");
    }
    if (query.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/scaled-dot-product-attention: Vulkan currently supports Float32 tensors", null);
    }
    if (!tensor_has_valid_device_storage(query, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(key, TENSOR_DEVICE_VULKAN) ||
        !tensor_has_valid_device_storage(value, TENSOR_DEVICE_VULKAN) ||
        (mask_kind != 0 && !tensor_has_valid_device_storage(mask, TENSOR_DEVICE_VULKAN))) {
        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: Vulkan tensor backing storage is missing");
    }
    if (!tensor_dense_row_major(query) || !tensor_dense_row_major(key) || !tensor_dense_row_major(value) ||
        (mask_kind != 0 && !tensor_dense_row_major(mask))) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/scaled-dot-product-attention: Vulkan currently supports dense row-major tensors", null);
    }
    if (scale > (double)TENSOR_FLOAT32_FINITE_LIMIT) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/domain-error", "ml/scaled-dot-product-attention: Vulkan scale must be representable as positive Float32", null);
    }

    TensorVal* tensor = tensor_alloc_payload_base_checked(query.dtype, result_shape, false, false, "ml/scaled-dot-product-attention: failed to allocate Vulkan result tensor metadata");
    if (tensor == null) return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: failed to allocate Vulkan result tensor metadata");
    tensor.kind = TENSOR_PAYLOAD_CONCRETE;
    tensor.device = TENSOR_DEVICE_VULKAN;

    void* out_device = null;
    void* mask_device = mask_kind == 0 ? query.device_handle : mask.device_handle;
    usz mask_byte_len = mask_kind == 0 ? query.byte_len : mask.byte_len;
    int status = omni_tensor_backend_vulkan_ml_attention_f32(
        query.device_handle,
        query.byte_len,
        key.device_handle,
        key.byte_len,
        value.device_handle,
        value.byte_len,
        mask_device,
        mask_byte_len,
        batch_count,
        query_len,
        key_len,
        head_dim,
        value_dim,
        (uint)mask_kind,
        (float)scale,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        tensor_free_payload(tensor);
        return tensor_vulkan_status_raise(interp, status, "ml/scaled-dot-product-attention: Vulkan attention kernel failed");
    }
    tensor.device_handle = out_device;
    tensor.device_finalizer = out_device != null ? &tensor_vulkan_device_finalizer : null;

    Value* result = interp.alloc_value();
    if (result == null) {
        tensor_free_payload(tensor);
        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: failed to allocate Vulkan result tensor value");
    }
    result.tag = TENSOR;
    result.tensor_val = tensor;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_value);
    return result;
}

fn Value* prim_ml_scaled_dot_product_attention(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3 || args.len > 5) {
        return primitive_arity_raise(interp, "ml/scaled-dot-product-attention", "3 to 5 arguments: query key value [mask] [scale]", args.len);
    }
    if (!is_tensor(args[0]) || !is_tensor(args[1]) || !is_tensor(args[2])) {
        return tensor_type_raise(interp, "ml/scaled-dot-product-attention: expected Tensor query, key, and value");
    }

    bool has_mask = false;
    Value* mask_arg = null;
    Value* scale_arg = null;
    if (args.len == 4) {
        if (is_tensor(args[3])) {
            has_mask = true;
            mask_arg = args[3];
        } else {
            scale_arg = args[3];
        }
    } else if (args.len == 5) {
        if (!is_tensor(args[3])) return tensor_type_raise(interp, "ml/scaled-dot-product-attention: mask must be a Tensor");
        has_mask = true;
        mask_arg = args[3];
        scale_arg = args[4];
    }

    bool touches_vulkan = tensor_expr_has_device(args[0], TENSOR_DEVICE_VULKAN) ||
        tensor_expr_has_device(args[1], TENSOR_DEVICE_VULKAN) ||
        tensor_expr_has_device(args[2], TENSOR_DEVICE_VULKAN) ||
        (has_mask && tensor_expr_has_device(mask_arg, TENSOR_DEVICE_VULKAN));
    if (touches_vulkan) {
        if (!tensor_expr_has_device(args[0], TENSOR_DEVICE_VULKAN) ||
            !tensor_expr_has_device(args[1], TENSOR_DEVICE_VULKAN) ||
            !tensor_expr_has_device(args[2], TENSOR_DEVICE_VULKAN) ||
            (has_mask && !tensor_expr_has_device(mask_arg, TENSOR_DEVICE_VULKAN)) ||
            tensor_expr_has_non_vulkan_device(args[0]) ||
            tensor_expr_has_non_vulkan_device(args[1]) ||
            tensor_expr_has_non_vulkan_device(args[2]) ||
            (has_mask && tensor_expr_has_non_vulkan_device(mask_arg))) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/scaled-dot-product-attention: Vulkan operands must remain Vulkan-placed", null);
        }
    }

    TensorVal* query = null;
    TensorVal* key = null;
    TensorVal* value = null;
    TensorVal* mask = null;
    Value* query_value = null;
    Value* key_value = null;
    Value* value_value = null;
    Value* mask_value = null;
    Value* err = tensor_expr_resolve_concrete_any_device(args[0], &query, &query_value, interp, "ml/scaled-dot-product-attention: query tensor backing storage is missing");
    if (err != null) return err;
    defer if (query_value != null && query_value != args[0]) boundary_cleanup_materialized_value(null, query_value, interp.current_scope);
    err = tensor_expr_resolve_concrete_any_device(args[1], &key, &key_value, interp, "ml/scaled-dot-product-attention: key tensor backing storage is missing");
    if (err != null) return err;
    defer if (key_value != null && key_value != args[1]) boundary_cleanup_materialized_value(null, key_value, interp.current_scope);
    err = tensor_expr_resolve_concrete_any_device(args[2], &value, &value_value, interp, "ml/scaled-dot-product-attention: value tensor backing storage is missing");
    if (err != null) return err;
    defer if (value_value != null && value_value != args[2]) boundary_cleanup_materialized_value(null, value_value, interp.current_scope);
    if (has_mask) {
        err = tensor_expr_resolve_concrete_any_device(mask_arg, &mask, &mask_value, interp, "ml/scaled-dot-product-attention: mask tensor backing storage is missing");
        if (err != null) return err;
    }
    defer if (mask_value != null && mask_value != mask_arg) boundary_cleanup_materialized_value(null, mask_value, interp.current_scope);

    usz* result_shape = null;
    usz result_rank = 0;
    usz batch_count = 0;
    usz query_len = 0;
    usz key_len = 0;
    usz head_dim = 0;
    usz value_dim = 0;
    usz mask_kind = 0;
    err = ml_attention_validate(query, key, value, mask, has_mask, interp, &result_shape, &result_rank, &batch_count, &query_len, &key_len, &head_dim, &value_dim, &mask_kind);
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    double scale = 0.0;
    err = ml_attention_parse_scale(scale_arg, head_dim, &scale, interp);
    if (err != null) return err;

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];

    if (query.device != TENSOR_DEVICE_CPU && query.device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/scaled-dot-product-attention: backend kernel is not implemented for this tensor placement", null);
    }
    if (query.device == TENSOR_DEVICE_VULKAN) {
        return ml_attention_eval_vulkan(query, key, value, mask, mask_kind, scale, shape_slice, batch_count, query_len, key_len, head_dim, value_dim, interp);
    }

    if (!tensor_has_valid_storage(query) || !tensor_has_valid_storage(key) || !tensor_has_valid_storage(value) ||
        (has_mask && !tensor_has_valid_storage(mask))) {
        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: tensor backing storage is missing");
    }
    Value* out_value = make_tensor_uninitialized_checked(interp, query.dtype, shape_slice, "ml/scaled-dot-product-attention: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/scaled-dot-product-attention: result tensor backing storage is missing");
    }
    err = ml_attention_eval_cpu(query, key, value, mask, mask_kind, scale, batch_count, query_len, key_len, head_dim, value_dim, out_value.tensor_val, interp);
    if (err != null) return err;
    keep_out = true;
    return out_value;
}
