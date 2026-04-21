module lisp;

import std::core::mem;

extern fn double ml_clip_c_sqrt(double x) @extern("sqrt");

fn Value* ml_clip_invalid(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", message, null);
}

fn Value* ml_clip_resolve_cpu(Value* value, TensorVal** out_tensor, Value** out_value, Interp* interp, char[] label) {
    if (ml_grad_touches_vulkan(value)) return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/clip-gradients: mixed or unsupported Vulkan gradient clipping contract", null);
    return tensor_expr_resolve_concrete_cpu(value, out_tensor, out_value, interp, label, "ml/clip-gradients: backend kernel is not implemented for non-CPU tensors");
}

fn Value* ml_clip_accumulate_norm(Value* gradients, Interp* interp, double* out_sum) {
    if (gradients == null || out_sum == null) return ml_clip_invalid(interp, "ml/clip-gradients: gradient tree is missing");
    if (is_tensor(gradients)) {
        TensorVal* tensor = null;
        Value* materialized = null;
        Value* err = ml_clip_resolve_cpu(gradients, &tensor, &materialized, interp, "ml/clip-gradients: gradient tensor backing storage is missing");
        if (err != null) return err;
        defer if (materialized != null && materialized != gradients) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);
        if (!ml_reduction_supported_dtype(tensor.dtype)) return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/clip-gradients: only Float64 and Float32 tensors are supported", null);
        if (tensor.element_count == 0) return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/clip-gradients: empty gradient tensors are not supported", null);
        if (!tensor_dense_row_major(tensor)) return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/clip-gradients: currently supports dense row-major tensors", null);
        for (usz i = 0; i < tensor.element_count; i++) {
            double value = 0.0;
            if (!ml_reduction_read(tensor, i, &value)) return tensor_runtime_raise(interp, "ml/clip-gradients: tensor index is invalid");
            *out_sum = *out_sum + value * value;
            if (!conversion_double_is_finite(*out_sum)) return raise_error_with_payload_names(interp, "tensor", "tensor/numeric-overflow", "ml/clip-gradients: gradient norm is not finite", null);
        }
        return null;
    }
    if (gradients.tag == ARRAY && gradients.array_val != null) {
        for (usz i = 0; i < gradients.array_val.length; i++) {
            Value* err = ml_clip_accumulate_norm(gradients.array_val.items[i], interp, out_sum);
            if (err != null) return err;
        }
        return null;
    }
    if (gradients.tag == HASHMAP && gradients.hashmap_val != null) {
        for (uint i = 0; i < gradients.hashmap_val.capacity; i++) {
            HashEntry* entry = &gradients.hashmap_val.entries[i];
            if (entry.key == null) continue;
            Value* err = ml_clip_accumulate_norm(entry.value, interp, out_sum);
            if (err != null) return err;
        }
        return null;
    }
    return ml_clip_invalid(interp, "ml/clip-gradients: gradient tree leaves must be tensors");
}

fn Value* ml_clip_scale_tree(Value* gradients, double scale, Interp* interp, Value** out_gradients) {
    if (out_gradients == null) return ml_clip_invalid(interp, "ml/clip-gradients: output gradient tree is missing");
    if (scale >= 1.0) {
        *out_gradients = gradients;
        return null;
    }
    if (is_tensor(gradients)) {
        TensorVal* tensor = null;
        Value* materialized = null;
        Value* err = ml_clip_resolve_cpu(gradients, &tensor, &materialized, interp, "ml/clip-gradients: gradient tensor backing storage is missing");
        if (err != null) return err;
        defer if (materialized != null && materialized != gradients) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);
        if (tensor.element_count > usz.max / double.sizeof) return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/clip-gradients: gradient tensor is too large", null);
        double* values = (double*)mem::malloc(tensor.element_count * double.sizeof);
        if (values == null) return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/clip-gradients: failed to allocate clipped tensor buffer", null);
        defer mem::free(values);
        for (usz i = 0; i < tensor.element_count; i++) {
            double value = 0.0;
            if (!ml_reduction_read(tensor, i, &value)) return tensor_runtime_raise(interp, "ml/clip-gradients: tensor index is invalid");
            values[i] = value * scale;
        }
        Value* clipped = ml_grad_make_tensor(interp, tensor.dtype, tensor.shape[0:tensor.rank], "ml/clip-gradients: failed to allocate clipped tensor");
        if (clipped == null || clipped.tag == ERROR) return clipped;
        err = ml_reduction_write_result(clipped.tensor_val, values, interp, "ml/clip-gradients: Float32 clipped gradient is not representable");
        if (err != null) return err;
        *out_gradients = clipped;
        return null;
    }
    if (gradients.tag == ARRAY && gradients.array_val != null) {
        Value* clipped = make_array_checked(interp, gradients.array_val.length, "ml/clip-gradients: out of memory");
        if (clipped == null || clipped.tag == ERROR) return clipped;
        for (usz i = 0; i < gradients.array_val.length; i++) {
            Value* child = null;
            Value* err = ml_clip_scale_tree(gradients.array_val.items[i], scale, interp, &child);
            if (err != null) return err;
            clipped.array_val.items[i] = child;
            clipped.array_val.length++;
        }
        *out_gradients = clipped;
        return null;
    }
    if (gradients.tag == HASHMAP && gradients.hashmap_val != null) {
        Value* clipped = nn_make_dict(interp, gradients.hashmap_val.count == 0 ? 2 : gradients.hashmap_val.count * 2);
        if (clipped == null || clipped.tag == ERROR) return clipped;
        for (uint i = 0; i < gradients.hashmap_val.capacity; i++) {
            HashEntry* entry = &gradients.hashmap_val.entries[i];
            if (entry.key == null) continue;
            Value* child = null;
            Value* err = ml_clip_scale_tree(entry.value, scale, interp, &child);
            if (err != null) return err;
            if (!hashmap_set_checked(clipped.hashmap_val, entry.key, child, interp)) return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/clip-gradients: failed to allocate clipped dictionary", null);
        }
        *out_gradients = clipped;
        return null;
    }
    return ml_clip_invalid(interp, "ml/clip-gradients: gradient tree leaves must be tensors");
}

fn Value* ml_clip_gradient_tree(Value* gradients, double max_norm, Interp* interp, Value** out_gradients) {
    if (max_norm < 0.0 || !conversion_double_is_finite(max_norm)) return ml_clip_invalid(interp, "ml/clip-gradients: max-norm must be a non-negative finite number");
    bool vulkan_attempted = false;
    Value* vulkan_err = ml_clip_gradient_tree_try_vulkan(gradients, max_norm, interp, out_gradients, &vulkan_attempted);
    if (vulkan_err != null) return vulkan_err;
    if (vulkan_attempted) return null;

    double sum = 0.0;
    Value* err = ml_clip_accumulate_norm(gradients, interp, &sum);
    if (err != null) return err;
    double norm = ml_clip_c_sqrt(sum);
    if (!conversion_double_is_finite(norm)) return raise_error_with_payload_names(interp, "tensor", "tensor/numeric-overflow", "ml/clip-gradients: gradient norm is not finite", null);
    double scale = (norm > max_norm && norm > 0.0) ? max_norm / norm : 1.0;
    return ml_clip_scale_tree(gradients, scale, interp, out_gradients);
}

fn Value* prim_ml_clip_gradients(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return primitive_arity_raise(interp, "ml/clip-gradients", "2 arguments: gradients max-norm", args.len);
    double max_norm = 0.0;
    if (!try_numeric_to_double(args[1], &max_norm) || !conversion_double_is_finite(max_norm) || max_norm < 0.0) return primitive_arg_type_raise(interp, "ml/clip-gradients", 1, "non-negative finite number", args[1]);
    Value* clipped = null;
    Value* err = ml_clip_gradient_tree(args[0], max_norm, interp, &clipped);
    if (err != null) return err;
    return clipped;
}
