module lisp;

fn bool ml_clip_tree_touches_vulkan(Value* gradients) {
    if (gradients == null) return false;
    if (is_tensor(gradients)) return tensor_expr_has_device(gradients, TENSOR_DEVICE_VULKAN);
    if (gradients.tag == ARRAY && gradients.array_val != null) {
        for (usz i = 0; i < gradients.array_val.length; i++) {
            if (ml_clip_tree_touches_vulkan(gradients.array_val.items[i])) return true;
        }
        return false;
    }
    if (gradients.tag == HASHMAP && gradients.hashmap_val != null) {
        for (uint i = 0; i < gradients.hashmap_val.capacity; i++) {
            HashEntry* entry = &gradients.hashmap_val.entries[i];
            if (entry.key != null && ml_clip_tree_touches_vulkan(entry.value)) return true;
        }
        return false;
    }
    return false;
}

fn Value* ml_clip_vulkan_resolve_leaf(Value* value, TensorVal** out_tensor, Value** out_value, Interp* interp) {
    if (!is_tensor(value)) return ml_clip_invalid(interp, "ml/clip-gradients: gradient tree leaves must be tensors");

    Value* err = tensor_expr_resolve_vulkan_or_migrate(value, out_tensor, out_value, interp, "ml/clip-gradients: gradient tensor backing storage is missing");
    if (err != null) return err;
    if (*out_tensor == null || (*out_tensor).device != TENSOR_DEVICE_VULKAN) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/clip-gradients: Vulkan gradient trees must realize to Vulkan storage", null);
    }
    if ((*out_tensor).dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/clip-gradients: Vulkan currently supports Float32 tensors", null);
    }
    if ((*out_tensor).element_count == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "ml/clip-gradients: empty gradient tensors are not supported", null);
    }
    if (!tensor_has_valid_device_storage(*out_tensor, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "ml/clip-gradients: Vulkan tensor backing storage is missing");
    }
    return null;
}

fn Value* ml_clip_vulkan_accumulate_norm(Value* gradients, Interp* interp, double* out_sum) {
    if (gradients == null || out_sum == null) return ml_clip_invalid(interp, "ml/clip-gradients: gradient tree is missing");
    if (is_tensor(gradients)) {
        TensorVal* tensor = null;
        Value* materialized = null;
        Value* err = ml_clip_vulkan_resolve_leaf(gradients, &tensor, &materialized, interp);
        if (err != null) return err;
        defer if (materialized != null && materialized != gradients) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);

        float leaf_sum = 0.0;
        int status = omni_tensor_backend_vulkan_ml_clip_sum_squares_f32(
            tensor.device_handle,
            tensor.byte_len,
            tensor.element_count,
            &leaf_sum
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            return tensor_vulkan_status_raise(interp, status, "ml/clip-gradients: Vulkan norm kernel failed");
        }
        *out_sum = *out_sum + (double)leaf_sum;
        if (!conversion_double_is_finite(*out_sum)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/numeric-overflow", "ml/clip-gradients: gradient norm is not finite", null);
        }
        return null;
    }
    if (gradients.tag == ARRAY && gradients.array_val != null) {
        for (usz i = 0; i < gradients.array_val.length; i++) {
            Value* err = ml_clip_vulkan_accumulate_norm(gradients.array_val.items[i], interp, out_sum);
            if (err != null) return err;
        }
        return null;
    }
    if (gradients.tag == HASHMAP && gradients.hashmap_val != null) {
        for (uint i = 0; i < gradients.hashmap_val.capacity; i++) {
            HashEntry* entry = &gradients.hashmap_val.entries[i];
            if (entry.key == null) continue;
            Value* err = ml_clip_vulkan_accumulate_norm(entry.value, interp, out_sum);
            if (err != null) return err;
        }
        return null;
    }
    return ml_clip_invalid(interp, "ml/clip-gradients: gradient tree leaves must be tensors");
}

fn Value* ml_clip_vulkan_scale_tree(Value* gradients, double scale, Interp* interp, Value** out_gradients) {
    if (out_gradients == null) return ml_clip_invalid(interp, "ml/clip-gradients: output gradient tree is missing");
    if (scale >= 1.0) {
        *out_gradients = gradients;
        return null;
    }
    if (is_tensor(gradients)) {
        TensorVal* tensor = null;
        Value* materialized = null;
        Value* err = ml_clip_vulkan_resolve_leaf(gradients, &tensor, &materialized, interp);
        if (err != null) return err;
        defer if (materialized != null && materialized != gradients) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);

        void* out_device = null;
        int status = omni_tensor_backend_vulkan_ml_clip_scale_f32(
            tensor.device_handle,
            tensor.byte_len,
            tensor.element_count,
            (float)scale,
            &out_device
        );
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
            if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
            return tensor_vulkan_status_raise(interp, status, "ml/clip-gradients: Vulkan scale kernel failed");
        }

        Value* clipped = ml_optimizer_vulkan_tensor_value(tensor, out_device, interp, "ml/clip-gradients: failed to allocate Vulkan clipped tensor metadata", "ml/clip-gradients: failed to allocate Vulkan clipped tensor value");
        if (clipped == null || clipped.tag == ERROR) return clipped;
        *out_gradients = clipped;
        return null;
    }
    if (gradients.tag == ARRAY && gradients.array_val != null) {
        Value* clipped = make_array_checked(interp, gradients.array_val.length, "ml/clip-gradients: out of memory");
        if (clipped == null || clipped.tag == ERROR) return clipped;
        for (usz i = 0; i < gradients.array_val.length; i++) {
            Value* child = null;
            Value* err = ml_clip_vulkan_scale_tree(gradients.array_val.items[i], scale, interp, &child);
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
            Value* err = ml_clip_vulkan_scale_tree(entry.value, scale, interp, &child);
            if (err != null) return err;
            if (!hashmap_set_checked(clipped.hashmap_val, entry.key, child, interp)) {
                return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/clip-gradients: failed to allocate clipped dictionary", null);
            }
        }
        *out_gradients = clipped;
        return null;
    }
    return ml_clip_invalid(interp, "ml/clip-gradients: gradient tree leaves must be tensors");
}

fn Value* ml_clip_gradient_tree_try_vulkan(Value* gradients, double max_norm, Interp* interp, Value** out_gradients, bool* out_attempted) {
    if (out_attempted != null) *out_attempted = false;
    if (!ml_clip_tree_touches_vulkan(gradients)) return null;
    if (out_attempted != null) *out_attempted = true;

    double sum = 0.0;
    Value* err = ml_clip_vulkan_accumulate_norm(gradients, interp, &sum);
    if (err != null) return err;
    double norm = ml_clip_c_sqrt(sum);
    if (!conversion_double_is_finite(norm)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/numeric-overflow", "ml/clip-gradients: gradient norm is not finite", null);
    }
    double scale = (norm > max_norm && norm > 0.0) ? max_norm / norm : 1.0;
    return ml_clip_vulkan_scale_tree(gradients, scale, interp, out_gradients);
}
