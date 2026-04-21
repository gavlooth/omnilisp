module lisp;

fn Value* kernel_invalid(Interp* interp, char[] path, char[] expected, Value* actual) @inline {
    return nn_invalid(interp, path, expected, actual);
}

fn bool kernel_symbol_is(Value* value, Interp* interp, char[] name) @inline {
    return nn_symbol_is(value, interp, name);
}

fn bool kernel_is_value(Value* value, Interp* interp) {
    if (value == null || value.tag != HASHMAP || value.hashmap_val == null) return false;
    if (!kernel_symbol_is(nn_dict_get(value, interp, "kind"), interp, "kernel")) return false;
    Value* backend = nn_dict_get(value, interp, "backend");
    Value* operation = nn_dict_get(value, interp, "operation");
    Value* inputs = nn_dict_get(value, interp, "inputs");
    Value* outputs = nn_dict_get(value, interp, "outputs");
    return backend != null && backend.tag == SYMBOL &&
        operation != null && operation.tag == SYMBOL &&
        inputs != null && inputs.tag == ARRAY && inputs.array_val != null &&
        outputs != null && outputs.tag == ARRAY && outputs.array_val != null &&
        outputs.array_val.length > 0;
}

fn bool kernel_key_allowed(Value* key, Interp* interp) {
    if (key == null || key.tag != SYMBOL) return false;
    return nn_symbol_name_is(key.sym_val, interp, "kind") ||
        nn_symbol_name_is(key.sym_val, interp, "backend") ||
        nn_symbol_name_is(key.sym_val, interp, "operation") ||
        nn_symbol_name_is(key.sym_val, interp, "inputs") ||
        nn_symbol_name_is(key.sym_val, interp, "outputs") ||
        nn_symbol_name_is(key.sym_val, interp, "push") ||
        nn_symbol_name_is(key.sym_val, interp, "workgroup") ||
        nn_symbol_name_is(key.sym_val, interp, "source") ||
        nn_symbol_name_is(key.sym_val, interp, "entry") ||
        nn_symbol_name_is(key.sym_val, interp, "metadata");
}

fn Value* kernel_validate_shape(Value* shape, Interp* interp, char[] path) {
    if (shape == null || shape.tag != ARRAY || shape.array_val == null) {
        return kernel_invalid(interp, path, "Array of non-negative integer or symbol dimensions", shape);
    }
    for (usz i = 0; i < shape.array_val.length; i++) {
        Value* dim = shape.array_val.items[i];
        if (dim == null ||
            !((dim.tag == INT && dim.int_val >= 0) || dim.tag == SYMBOL)) {
            return kernel_invalid(interp, path, "Array of non-negative integer or symbol dimensions", dim);
        }
    }
    return null;
}

fn Value* kernel_validate_io_descriptor(Value* descriptor, Interp* interp, char[] path) {
    if (descriptor == null || descriptor.tag != HASHMAP || descriptor.hashmap_val == null) {
        return kernel_invalid(interp, path, "Dictionary with name, dtype, and shape", descriptor);
    }
    Value* name = nn_required_field(descriptor, interp, "name", path, "symbol name");
    if (name == null || name.tag == ERROR) return name;
    if (name.tag != SYMBOL) return kernel_invalid(interp, path, "symbol name", name);
    Value* dtype = nn_required_field(descriptor, interp, "dtype", path, "dtype symbol");
    if (dtype == null || dtype.tag == ERROR) return dtype;
    if (dtype.tag != SYMBOL) return kernel_invalid(interp, path, "dtype symbol", dtype);
    Value* shape = nn_required_field(descriptor, interp, "shape", path, "Array shape");
    if (shape == null || shape.tag == ERROR) return shape;
    return kernel_validate_shape(shape, interp, path);
}

fn Value* kernel_validate_io_array(Value* values, Interp* interp, char[] path, bool allow_empty) {
    if (values == null || values.tag != ARRAY || values.array_val == null) {
        return kernel_invalid(interp, path, "Array of IO descriptor dictionaries", values);
    }
    if (!allow_empty && values.array_val.length == 0) {
        return kernel_invalid(interp, path, "non-empty Array of IO descriptor dictionaries", values);
    }
    for (usz i = 0; i < values.array_val.length; i++) {
        if (Value* err = kernel_validate_io_descriptor(values.array_val.items[i], interp, path)) return err;
    }
    return null;
}

fn Value* kernel_validate_push(Value* push, Interp* interp) {
    if (push == null || push.tag == NIL) return null;
    if (push.tag != HASHMAP || push.hashmap_val == null) {
        return kernel_invalid(interp, "kernel.push", "Dictionary of symbol keys to dtype symbols", push);
    }
    for (uint i = 0; i < push.hashmap_val.capacity; i++) {
        HashEntry* entry = &push.hashmap_val.entries[i];
        if (entry.key == null) continue;
        if (entry.key.tag != SYMBOL || entry.value == null || entry.value.tag != SYMBOL) {
            return kernel_invalid(interp, "kernel.push", "Dictionary of symbol keys to dtype symbols", entry.key);
        }
    }
    return null;
}

fn Value* kernel_validate_workgroup(Value* workgroup, Interp* interp) {
    if (workgroup == null || workgroup.tag == NIL) return null;
    if (workgroup.tag != ARRAY || workgroup.array_val == null || workgroup.array_val.length != 3) {
        return kernel_invalid(interp, "kernel.workgroup", "Array of three positive integers", workgroup);
    }
    for (usz i = 0; i < workgroup.array_val.length; i++) {
        Value* dim = workgroup.array_val.items[i];
        if (dim == null || dim.tag != INT || dim.int_val <= 0) {
            return kernel_invalid(interp, "kernel.workgroup", "Array of three positive integers", dim);
        }
    }
    return null;
}

fn Value* kernel_validate_spec(Value* spec, Interp* interp) {
    if (spec == null || spec.tag != HASHMAP || spec.hashmap_val == null) {
        return primitive_arg_type_raise(interp, "Kernel", 0, "Dictionary kernel spec", spec);
    }
    for (uint i = 0; i < spec.hashmap_val.capacity; i++) {
        HashEntry* entry = &spec.hashmap_val.entries[i];
        if (entry.key == null) continue;
        if (!kernel_key_allowed(entry.key, interp)) {
            return kernel_invalid(interp, "kernel", "recognized Kernel spec key", entry.key);
        }
    }
    Value* kind = nn_dict_get(spec, interp, "kind");
    if (kind != null && !kernel_symbol_is(kind, interp, "kernel")) {
        return kernel_invalid(interp, "kernel.kind", "kernel symbol", kind);
    }
    Value* backend = nn_required_field(spec, interp, "backend", "kernel.backend", "backend symbol");
    if (backend == null || backend.tag == ERROR) return backend;
    if (backend.tag != SYMBOL) return kernel_invalid(interp, "kernel.backend", "backend symbol", backend);
    Value* operation = nn_required_field(spec, interp, "operation", "kernel.operation", "operation symbol");
    if (operation == null || operation.tag == ERROR) return operation;
    if (operation.tag != SYMBOL) return kernel_invalid(interp, "kernel.operation", "operation symbol", operation);
    Value* inputs = nn_required_field(spec, interp, "inputs", "kernel.inputs", "Array of IO descriptors");
    if (inputs == null || inputs.tag == ERROR) return inputs;
    if (Value* err = kernel_validate_io_array(inputs, interp, "kernel.inputs", true)) return err;
    Value* outputs = nn_required_field(spec, interp, "outputs", "kernel.outputs", "non-empty Array of IO descriptors");
    if (outputs == null || outputs.tag == ERROR) return outputs;
    if (Value* err = kernel_validate_io_array(outputs, interp, "kernel.outputs", false)) return err;
    if (Value* err = kernel_validate_push(nn_dict_get(spec, interp, "push"), interp)) return err;
    return kernel_validate_workgroup(nn_dict_get(spec, interp, "workgroup"), interp);
}

fn Value* kernel_copy_optional(Value* out, Value* spec, Interp* interp, char[] key) {
    Value* value = nn_dict_get(spec, interp, key);
    if (value == null) return null;
    return nn_dict_set(out, interp, key, value);
}

fn Value* kernel_copy_optional_or_error(Value* out, Value* spec, Interp* interp, char[] key) {
    if (Value* err = kernel_copy_optional(out, spec, interp, key)) return err;
    return null;
}

fn Value* kernel_normalize(Value* spec, Interp* interp) {
    Value* out = nn_make_dict(interp, 16);
    if (out == null || out.tag == ERROR) return out;
    if (Value* err = nn_dict_set_symbol(out, interp, "kind", "kernel")) return err;
    if (Value* err = nn_dict_set(out, interp, "backend", nn_dict_get(spec, interp, "backend"))) return err;
    if (Value* err = nn_dict_set(out, interp, "operation", nn_dict_get(spec, interp, "operation"))) return err;
    if (Value* err = nn_dict_set(out, interp, "inputs", nn_dict_get(spec, interp, "inputs"))) return err;
    if (Value* err = nn_dict_set(out, interp, "outputs", nn_dict_get(spec, interp, "outputs"))) return err;
    if (Value* err = kernel_copy_optional_or_error(out, spec, interp, "push")) return err;
    if (Value* err = kernel_copy_optional_or_error(out, spec, interp, "workgroup")) return err;
    if (Value* err = kernel_copy_optional_or_error(out, spec, interp, "source")) return err;
    if (Value* err = kernel_copy_optional_or_error(out, spec, interp, "entry")) return err;
    if (Value* err = kernel_copy_optional_or_error(out, spec, interp, "metadata")) return err;
    return out;
}

fn Value* kernel_run_invalid(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", message, null);
}

fn Value* kernel_run_unsupported(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", message, null);
}

fn bool kernel_dtype_is(Value* descriptor, Interp* interp, char[] dtype_name) @inline {
    Value* dtype = nn_dict_get(descriptor, interp, "dtype");
    return dtype != null && dtype.tag == SYMBOL && nn_symbol_is(dtype, interp, dtype_name);
}

fn Value* kernel_descriptor_name(Value* descriptor, Interp* interp, char[] label) {
    Value* name = nn_dict_get(descriptor, interp, "name");
    if (name == null || name.tag != SYMBOL) return kernel_run_invalid(interp, label);
    return name;
}

fn bool kernel_shape_matches_tensor(Value* shape, TensorVal* tensor) {
    if (shape == null || shape.tag != ARRAY || shape.array_val == null || tensor == null) return false;
    if (shape.array_val.length != tensor.rank) return false;
    for (usz i = 0; i < shape.array_val.length; i++) {
        Value* dim = shape.array_val.items[i];
        if (dim == null) return false;
        if (dim.tag == INT) {
            if (dim.int_val < 0 || (ulong)dim.int_val != (ulong)tensor.shape[i]) return false;
        } else if (dim.tag != SYMBOL) {
            return false;
        }
    }
    return true;
}

fn Value* kernel_run_validate_scale_f32_spec(Value* kernel, Interp* interp, Value** out_input_descriptor, Value** out_output_descriptor) {
    Value* inputs = nn_dict_get(kernel, interp, "inputs");
    Value* outputs = nn_dict_get(kernel, interp, "outputs");
    if (inputs == null || inputs.tag != ARRAY || inputs.array_val == null || inputs.array_val.length != 1) {
        return kernel_run_unsupported(interp, "kernel/run: scale-f32 expects exactly one input descriptor");
    }
    if (outputs == null || outputs.tag != ARRAY || outputs.array_val == null || outputs.array_val.length != 1) {
        return kernel_run_unsupported(interp, "kernel/run: scale-f32 expects exactly one output descriptor");
    }
    Value* input_descriptor = inputs.array_val.items[0];
    Value* output_descriptor = outputs.array_val.items[0];
    if (!kernel_dtype_is(input_descriptor, interp, "Float32") || !kernel_dtype_is(output_descriptor, interp, "Float32")) {
        return kernel_run_unsupported(interp, "kernel/run: scale-f32 currently supports Float32 inputs and outputs");
    }
    Value* push_spec = nn_dict_get(kernel, interp, "push");
    if (push_spec == null || push_spec.tag != HASHMAP || push_spec.hashmap_val == null ||
        !nn_symbol_is(nn_dict_get(push_spec, interp, "scale"), interp, "Float32")) {
        return kernel_run_invalid(interp, "kernel/run: scale-f32 requires push Dictionary with scale Float32");
    }
    *out_input_descriptor = input_descriptor;
    *out_output_descriptor = output_descriptor;
    return null;
}

fn Value* kernel_run_scale_f32(Value* kernel, Value* inputs, Value* push, Interp* interp) {
    Value* input_descriptor = null;
    Value* output_descriptor = null;
    if (Value* err = kernel_run_validate_scale_f32_spec(kernel, interp, &input_descriptor, &output_descriptor)) return err;

    Value* input_name = kernel_descriptor_name(input_descriptor, interp, "kernel/run: scale-f32 input descriptor name is invalid");
    if (input_name == null || input_name.tag == ERROR) return input_name;
    Value* output_name = kernel_descriptor_name(output_descriptor, interp, "kernel/run: scale-f32 output descriptor name is invalid");
    if (output_name == null || output_name.tag == ERROR) return output_name;

    Value* input_value = hashmap_get(inputs.hashmap_val, input_name);
    if (input_value == null) return kernel_run_invalid(interp, "kernel/run: scale-f32 input is missing");
    Value* scale_value = nn_dict_get(push, interp, "scale");
    float scale = 0.0;
    if (scale_value == null || !tensor_value_to_float32(scale_value, &scale)) {
        return kernel_run_invalid(interp, "kernel/run: scale-f32 push scale must be representable as Float32");
    }
    if (!is_tensor(input_value)) return kernel_run_invalid(interp, "kernel/run: scale-f32 input must be a Tensor");
    if (!tensor_expr_has_device(input_value, TENSOR_DEVICE_VULKAN) || tensor_expr_has_non_vulkan_device(input_value)) {
        return kernel_run_unsupported(interp, "kernel/run: scale-f32 input must remain fully Vulkan-placed");
    }

    TensorVal* input_tensor = null;
    Value* materialized = null;
    Value* err = tensor_expr_resolve_concrete_any_device(input_value, &input_tensor, &materialized, interp, "kernel/run: scale-f32 tensor backing storage is missing");
    if (err != null) return err;
    defer if (materialized != null && materialized != input_value) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);

    if (input_tensor == null || input_tensor.device != TENSOR_DEVICE_VULKAN) {
        return kernel_run_unsupported(interp, "kernel/run: scale-f32 input must realize to Vulkan storage");
    }
    if (input_tensor.dtype != TENSOR_DTYPE_FLOAT32) {
        return kernel_run_unsupported(interp, "kernel/run: scale-f32 currently supports Float32 tensor storage");
    }
    if (input_tensor.element_count == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/run: scale-f32 empty tensors are not supported", null);
    }
    if (!tensor_has_valid_device_storage(input_tensor, TENSOR_DEVICE_VULKAN)) {
        return tensor_runtime_raise(interp, "kernel/run: scale-f32 Vulkan tensor backing storage is missing");
    }
    if (!kernel_shape_matches_tensor(nn_dict_get(input_descriptor, interp, "shape"), input_tensor) ||
        !kernel_shape_matches_tensor(nn_dict_get(output_descriptor, interp, "shape"), input_tensor)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/run: scale-f32 descriptor shape does not match input tensor", null);
    }

    void* out_device = null;
    int status = omni_tensor_backend_vulkan_ml_clip_scale_f32(
        input_tensor.device_handle,
        input_tensor.byte_len,
        input_tensor.element_count,
        scale,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "kernel/run: Vulkan scale-f32 kernel failed");
    }

    Value* output_value = ml_optimizer_vulkan_tensor_value(input_tensor, out_device, interp, "kernel/run: failed to allocate Vulkan output tensor metadata", "kernel/run: failed to allocate Vulkan output tensor value");
    if (output_value == null || output_value.tag == ERROR) return output_value;

    Value* result = nn_make_dict(interp, 2);
    if (result == null || result.tag == ERROR) return result;
    if (!hashmap_set_checked(result.hashmap_val, output_name, output_value, interp)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "kernel/run: failed to allocate output dictionary", null);
    }
    return result;
}

fn Value* prim_kernel(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "Kernel", "1 argument: spec Dictionary", args.len);
    if (Value* err = kernel_validate_spec(args[0], interp)) return err;
    if (kernel_is_value(args[0], interp)) return args[0];
    return kernel_normalize(args[0], interp);
}

fn Value* prim_kernel_run(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 3) return primitive_arity_raise(interp, "kernel/run", "3 arguments: kernel inputs push", args.len);
    if (Value* err = kernel_validate_spec(args[0], interp)) return err;
    if (!kernel_is_value(args[0], interp)) return kernel_invalid(interp, "kernel", "Kernel value", args[0]);
    if (args[1].tag != HASHMAP || args[1].hashmap_val == null) {
        return primitive_arg_type_raise(interp, "kernel/run", 1, "Dictionary of input tensors", args[1]);
    }
    if (args[2].tag != HASHMAP || args[2].hashmap_val == null) {
        return primitive_arg_type_raise(interp, "kernel/run", 2, "Dictionary of push constants", args[2]);
    }
    if (!nn_symbol_is(nn_dict_get(args[0], interp, "backend"), interp, "vulkan")) {
        return kernel_run_unsupported(interp, "kernel/run: only Vulkan Kernel execution is currently supported");
    }
    if (nn_symbol_is(nn_dict_get(args[0], interp, "operation"), interp, "scale-f32")) {
        return kernel_run_scale_f32(args[0], args[1], args[2], interp);
    }
    return kernel_run_unsupported(interp, "kernel/run: unsupported Kernel operation for Vulkan backend");
}
