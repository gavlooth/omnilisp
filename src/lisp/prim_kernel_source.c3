module lisp;

import std::core::mem;

const ulong KERNEL_SOURCE_SPIRV_MAGIC = 0x07230203;

fn bool kernel_has_source(Value* kernel, Interp* interp) {
    Value* source = nn_dict_get(kernel, interp, "source");
    return source != null && source.tag != NIL;
}

fn bool kernel_source_key_allowed(Value* key, Interp* interp) {
    if (key == null || key.tag != SYMBOL) return false;
    return nn_symbol_name_is(key.sym_val, interp, "format") ||
        nn_symbol_name_is(key.sym_val, interp, "name") ||
        nn_symbol_name_is(key.sym_val, interp, "abi") ||
        nn_symbol_name_is(key.sym_val, interp, "words") ||
        nn_symbol_name_is(key.sym_val, interp, "metadata");
}

fn bool kernel_source_is_u32_word(Value* word) @inline {
    return word != null &&
        word.tag == INT &&
        word.int_val >= 0 &&
        (ulong)word.int_val <= (ulong)uint.max;
}

fn bool kernel_source_entry_is_main(Value* entry, Interp* interp) {
    if (entry == null || entry.tag == NIL) return true;
    if (entry.tag == SYMBOL) return nn_symbol_is(entry, interp, "main");
    if (entry.tag != STRING || entry.str_len != 4) return false;
    return entry.str_chars[0] == 'm' &&
        entry.str_chars[1] == 'a' &&
        entry.str_chars[2] == 'i' &&
        entry.str_chars[3] == 'n';
}

fn bool kernel_source_named_value_is(Value* value, Interp* interp, String expected) {
    if (value == null || value.tag == NIL) return false;
    if (value.tag == SYMBOL) return nn_symbol_name_is(value.sym_val, interp, expected);
    if (value.tag != STRING || value.str_len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (value.str_chars[i] != expected.ptr[i]) return false;
    }
    return true;
}

fn bool kernel_source_abi_is_scale_f32(Value* abi, Interp* interp) {
    if (abi == null || abi.tag == NIL) return true;
    return kernel_source_named_value_is(abi, interp, "source-scale-f32-v1");
}

fn bool kernel_source_abi_is_unary_f32(Value* abi, Interp* interp) {
    return kernel_source_named_value_is(abi, interp, "source-unary-f32-v1");
}

fn bool kernel_source_abi_is_binary_f32(Value* abi, Interp* interp) {
    return kernel_source_named_value_is(abi, interp, "source-binary-f32-v1");
}

fn bool kernel_source_metadata_is_binary_f32_layout(Value* metadata, Interp* interp) {
    if (metadata == null || metadata.tag == NIL) return true;
    return kernel_source_named_value_is(metadata, interp, "storage2-output1-f32-v1");
}

fn char[] kernel_source_layout_name_for_abi(Value* abi, Interp* interp) {
    if (kernel_source_abi_is_binary_f32(abi, interp)) return "storage2-output1-f32-v1";
    if (kernel_source_abi_is_unary_f32(abi, interp)) return "storage1-output1-push-unary-f32-v1";
    if (kernel_source_abi_is_scale_f32(abi, interp)) return "storage1-output1-push-scale-f32-v1";
    return "";
}

fn char[] kernel_source_push_layout_name_for_abi(Value* abi, Interp* interp) {
    if (kernel_source_abi_is_binary_f32(abi, interp)) return "count-only-f32-v1";
    if (kernel_source_abi_is_unary_f32(abi, interp)) return "count-op-f32-v1";
    if (kernel_source_abi_is_scale_f32(abi, interp)) return "count-scale-f32-v1";
    return "";
}

<*
@require metadata != null : "kernel_source_metadata_int_field_is: null metadata"
@require metadata.tag == HASHMAP : "kernel_source_metadata_int_field_is: metadata must be Dictionary"
@require metadata.hashmap_val != null : "kernel_source_metadata_int_field_is: missing metadata hashmap"
@require interp != null : "kernel_source_metadata_int_field_is: null interp"
*>
fn bool kernel_source_metadata_int_field_is(Value* metadata, Interp* interp, char[] key, long expected) {
    Value* value = nn_dict_get(metadata, interp, key);
    return value != null && value.tag == INT && value.int_val == expected;
}

fn bool kernel_source_metadata_matches_abi(Value* metadata, Value* abi, Interp* interp) {
    if (metadata == null || metadata.tag == NIL) return true;
    if (metadata.tag == SYMBOL || metadata.tag == STRING) {
        return kernel_source_named_value_is(metadata, interp, (String)kernel_source_layout_name_for_abi(abi, interp));
    }
    if (metadata.tag != HASHMAP || metadata.hashmap_val == null) return false;
    char[] layout = kernel_source_layout_name_for_abi(abi, interp);
    char[] push_layout = kernel_source_push_layout_name_for_abi(abi, interp);
    if (layout.len == 0 || push_layout.len == 0) return false;
    char[] abi_name = kernel_source_abi_is_binary_f32(abi, interp) ? "source-binary-f32-v1" :
        (kernel_source_abi_is_unary_f32(abi, interp) ? "source-unary-f32-v1" : "source-scale-f32-v1");
    long input_count = kernel_source_abi_is_binary_f32(abi, interp) ? 2 : 1;
    Value* push = nn_dict_get(metadata, interp, "push-layout");
    return nn_symbol_is(nn_dict_get(metadata, interp, "kind"), interp, "kernel-source-layout") &&
        kernel_source_metadata_int_field_is(metadata, interp, "version", 1) &&
        kernel_source_named_value_is(nn_dict_get(metadata, interp, "abi"), interp, (String)abi_name) &&
        kernel_source_named_value_is(nn_dict_get(metadata, interp, "descriptor-layout"), interp, (String)layout) &&
        kernel_source_named_value_is(nn_dict_get(metadata, interp, "dtype"), interp, "Float32") &&
        kernel_source_metadata_int_field_is(metadata, interp, "inputs", input_count) &&
        kernel_source_metadata_int_field_is(metadata, interp, "outputs", 1) &&
        (push == null || push.tag == NIL || kernel_source_named_value_is(push, interp, (String)push_layout));
}

fn bool kernel_source_abi_is_direct_spirv_supported(Value* abi, Interp* interp) {
    return kernel_source_abi_is_scale_f32(abi, interp) ||
        kernel_source_abi_is_unary_f32(abi, interp) ||
        kernel_source_abi_is_binary_f32(abi, interp);
}

fn bool kernel_source_is_registered_scale_f32(Value* source, Interp* interp) {
    if (source == null || source.tag == NIL) return false;
    if (source.tag == SYMBOL) return nn_symbol_is(source, interp, "ml-clip-scale-f32");
    if (source.tag != HASHMAP || source.hashmap_val == null) return false;
    Value* format = nn_dict_get(source, interp, "format");
    Value* name = nn_dict_get(source, interp, "name");
    return nn_symbol_is(format, interp, "builtin-spirv") &&
        nn_symbol_is(name, interp, "ml-clip-scale-f32");
}

fn bool kernel_source_is_direct_spirv_words(Value* source, Interp* interp) {
    if (source == null || source.tag != HASHMAP || source.hashmap_val == null) return false;
    return nn_symbol_is(nn_dict_get(source, interp, "format"), interp, "spirv") &&
        nn_dict_get(source, interp, "words") != null;
}

fn Value* kernel_source_direct_spirv_words(Value* source, Interp* interp) {
    if (!kernel_source_is_direct_spirv_words(source, interp)) return null;
    return nn_dict_get(source, interp, "words");
}

<*
@require interp != null : "kernel_validate_spirv_words: null interp"
*>
fn Value* kernel_validate_spirv_words(Value* words, Interp* interp) {
    if (words == null || words.tag != ARRAY || words.array_val == null || words.array_val.length < 5) {
        return kernel_invalid(interp, "kernel.source.words", "SPIR-V word Array with at least five unsigned 32-bit integer words", words);
    }
    foreach (word : words.array_val.items[0:words.array_val.length]) {
        if (!kernel_source_is_u32_word(word)) {
            return kernel_invalid(interp, "kernel.source.words", "Array of unsigned 32-bit integer words", word);
        }
    }
    Value* magic = words.array_val.items[0];
    if ((ulong)magic.int_val != KERNEL_SOURCE_SPIRV_MAGIC) {
        return kernel_invalid(interp, "kernel.source.words", "SPIR-V binary header with magic word 0x07230203", magic);
    }
    Value* version = words.array_val.items[1];
    if (version.int_val <= 0) {
        return kernel_invalid(interp, "kernel.source.words", "SPIR-V binary header with positive version word", version);
    }
    Value* bound = words.array_val.items[3];
    if (bound.int_val <= 0) {
        return kernel_invalid(interp, "kernel.source.words", "SPIR-V binary header with positive id bound", bound);
    }
    Value* schema = words.array_val.items[4];
    if (schema.int_val != 0) {
        return kernel_invalid(interp, "kernel.source.words", "SPIR-V binary header with reserved schema word 0", schema);
    }
    return null;
}

<*
@require interp != null : "kernel_copy_spirv_words: null interp"
@require out_words != null : "kernel_copy_spirv_words: null out_words"
@require out_count != null : "kernel_copy_spirv_words: null out_count"
*>
fn Value* kernel_copy_spirv_words(Value* words, Interp* interp, uint** out_words, usz* out_count) {
    if (out_words == null || out_count == null) return nn_oom(interp);
    *out_words = null;
    *out_count = 0;
    if (Value* err = kernel_validate_spirv_words(words, interp)) return err;
    uint* copied = mem::new_array(uint, words.array_val.length);
    if (copied == null) return nn_oom(interp);
    foreach (i, word : words.array_val.items[0:words.array_val.length]) {
        copied[i] = (uint)word.int_val;
    }
    *out_words = copied;
    *out_count = words.array_val.length;
    return null;
}

<*
@require source != null : "kernel_validate_source_dict: null source"
@require source.tag == HASHMAP : "kernel_validate_source_dict: source must be Dictionary"
@require source.hashmap_val != null : "kernel_validate_source_dict: missing source hashmap"
@require interp != null : "kernel_validate_source_dict: null interp"
*>
fn Value* kernel_validate_source_dict(Value* source, Interp* interp) {
    foreach (entry : source.hashmap_val.entries[0:source.hashmap_val.capacity]) {
        if (entry.key == null) continue;
        if (!kernel_source_key_allowed(entry.key, interp)) {
            return kernel_invalid(interp, "kernel.source", "recognized Kernel source key", entry.key);
        }
    }
    Value* format = nn_required_field(source, interp, "format", "kernel.source.format", "source format symbol");
    if (format == null || format.tag == ERROR) return format;
    if (format.tag != SYMBOL) return kernel_invalid(interp, "kernel.source.format", "source format symbol", format);
    if (nn_symbol_is(format, interp, "builtin-spirv")) {
        Value* name = nn_required_field(source, interp, "name", "kernel.source.name", "registered builtin SPIR-V source name");
        if (name == null || name.tag == ERROR) return name;
        if (!nn_symbol_is(name, interp, "ml-clip-scale-f32") &&
            !nn_symbol_is(name, interp, "map-unary-f32")) {
            return kernel_invalid(interp, "kernel.source.name", "registered builtin SPIR-V source name", name);
        }
        return null;
    }
    if (nn_symbol_is(format, interp, "spirv")) {
        Value* words = nn_required_field(source, interp, "words", "kernel.source.words", "SPIR-V word Array");
        if (words == null || words.tag == ERROR) return words;
        if (Value* err = kernel_validate_spirv_words(words, interp)) return err;
        Value* abi = nn_dict_get(source, interp, "abi");
        if (!kernel_source_abi_is_direct_spirv_supported(abi, interp)) {
            return kernel_invalid(interp, "kernel.source.abi", "source-scale-f32-v1, source-unary-f32-v1, or source-binary-f32-v1", abi);
        }
        if (!kernel_source_metadata_matches_abi(nn_dict_get(source, interp, "metadata"), abi, interp)) {
            return kernel_invalid(interp, "kernel.source.metadata", "kernel-source-layout version 1 matching the direct SPIR-V ABI", nn_dict_get(source, interp, "metadata"));
        }
        return null;
    }
    return kernel_invalid(interp, "kernel.source.format", "builtin-spirv or spirv", format);
}

fn Value* kernel_validate_source(Value* source, Interp* interp) {
    if (source == null || source.tag == NIL) return null;
    if (source.tag == SYMBOL) {
        if (nn_symbol_is(source, interp, "ml-clip-scale-f32") ||
            nn_symbol_is(source, interp, "map-unary-f32")) return null;
        return kernel_invalid(interp, "kernel.source", "registered Kernel source symbol", source);
    }
    if (source.tag != HASHMAP || source.hashmap_val == null) {
        return kernel_invalid(interp, "kernel.source", "registered source symbol or source Dictionary", source);
    }
    return kernel_validate_source_dict(source, interp);
}

fn uint kernel_source_local_size(Value* kernel, Interp* interp) {
    Value* workgroup = nn_dict_get(kernel, interp, "workgroup");
    if (workgroup == null || workgroup.tag != ARRAY || workgroup.array_val == null || workgroup.array_val.length == 0) {
        return 64;
    }
    Value* x = workgroup.array_val.items[0];
    if (x == null || x.tag != INT || x.int_val <= 0) return 64;
    return (uint)x.int_val;
}

<*
@require kernel != null : "kernel_run_source_scale_f32: null kernel"
@require kernel.tag == HASHMAP : "kernel_run_source_scale_f32: kernel must be Dictionary"
@require kernel.hashmap_val != null : "kernel_run_source_scale_f32: missing kernel hashmap"
@require inputs != null : "kernel_run_source_scale_f32: null inputs"
@require inputs.tag == HASHMAP : "kernel_run_source_scale_f32: inputs must be Dictionary"
@require inputs.hashmap_val != null : "kernel_run_source_scale_f32: missing inputs hashmap"
@require push != null : "kernel_run_source_scale_f32: null push"
@require push.tag == HASHMAP : "kernel_run_source_scale_f32: push must be Dictionary"
@require push.hashmap_val != null : "kernel_run_source_scale_f32: missing push hashmap"
@require interp != null : "kernel_run_source_scale_f32: null interp"
*>
fn Value* kernel_run_source_scale_f32(Value* kernel, Value* inputs, Value* push, Interp* interp) {
    if (!kernel_source_entry_is_main(nn_dict_get(kernel, interp, "entry"), interp)) {
        return kernel_run_unsupported(interp, "kernel/run: source-backed Vulkan kernels currently require entry main");
    }
    Value* source = nn_dict_get(kernel, interp, "source");
    Value* direct_words = kernel_source_direct_spirv_words(source, interp);
    bool direct_spirv = direct_words != null;
    if (!direct_spirv && !kernel_source_is_registered_scale_f32(source, interp)) {
        return kernel_run_unsupported(interp, "kernel/run: unsupported Vulkan Kernel source");
    }
    if (direct_spirv && !kernel_source_abi_is_scale_f32(nn_dict_get(source, interp, "abi"), interp)) {
        return kernel_run_unsupported(interp, "kernel/run: unsupported direct SPIR-V Kernel ABI");
    }
    if (direct_spirv && !kernel_source_metadata_matches_abi(nn_dict_get(source, interp, "metadata"), nn_dict_get(source, interp, "abi"), interp)) {
        return kernel_run_unsupported(interp, "kernel/run: direct SPIR-V source-scale-f32 metadata does not match source-scale-f32-v1 ABI");
    }

    Value* input_descriptor = null;
    Value* output_descriptor = null;
    if (Value* err = kernel_run_validate_scale_f32_spec(kernel, interp, &input_descriptor, &output_descriptor)) return err;

    Value* input_name = kernel_descriptor_name(input_descriptor, interp, "kernel/run: source-scale-f32 input descriptor name is invalid");
    if (input_name == null || input_name.tag == ERROR) return input_name;
    Value* output_name = kernel_descriptor_name(output_descriptor, interp, "kernel/run: source-scale-f32 output descriptor name is invalid");
    if (output_name == null || output_name.tag == ERROR) return output_name;

    Value* input_value = hashmap_get(inputs.hashmap_val, input_name);
    if (input_value == null) return kernel_run_invalid(interp, "kernel/run: source-scale-f32 input is missing");
    Value* scale_value = nn_dict_get(push, interp, "scale");
    float scale = 0.0;
    if (scale_value == null || !tensor_value_to_float32(scale_value, &scale)) {
        return kernel_run_invalid(interp, "kernel/run: source-scale-f32 push scale must be representable as Float32");
    }

    TensorVal* input_tensor = null;
    Value* materialized = null;
    Value* err = kernel_resolve_vulkan_f32_input(input_value, &input_tensor, &materialized, interp, "kernel/run: source-scale-f32 tensor backing storage is missing");
    if (err != null) return err;
    defer if (materialized != null && materialized != input_value) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);

    if (!kernel_shape_matches_tensor(nn_dict_get(input_descriptor, interp, "shape"), input_tensor) ||
        !kernel_shape_matches_tensor(nn_dict_get(output_descriptor, interp, "shape"), input_tensor)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/run: source-scale-f32 descriptor shape does not match input tensor", null);
    }

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_INVALID;
    uint* shader_words = null;
    usz shader_word_count = 0;
    defer if (shader_words != null) mem::free(shader_words);
    if (direct_spirv) {
        if (Value* copy_err = kernel_copy_spirv_words(direct_words, interp, &shader_words, &shader_word_count)) return copy_err;
        status = omni_tensor_backend_vulkan_kernel_source_scale_f32_spirv(
            input_tensor.device_handle,
            input_tensor.byte_len,
            input_tensor.element_count,
            scale,
            kernel_source_local_size(kernel, interp),
            shader_words,
            shader_word_count,
            &out_device
        );
    } else {
        status = omni_tensor_backend_vulkan_kernel_source_scale_f32(
            input_tensor.device_handle,
            input_tensor.byte_len,
            input_tensor.element_count,
            scale,
            kernel_source_local_size(kernel, interp),
            &out_device
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "kernel/run: Vulkan source-scale-f32 kernel failed");
    }

    Value* output_value = ml_optimizer_vulkan_tensor_value(input_tensor, out_device, interp, "kernel/run: failed to allocate Vulkan output tensor metadata", "kernel/run: failed to allocate Vulkan output tensor value");
    if (output_value == null || output_value.tag == ERROR) return output_value;
    return kernel_output_dict(output_name, output_value, interp);
}

<*
@require kernel != null : "kernel_run_source_unary_f32: null kernel"
@require kernel.tag == HASHMAP : "kernel_run_source_unary_f32: kernel must be Dictionary"
@require kernel.hashmap_val != null : "kernel_run_source_unary_f32: missing kernel hashmap"
@require inputs != null : "kernel_run_source_unary_f32: null inputs"
@require inputs.tag == HASHMAP : "kernel_run_source_unary_f32: inputs must be Dictionary"
@require inputs.hashmap_val != null : "kernel_run_source_unary_f32: missing inputs hashmap"
@require push != null : "kernel_run_source_unary_f32: null push"
@require push.tag == HASHMAP : "kernel_run_source_unary_f32: push must be Dictionary"
@require push.hashmap_val != null : "kernel_run_source_unary_f32: missing push hashmap"
@require interp != null : "kernel_run_source_unary_f32: null interp"
*>
fn Value* kernel_run_source_unary_f32(Value* kernel, Value* inputs, Value* push, Interp* interp, uint op) {
    if (push.hashmap_val.count != 0) return kernel_run_invalid(interp, "kernel/run: source unary Float32 kernels do not accept push constants");
    if (!kernel_source_entry_is_main(nn_dict_get(kernel, interp, "entry"), interp)) {
        return kernel_run_unsupported(interp, "kernel/run: source-backed Vulkan kernels currently require entry main");
    }
    Value* source = nn_dict_get(kernel, interp, "source");
    Value* direct_words = kernel_source_direct_spirv_words(source, interp);
    bool direct_spirv = direct_words != null;
    bool registered_unary = source != null && (
        nn_symbol_is(source, interp, "map-unary-f32") ||
        (source.tag == HASHMAP &&
            nn_symbol_is(nn_dict_get(source, interp, "format"), interp, "builtin-spirv") &&
            nn_symbol_is(nn_dict_get(source, interp, "name"), interp, "map-unary-f32")));
    if (direct_spirv && !kernel_source_abi_is_unary_f32(nn_dict_get(source, interp, "abi"), interp)) {
        return kernel_run_unsupported(interp, "kernel/run: unsupported direct SPIR-V Kernel ABI for unary Float32 operation");
    }
    if (direct_spirv && !kernel_source_metadata_matches_abi(nn_dict_get(source, interp, "metadata"), nn_dict_get(source, interp, "abi"), interp)) {
        return kernel_run_unsupported(interp, "kernel/run: direct SPIR-V unary Float32 metadata does not match source-unary-f32-v1 ABI");
    }
    if (!direct_spirv && !registered_unary) {
        return kernel_run_unsupported(interp, "kernel/run: unsupported Vulkan source unary Kernel source");
    }

    Value* input_descriptor = null;
    Value* output_descriptor = null;
    if (Value* err = kernel_run_validate_unary_f32_spec(kernel, interp, &input_descriptor, &output_descriptor)) return err;

    Value* input_value = kernel_input_by_descriptor(inputs, input_descriptor, interp, "kernel/run: source unary Float32 input descriptor name is invalid", "kernel/run: source unary Float32 input is missing");
    if (input_value == null || input_value.tag == ERROR) return input_value;
    Value* output_name = kernel_descriptor_name(output_descriptor, interp, "kernel/run: source unary Float32 output descriptor name is invalid");
    if (output_name == null || output_name.tag == ERROR) return output_name;

    TensorVal* input_tensor = null;
    Value* materialized = null;
    Value* err = kernel_resolve_vulkan_f32_input(input_value, &input_tensor, &materialized, interp, "kernel/run: source unary Float32 tensor backing storage is missing");
    if (err != null) return err;
    defer if (materialized != null && materialized != input_value) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);

    if (!kernel_shape_matches_tensor(nn_dict_get(input_descriptor, interp, "shape"), input_tensor) ||
        !kernel_shape_matches_tensor(nn_dict_get(output_descriptor, interp, "shape"), input_tensor)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/run: source unary Float32 descriptor shape does not match input tensor", null);
    }

    void* out_device = null;
    int status = OMNI_TENSOR_VULKAN_INVALID;
    uint* shader_words = null;
    usz shader_word_count = 0;
    defer if (shader_words != null) mem::free(shader_words);
    if (direct_spirv) {
        if (Value* copy_err = kernel_copy_spirv_words(direct_words, interp, &shader_words, &shader_word_count)) return copy_err;
        status = omni_tensor_backend_vulkan_kernel_source_unary_f32_spirv(
            input_tensor.device_handle,
            input_tensor.byte_len,
            input_tensor.element_count,
            op,
            kernel_source_local_size(kernel, interp),
            shader_words,
            shader_word_count,
            &out_device
        );
    } else {
        status = omni_tensor_backend_vulkan_kernel_source_unary_f32(
            input_tensor.device_handle,
            input_tensor.byte_len,
            input_tensor.element_count,
            op,
            kernel_source_local_size(kernel, interp),
            &out_device
        );
    }
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "kernel/run: Vulkan source unary Float32 kernel failed");
    }

    Value* output_value = ml_optimizer_vulkan_tensor_value(input_tensor, out_device, interp, "kernel/run: failed to allocate Vulkan output tensor metadata", "kernel/run: failed to allocate Vulkan output tensor value");
    if (output_value == null || output_value.tag == ERROR) return output_value;
    return kernel_output_dict(output_name, output_value, interp);
}

<*
@require kernel != null : "kernel_run_source_binary_f32: null kernel"
@require kernel.tag == HASHMAP : "kernel_run_source_binary_f32: kernel must be Dictionary"
@require kernel.hashmap_val != null : "kernel_run_source_binary_f32: missing kernel hashmap"
@require inputs != null : "kernel_run_source_binary_f32: null inputs"
@require inputs.tag == HASHMAP : "kernel_run_source_binary_f32: inputs must be Dictionary"
@require inputs.hashmap_val != null : "kernel_run_source_binary_f32: missing inputs hashmap"
@require push != null : "kernel_run_source_binary_f32: null push"
@require push.tag == HASHMAP : "kernel_run_source_binary_f32: push must be Dictionary"
@require push.hashmap_val != null : "kernel_run_source_binary_f32: missing push hashmap"
@require interp != null : "kernel_run_source_binary_f32: null interp"
*>
fn Value* kernel_run_source_binary_f32(Value* kernel, Value* inputs, Value* push, Interp* interp) {
    if (push.hashmap_val.count != 0) return kernel_run_invalid(interp, "kernel/run: source binary Float32 kernels do not accept push constants");
    if (!kernel_source_entry_is_main(nn_dict_get(kernel, interp, "entry"), interp)) {
        return kernel_run_unsupported(interp, "kernel/run: source-backed Vulkan kernels currently require entry main");
    }
    Value* source = nn_dict_get(kernel, interp, "source");
    Value* direct_words = kernel_source_direct_spirv_words(source, interp);
    if (direct_words == null || !kernel_source_abi_is_binary_f32(nn_dict_get(source, interp, "abi"), interp)) {
        return kernel_run_unsupported(interp, "kernel/run: unsupported direct SPIR-V Kernel ABI for binary Float32 operation");
    }
    if (!kernel_source_metadata_matches_abi(nn_dict_get(source, interp, "metadata"), nn_dict_get(source, interp, "abi"), interp)) {
        return kernel_run_unsupported(interp, "kernel/run: unsupported direct SPIR-V binary Float32 descriptor layout");
    }

    Value* left_descriptor = null;
    Value* right_descriptor = null;
    Value* output_descriptor = null;
    if (Value* err = kernel_run_validate_binary_f32_spec(kernel, interp, &left_descriptor, &right_descriptor, &output_descriptor)) return err;

    Value* left_value = kernel_input_by_descriptor(inputs, left_descriptor, interp, "kernel/run: source binary Float32 left input descriptor name is invalid", "kernel/run: source binary Float32 left input is missing");
    if (left_value == null || left_value.tag == ERROR) return left_value;
    Value* right_value = kernel_input_by_descriptor(inputs, right_descriptor, interp, "kernel/run: source binary Float32 right input descriptor name is invalid", "kernel/run: source binary Float32 right input is missing");
    if (right_value == null || right_value.tag == ERROR) return right_value;
    Value* output_name = kernel_descriptor_name(output_descriptor, interp, "kernel/run: source binary Float32 output descriptor name is invalid");
    if (output_name == null || output_name.tag == ERROR) return output_name;

    TensorVal* left_tensor = null;
    TensorVal* right_tensor = null;
    Value* left_materialized = null;
    Value* right_materialized = null;
    Value* err = kernel_resolve_vulkan_f32_input(left_value, &left_tensor, &left_materialized, interp, "kernel/run: source binary Float32 left tensor backing storage is missing");
    if (err != null) return err;
    defer if (left_materialized != null && left_materialized != left_value) boundary_cleanup_materialized_value(null, left_materialized, interp.current_scope);
    err = kernel_resolve_vulkan_f32_input(right_value, &right_tensor, &right_materialized, interp, "kernel/run: source binary Float32 right tensor backing storage is missing");
    if (err != null) return err;
    defer if (right_materialized != null && right_materialized != right_value) boundary_cleanup_materialized_value(null, right_materialized, interp.current_scope);

    if (!ml_sgd_tensor_shapes_match(left_tensor, right_tensor)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/run: source binary Float32 input tensor shapes must match", null);
    }
    if (!kernel_shape_matches_tensor(nn_dict_get(left_descriptor, interp, "shape"), left_tensor) ||
        !kernel_shape_matches_tensor(nn_dict_get(right_descriptor, interp, "shape"), right_tensor) ||
        !kernel_shape_matches_tensor(nn_dict_get(output_descriptor, interp, "shape"), left_tensor)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "kernel/run: source binary Float32 descriptor shape does not match input tensors", null);
    }

    void* out_device = null;
    uint* shader_words = null;
    usz shader_word_count = 0;
    defer if (shader_words != null) mem::free(shader_words);
    if (Value* copy_err = kernel_copy_spirv_words(direct_words, interp, &shader_words, &shader_word_count)) return copy_err;
    int status = omni_tensor_backend_vulkan_kernel_source_binary_f32_spirv(
        left_tensor.device_handle,
        left_tensor.byte_len,
        right_tensor.device_handle,
        right_tensor.byte_len,
        left_tensor.element_count,
        kernel_source_local_size(kernel, interp),
        shader_words,
        shader_word_count,
        &out_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        if (out_device != null) omni_tensor_backend_vulkan_free(out_device);
        return tensor_vulkan_status_raise(interp, status, "kernel/run: Vulkan source binary Float32 kernel failed");
    }

    Value* output_value = ml_optimizer_vulkan_tensor_value(left_tensor, out_device, interp, "kernel/run: failed to allocate Vulkan output tensor metadata", "kernel/run: failed to allocate Vulkan output tensor value");
    if (output_value == null || output_value.tag == ERROR) return output_value;
    return kernel_output_dict(output_name, output_value, interp);
}

<*
@require kernel != null : "kernel_run_source_f32: null kernel"
@require kernel.tag == HASHMAP : "kernel_run_source_f32: kernel must be Dictionary"
@require kernel.hashmap_val != null : "kernel_run_source_f32: missing kernel hashmap"
@require inputs != null : "kernel_run_source_f32: null inputs"
@require inputs.tag == HASHMAP : "kernel_run_source_f32: inputs must be Dictionary"
@require inputs.hashmap_val != null : "kernel_run_source_f32: missing inputs hashmap"
@require push != null : "kernel_run_source_f32: null push"
@require push.tag == HASHMAP : "kernel_run_source_f32: push must be Dictionary"
@require push.hashmap_val != null : "kernel_run_source_f32: missing push hashmap"
@require interp != null : "kernel_run_source_f32: null interp"
*>
fn Value* kernel_run_source_f32(Value* kernel, Value* inputs, Value* push, Interp* interp) {
    Value* operation = nn_dict_get(kernel, interp, "operation");
    if (nn_symbol_is(operation, interp, "source-scale-f32") ||
        nn_symbol_is(operation, interp, "scale-f32")) {
        return kernel_run_source_scale_f32(kernel, inputs, push, interp);
    }
    uint unary_op = 0;
    if (kernel_unary_f32_operation(operation, interp, &unary_op)) {
        return kernel_run_source_unary_f32(kernel, inputs, push, interp, unary_op);
    }
    uint binary_op = 0;
    if (kernel_binary_f32_operation(operation, interp, &binary_op)) {
        return kernel_run_source_binary_f32(kernel, inputs, push, interp);
    }
    return kernel_run_unsupported(interp, "kernel/run: unsupported source-backed Vulkan Kernel operation");
}
