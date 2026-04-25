module lisp;

import std::io;

fn bool ffi_tensor_buffer_test_error_contains(Value* value, char[] needle) {
    if (value == null || value.tag != ERROR || value.str_chars == null) return false;
    char[] msg = value.str_chars[:value.str_len];
    if (needle.len > msg.len) return false;
    for (usz i = 0; i <= msg.len - needle.len; i++) {
        bool match = true;
        for (usz j = 0; j < needle.len; j++) {
            if (msg[i + j] != needle[j]) {
                match = false;
                break;
            }
        }
        if (match) return true;
    }
    return false;
}

fn bool ffi_tensor_buffer_export_contract_for_tests(Interp* interp) {
    usz[1] shape = { 2 };
    Value* tensor = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, shape[0:1],
        "tensor-buffer: failed to allocate CPU tensor");
    if (tensor == null || tensor.tag != TENSOR || tensor.tensor_val == null) return false;

    Value* exported = foreign_runtime_tensor_buffer(tensor, interp);
    if (exported == null || exported.tag != FFI_HANDLE || exported.ffi_val == null) return false;

    ForeignRuntimeAdapter adapter;
    if (!foreign_runtime_adapter_for_handle(exported, &adapter) ||
        !foreign_runtime_adapter_supports(&adapter, FOREIGN_CAP_TENSOR_BUFFER) ||
        adapter.tensor_buffer == null) {
        return false;
    }

    Value* desc = foreign_runtime_describe_handle(exported, interp);
    if (desc == null || desc.tag != HASHMAP) return false;

    Value* kind = ffi_metadata_dict_lookup(desc, interp, "kind");
    Value* runtime = ffi_metadata_dict_lookup(desc, interp, "runtime");
    Value* name = ffi_metadata_dict_lookup(desc, interp, "name");
    Value* caps = ffi_metadata_dict_lookup(desc, interp, "capabilities");
    bool cpu_ok = kind != null && kind.tag == SYMBOL && kind.sym_val == interp.symbols.intern("buffer") &&
        runtime != null && runtime.tag == SYMBOL && runtime.sym_val == interp.symbols.intern("native") &&
        name != null && name.tag == SYMBOL && name.sym_val == interp.symbols.intern("tensor-buffer") &&
        caps != null && caps.tag == ARRAY && caps.array_val != null &&
        caps.array_val.length == 1 &&
        caps.array_val.items[0] != null &&
        caps.array_val.items[0].tag == SYMBOL &&
        caps.array_val.items[0].sym_val == interp.symbols.intern("tensor-buffer");

    Value* released = foreign_runtime_release_handle(exported, interp);
    bool cpu_release_rejected = released != null && released.tag == ERROR &&
        ffi_tensor_buffer_test_error_contains(released, "handle is not releasable");
    if (!cpu_ok || !cpu_release_rejected) return false;

    bool cuda_ok = true;
    if (omni_tensor_backend_cuda_available() != 0) {
        Value* cuda_tensor = tensor_copy_cpu_to_cuda_value(tensor, interp);
        if (cuda_tensor == null || cuda_tensor.tag != TENSOR || cuda_tensor.tensor_val == null) return false;

        Value* cuda_exported = foreign_runtime_tensor_buffer(cuda_tensor, interp);
        if (cuda_exported == null || cuda_exported.tag != FFI_HANDLE || cuda_exported.ffi_val == null) return false;

        if (!foreign_runtime_adapter_for_handle(cuda_exported, &adapter) ||
            !foreign_runtime_adapter_supports(&adapter, FOREIGN_CAP_TENSOR_BUFFER) ||
            adapter.tensor_buffer == null) {
            return false;
        }

        Value* cuda_desc = foreign_runtime_describe_handle(cuda_exported, interp);
        if (cuda_desc == null || cuda_desc.tag != HASHMAP) return false;

        Value* cuda_kind = ffi_metadata_dict_lookup(cuda_desc, interp, "kind");
        Value* cuda_runtime = ffi_metadata_dict_lookup(cuda_desc, interp, "runtime");
        Value* cuda_name = ffi_metadata_dict_lookup(cuda_desc, interp, "name");
        Value* cuda_caps = ffi_metadata_dict_lookup(cuda_desc, interp, "capabilities");
        bool cuda_desc_ok = cuda_kind != null && cuda_kind.tag == SYMBOL && cuda_kind.sym_val == interp.symbols.intern("buffer") &&
            cuda_runtime != null && cuda_runtime.tag == SYMBOL && cuda_runtime.sym_val == interp.symbols.intern("cuda") &&
            cuda_name != null && cuda_name.tag == SYMBOL && cuda_name.sym_val == interp.symbols.intern("tensor-buffer") &&
            cuda_caps != null && cuda_caps.tag == ARRAY && cuda_caps.array_val != null &&
            cuda_caps.array_val.length == 1 &&
            cuda_caps.array_val.items[0] != null &&
            cuda_caps.array_val.items[0].tag == SYMBOL &&
            cuda_caps.array_val.items[0].sym_val == interp.symbols.intern("tensor-buffer");
        Value* cuda_released = foreign_runtime_release_handle(cuda_exported, interp);
        bool cuda_release_rejected = cuda_released != null && cuda_released.tag == ERROR &&
            ffi_tensor_buffer_test_error_contains(cuda_released, "handle is not releasable");
        cuda_ok = cuda_desc_ok && cuda_release_rejected;
        tensor_free_payload(cuda_tensor.tensor_val);
        cuda_tensor.tensor_val = null;
    }
    tensor_free_payload(tensor.tensor_val);
    tensor.tensor_val = null;
    return cuda_ok;
}

fn bool ffi_tensor_buffer_export_rejects_unsupported_device_for_tests(Interp* interp) {
    usz[1] shape = { 2 };
    Value* tensor = make_tensor_uninitialized_checked(interp, TENSOR_DTYPE_FLOAT32, shape[0:1],
        "tensor-buffer: failed to allocate tensor");
    if (tensor == null || tensor.tag != TENSOR || tensor.tensor_val == null) return false;

    tensor.tensor_val.device = TENSOR_DEVICE_VULKAN;
    tensor.tensor_val.device_handle = null;
    Value* rejected = foreign_runtime_tensor_buffer(tensor, interp);
    bool rejected_ok = ffi_tensor_buffer_test_error_contains(rejected, "tensor device is not buffer-exportable");
    tensor.tensor_val.device = TENSOR_DEVICE_CPU;
    tensor_free_payload(tensor.tensor_val);
    tensor.tensor_val = null;
    return rejected_ok;
}

fn void run_advanced_ffi_system_tensor_buffer_tests(Interp* interp, int* pass, int* fail) {
    if (ffi_tensor_buffer_export_contract_for_tests(interp)) {
        io::printn("[PASS] foreign tensor buffer export contract");
        (*pass)++;
    } else {
        io::printn("[FAIL] foreign tensor buffer export contract");
        (*fail)++;
    }

    if (ffi_tensor_buffer_export_rejects_unsupported_device_for_tests(interp)) {
        io::printn("[PASS] foreign tensor buffer export rejects unsupported device");
        (*pass)++;
    } else {
        io::printn("[FAIL] foreign tensor buffer export rejects unsupported device");
        (*fail)++;
    }
}
