module lisp;

import std::core::mem;
import main;

fn Value* prim_tensor_layout(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "tensor-layout", "1 argument: tensor", args.len);
    if (!is_tensor(args[0])) return raise_error(interp, "tensor-layout: expected Tensor");
    TensorVal* tensor = args[0].tensor_val;
    if (tensor == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "tensor-layout: tensor backing storage is missing", null);
    if (!tensor_has_valid_metadata(tensor)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "tensor-layout: tensor metadata is invalid", null);
    }

    Value* out = make_hashmap_checked(interp, 32, "tensor-layout: out of memory");
    if (out == null || out.tag == ERROR) return out;
    Value* err = null;

    err = tensor_layout_set_field(interp, out, "dtype", make_symbol(interp, tensor_dtype_symbol(tensor.dtype, interp)));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "device", make_symbol(interp, tensor_device_symbol(tensor.device, interp)));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "payload", make_symbol(interp, tensor_payload_kind_symbol(tensor.kind, interp)));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "layout", make_symbol(interp, tensor_dense_row_major(tensor) ? interp.symbols.intern("dense-row-major") : interp.symbols.intern("strided")));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "dense-row-major", make_symbol(interp, tensor_dense_row_major(tensor) ? interp.sym_true : interp.sym_false));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "shape", tensor_metadata_usz_array(interp, tensor.shape, tensor.rank, "tensor-layout: failed to allocate shape array", "tensor-layout: tensor dimension is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "strides", tensor_metadata_usz_array(interp, tensor.strides, tensor.rank, "tensor-layout: failed to allocate strides array", "tensor-layout: tensor stride is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "rank", tensor_usz_metadata_value(interp, tensor.rank, "tensor-layout: tensor rank is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "element-count", tensor_usz_metadata_value(interp, tensor.element_count, "tensor-layout: tensor element count is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "byte-length", tensor_usz_metadata_value(interp, tensor.byte_len, "tensor-layout: tensor byte length is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "storage-offset", tensor_usz_metadata_value(interp, tensor.storage_offset, "tensor-layout: tensor storage offset is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "storage-elements", tensor_usz_metadata_value(interp, tensor.storage_element_count, "tensor-layout: tensor storage element count is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "storage-bytes", tensor_usz_metadata_value(interp, tensor.storage_byte_len, "tensor-layout: tensor storage byte length is too large"));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "is-view", make_symbol(interp, tensor.is_view ? interp.sym_true : interp.sym_false));
    if (err != null) return err;
    bool owns_storage = tensor.kind == TENSOR_PAYLOAD_CONCRETE && !tensor.is_view;
    err = tensor_layout_set_field(interp, out, "owns-storage", make_symbol(interp, owns_storage ? interp.sym_true : interp.sym_false));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "owner", make_symbol(interp, tensor_owner_symbol(tensor, interp)));
    if (err != null) return err;
    err = tensor_layout_set_field(interp, out, "write-policy", make_symbol(interp, tensor_write_policy_symbol(tensor, interp)));
    if (err != null) return err;
    return out;
}

fn void tensor_cuda_device_finalizer(void* handle) {
    omni_tensor_backend_cuda_free(handle);
}

fn void tensor_vulkan_device_finalizer(void* handle) {
    omni_tensor_backend_vulkan_free(handle);
}

fn Value* tensor_backend_entry(
    Interp* interp,
    SymbolId name,
    bool available,
    SymbolId placement,
    SymbolId reason
) {
    Value* entry = make_hashmap_checked(interp, 32, "tensor-backends: out of memory");
    if (entry == null || entry.tag == ERROR) return entry;
    Value* available_value = available ? make_symbol(interp, interp.sym_true) : make_symbol(interp, interp.sym_false);
    if (!hashmap_set_checked(entry.hashmap_val, make_symbol(interp, interp.symbols.intern("name")), make_symbol(interp, name), interp) ||
        !hashmap_set_checked(entry.hashmap_val, make_symbol(interp, interp.symbols.intern("available")), available_value, interp) ||
        !hashmap_set_checked(entry.hashmap_val, make_symbol(interp, interp.symbols.intern("device")), make_symbol(interp, placement), interp)) {
        return raise_error(interp, "tensor-backends: out of memory while building backend entry");
    }
    if (reason != INVALID_SYMBOL_ID &&
        !hashmap_set_checked(entry.hashmap_val, make_symbol(interp, interp.symbols.intern("reason")), make_symbol(interp, reason), interp)) {
        return raise_error(interp, "tensor-backends: out of memory while building backend entry");
    }
    return entry;
}

fn Value* tensor_backend_set_bool_field(Interp* interp, Value* entry, char[] field_name, bool value) {
    if (entry == null || entry.tag == ERROR) return entry;
    Value* field_value = value ? make_symbol(interp, interp.sym_true) : make_symbol(interp, interp.sym_false);
    if (!hashmap_set_checked(entry.hashmap_val, make_symbol(interp, interp.symbols.intern(field_name)), field_value, interp)) {
        return raise_error(interp, "tensor-backends: out of memory while building backend entry");
    }
    return null;
}

fn Value* tensor_backend_set_ml_capability_fields(
    Interp* interp,
    Value* entry,
    bool linear_available,
    bool linear_direct_float64_available,
    bool linear_direct_float32_available,
    bool neural_relu_float64_available,
    bool neural_relu_float32_available,
    bool neural_sigmoid_float64_available,
    bool neural_sigmoid_float32_available,
    bool neural_tanh_float64_available,
    bool neural_tanh_float32_available,
    bool neural_gelu_float64_available,
    bool neural_gelu_float32_available,
    bool reduction_float64_available,
    bool reduction_float32_available,
    bool conv1d_direct_float64_available,
    bool conv1d_direct_float32_available,
    bool conv2d_direct_float64_available,
    bool conv2d_direct_float32_available
) {
    Value* err = tensor_backend_set_bool_field(interp, entry, "ml-linear", linear_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-linear-direct-float64", linear_direct_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-linear-direct-float32", linear_direct_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-neural-relu-float64", neural_relu_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-neural-relu-float32", neural_relu_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-neural-sigmoid-float64", neural_sigmoid_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-neural-sigmoid-float32", neural_sigmoid_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-neural-tanh-float64", neural_tanh_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-neural-tanh-float32", neural_tanh_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-neural-gelu-float64", neural_gelu_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-neural-gelu-float32", neural_gelu_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-reduction-float64", reduction_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-reduction-float32", reduction_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-convolution", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-conv1d-direct-float64", conv1d_direct_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-conv1d-direct-float32", conv1d_direct_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-conv2d-direct-float64", conv2d_direct_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-conv2d-direct-float32", conv2d_direct_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-neural-map", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-normalization", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-attention", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-autograd", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-optimizer", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, entry, "ml-graph-execution", false);
    if (err != null) return err;
    return null;
}

fn Value* prim_tensor_backends(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 0) return primitive_arity_raise(interp, "tensor-backends", "no arguments", args.len);

    SymbolId cpu_sym = interp.symbols.intern("cpu");
    SymbolId cuda_sym = interp.symbols.intern("cuda");
    SymbolId cublas_sym = interp.symbols.intern("cublas");
    SymbolId vulkan_sym = interp.symbols.intern("vulkan");
    bool cuda_available = omni_tensor_backend_cuda_available() != 0;
    bool cuda_map_available = omni_tensor_backend_cuda_map_available() != 0;
    bool cuda_scientific_map_available = omni_tensor_backend_cuda_scientific_map_available() != 0;
    bool cuda_complex_map_available = omni_tensor_backend_cuda_complex_map_available() != 0;
    bool cuda_complex_matrix_available = omni_tensor_backend_cuda_complex_matrix_available() != 0;
    bool cuda_rounding_i64_available = omni_tensor_backend_cuda_rounding_i64_available() != 0;
    bool cublas_available = omni_tensor_backend_cublas_available() != 0;
    bool cublas_float32_available = omni_tensor_backend_cublas_float32_available() != 0;
    bool cublas_complex_available = omni_tensor_backend_cublas_complex_available() != 0;
    bool vulkan_available = omni_tensor_backend_vulkan_available() != 0;
    bool vulkan_float64_available = omni_tensor_backend_vulkan_float64_available() != 0;
    bool vulkan_rounding_i64_available = omni_tensor_backend_vulkan_rounding_i64_available() != 0;
    bool vulkan_float32_available = omni_tensor_backend_vulkan_float32_available() != 0;

    Value* vulkan_entry = tensor_backend_entry(
        interp,
        vulkan_sym,
        vulkan_available,
        vulkan_sym,
        vulkan_available ? INVALID_SYMBOL_ID : interp.symbols.intern("backend-unavailable")
    );
    if (vulkan_entry == null || vulkan_entry.tag == ERROR) return vulkan_entry;
    Value* err = tensor_backend_set_bool_field(interp, vulkan_entry, "float64", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "float32", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "complex128", vulkan_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "complex64", vulkan_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "elementwise-map-float64", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "elementwise-map-float32", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "elementwise-map-complex128", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "elementwise-map-complex64", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "contract-complex128", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "contract-complex64", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "matrix-structural-complex128", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "matrix-structural-complex64", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "matrix-numerical-complex128", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "matrix-numerical-complex64", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "scientific-map-float64", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "scientific-map-float32", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "stats-normal-float64", vulkan_float64_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "stats-normal-float32", vulkan_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, vulkan_entry, "rounding-big-integer", vulkan_rounding_i64_available);
    if (err != null) return err;
    err = tensor_backend_set_ml_capability_fields(interp, vulkan_entry, false, vulkan_float64_available, vulkan_float32_available, vulkan_float64_available, vulkan_float32_available, false, vulkan_float32_available, false, vulkan_float32_available, false, vulkan_float32_available, vulkan_float64_available, vulkan_float32_available, false, vulkan_float32_available, false, vulkan_float32_available);
    if (err != null) return err;
    Value* result = make_cons_or_error(interp, vulkan_entry, make_nil(interp), "tensor-backends: failed to allocate result list");
    if (result == null || result.tag == ERROR) return result;

    Value* cublas_entry = tensor_backend_entry(
        interp,
        cublas_sym,
        cublas_available,
        cuda_sym,
        cublas_available ? INVALID_SYMBOL_ID : interp.symbols.intern("backend-unavailable")
    );
    if (cublas_entry == null || cublas_entry.tag == ERROR) return cublas_entry;
    err = tensor_backend_set_bool_field(interp, cublas_entry, "float64", cublas_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cublas_entry, "float32", cublas_float32_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cublas_entry, "stats-normal-float64", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cublas_entry, "stats-normal-float32", false);
    if (err != null) return err;
    err = tensor_backend_set_ml_capability_fields(interp, cublas_entry, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false);
    if (err != null) return err;
    result = make_cons_or_error(interp, cublas_entry, result, "tensor-backends: failed to allocate result list");
    if (result == null || result.tag == ERROR) return result;

    Value* cuda_entry = tensor_backend_entry(
        interp,
        cuda_sym,
        cuda_available,
        cuda_sym,
        cuda_available ? INVALID_SYMBOL_ID : interp.symbols.intern("backend-unavailable")
    );
    if (cuda_entry == null || cuda_entry.tag == ERROR) return cuda_entry;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "float64", cuda_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "float32", cuda_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "complex128", cuda_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "complex64", cuda_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "elementwise-map-float64", cuda_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "elementwise-map-float32", cuda_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "elementwise-map-complex128", cuda_complex_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "elementwise-map-complex64", cuda_complex_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "contract-complex128", cublas_complex_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "contract-complex64", cublas_complex_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "matrix-structural-complex128", cuda_complex_matrix_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "matrix-structural-complex64", cuda_complex_matrix_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "matrix-numerical-complex128", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "matrix-numerical-complex64", false);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "scientific-map-float64", cuda_scientific_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "scientific-map-float32", cuda_scientific_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "stats-normal-float64", cuda_scientific_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "stats-normal-float32", cuda_scientific_map_available);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cuda_entry, "rounding-big-integer", cuda_rounding_i64_available);
    if (err != null) return err;
    err = tensor_backend_set_ml_capability_fields(interp, cuda_entry, false, false, false, cuda_map_available, cuda_map_available, false, cuda_scientific_map_available, false, cuda_scientific_map_available, false, cuda_scientific_map_available, false, false, false, false, false, false);
    if (err != null) return err;
    result = make_cons_or_error(interp, cuda_entry, result, "tensor-backends: failed to allocate result list");
    if (result == null || result.tag == ERROR) return result;

    Value* cpu_entry = tensor_backend_entry(interp, cpu_sym, true, cpu_sym, INVALID_SYMBOL_ID);
    if (cpu_entry == null || cpu_entry.tag == ERROR) return cpu_entry;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "elementwise-map-float64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "elementwise-map-float32", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "complex128", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "complex64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "elementwise-map-complex128", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "elementwise-map-complex64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "contract-complex128", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "contract-complex64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "matrix-structural-complex128", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "matrix-structural-complex64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "matrix-numerical-complex128", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "matrix-numerical-complex64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "scientific-map-float64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "scientific-map-float32", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "stats-normal-float64", true);
    if (err != null) return err;
    err = tensor_backend_set_bool_field(interp, cpu_entry, "stats-normal-float32", true);
    if (err != null) return err;
    err = tensor_backend_set_ml_capability_fields(interp, cpu_entry, true, true, true, true, true, false, true, false, true, false, true, true, true, true, true, true, true);
    if (err != null) return err;
    result = make_cons_or_error(interp, cpu_entry, result, "tensor-backends: failed to allocate result list");
    return result;
}
