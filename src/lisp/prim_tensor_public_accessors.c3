module lisp;

fn Value* prim_to_device(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return primitive_arity_raise(interp, "to-device", "2 arguments: tensor device", args.len);
    if (!is_tensor(args[0])) return raise_error(interp, "to-device: expected Tensor");
    if (!is_symbol(args[1])) return raise_error(interp, "to-device: device target must be a symbol");

    SymbolId cpu_sym = interp.symbols.intern("cpu");
    SymbolId cuda_sym = interp.symbols.intern("cuda");
    SymbolId vulkan_sym = interp.symbols.intern("vulkan");
    if (args[1].sym_val == cpu_sym) {
        return tensor_copy_device_to_cpu_value(args[0], interp);
    }
    if (args[1].sym_val == cuda_sym) {
        return tensor_copy_cpu_to_cuda_value(args[0], interp);
    }
    if (args[1].sym_val == vulkan_sym) {
        return tensor_copy_cpu_to_vulkan_value(args[0], interp);
    }
    return raise_error_with_payload_names(
        interp,
        "tensor",
        "tensor/backend-unsupported",
        "to-device: unsupported device target",
        null
    );
}

fn Value* prim_rank(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_tensor(args[0])) return raise_error(interp, "rank: expected Tensor");
    TensorVal* tensor = args[0].tensor_val;
    if (tensor == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "rank: tensor backing storage is missing");
    if (tensor.rank > (usz)long.max) return raise_error(interp, "rank: tensor rank is too large");
    return make_int(interp, (long)tensor.rank);
}

fn Value* prim_shape(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_tensor(args[0])) return raise_error(interp, "shape: expected Tensor");
    TensorVal* tensor = args[0].tensor_val;
    if (tensor == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "shape: tensor backing storage is missing");

    Value* out = make_array_checked(interp, tensor.rank, "shape: failed to allocate result array");
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != ARRAY || out.array_val == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "shape: result array backing storage is missing");
    }

    for (usz i = 0; i < tensor.rank; i++) {
        if (tensor.shape == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "shape: tensor shape storage is missing");
        if (tensor.shape[i] > (usz)long.max) return raise_error(interp, "shape: tensor dimension is too large");
        out.array_val.items[i] = make_int(interp, (long)tensor.shape[i]);
    }
    out.array_val.length = tensor.rank;
    return out;
}
