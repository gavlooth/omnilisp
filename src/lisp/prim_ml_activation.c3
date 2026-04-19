module lisp;

import main;

fn Value* ml_activation_zero_for_dtype(TensorDType dtype, Interp* interp, char[] primitive_name) {
    switch (dtype) {
        case TENSOR_DTYPE_DOUBLE:
            return make_double(interp, 0.0);
        case TENSOR_DTYPE_FLOAT32:
            return make_float32(interp, 0.0f);
        default:
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", primitive_name, null);
    }
}

fn Value* ml_activation_lookup_primitive(Interp* interp, char[] symbol_name, char[] primitive_name) {
    SymbolId sym = interp.symbols.intern(symbol_name);
    Value* func = interp.global_env.lookup(sym);
    if (func == null) return tensor_runtime_raise(interp, primitive_name);
    return func;
}

fn Value* prim_ml_relu(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "ml/relu", "1 argument: input", args.len);
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "ml/relu: expected Tensor input");

    TensorVal* source = null;
    Value* err = tensor_map_source_storage(args[0], &source, interp);
    if (err != null) return err;
    if (source == null) return tensor_runtime_raise(interp, "ml/relu: tensor metadata is missing");
    if (source.dtype != TENSOR_DTYPE_DOUBLE && source.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/relu: currently supports Float64 and Float32 tensors", null);
    }

    Value* max_func = ml_activation_lookup_primitive(interp, "max", "ml/relu: max primitive is unavailable");
    if (max_func == null || max_func.tag == ERROR) return max_func;
    Value* zero = ml_activation_zero_for_dtype(source.dtype, interp, "ml/relu: currently supports Float64 and Float32 tensors");
    if (zero == null || zero.tag == ERROR) return zero;

    Value*[3] map_args = { max_func, args[0], zero };
    return prim_tensor_map(map_args[..], env, interp);
}
