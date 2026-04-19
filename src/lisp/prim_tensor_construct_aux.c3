module lisp;

import std::core::mem;
import main;

fn Value* tensor_construct_from_shape_and_data(Value* dtype_arg, Value* shape_arg, Value* data_arg, Interp* interp) {
    TensorDType dtype = TENSOR_DTYPE_DOUBLE;
    if (!tensor_dtype_arg_to_dtype(dtype_arg, interp, &dtype)) {
        return tensor_type_raise(interp, "Tensor: first argument must be Float64, Float32, Complex128, Complex64, BigInteger, BigFloat, or BigComplex");
    }

    usz* shape = null;
    usz rank = 0;
    Value* err = null;
    if (!tensor_parse_shape(shape_arg, interp, &shape, &rank, &err)) return err;
    defer if (shape != null) mem::free(shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = rank == 0 ? scalar_shape[0:0] : shape[0:rank];
    Value* tensor = make_tensor_uninitialized_checked(interp, dtype, shape_slice, "Tensor: out of memory");
    if (tensor == null || tensor.tag == ERROR) return tensor;
    bool keep_tensor = false;
    defer if (!keep_tensor) {
        boundary_cleanup_materialized_value(null, tensor, interp.current_scope);
    }
    if (tensor.tag != TENSOR || tensor.tensor_val == null) {
        return tensor_runtime_raise(interp, "Tensor: constructor produced invalid backing storage");
    }

    Value* materialized_data = null;
    Value* fill_data_arg = data_arg;
    if (data_arg != null && data_arg.tag == ITERATOR) {
        Value*[1] iterator_args = { data_arg };
        materialized_data = prim_to_array(iterator_args[0:1], null, interp);
        if (materialized_data == null) return tensor_runtime_raise(interp, "Tensor: failed to consume iterator data");
        if (materialized_data.tag == ERROR) return materialized_data;
        if (!is_array(materialized_data)) return tensor_runtime_raise(interp, "Tensor: iterator data did not produce an array");
        fill_data_arg = materialized_data;
    }
    defer if (materialized_data != null) {
        boundary_cleanup_materialized_value(null, materialized_data, interp.current_scope);
    }

    err = tensor_fill_from_data_arg(tensor.tensor_val, fill_data_arg, interp);
    if (err != null) return err;
    keep_tensor = true;
    return tensor;
}

fn Value* tensor_construct_inferred(Value* data_arg, TensorDType dtype, Interp* interp) {
    usz* shape = null;
    usz rank = 0;
    usz count = 0;
    Value* err = null;
    if (!tensor_infer_shape_from_value(data_arg, interp, &shape, &rank, &count, &err)) return err;
    defer if (shape != null) mem::free(shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = rank == 0 ? scalar_shape[0:0] : shape[0:rank];
    Value* tensor = make_tensor_uninitialized_checked(interp, dtype, shape_slice, "Tensor: out of memory");
    if (tensor == null || tensor.tag == ERROR) return tensor;
    bool keep_tensor = false;
    defer if (!keep_tensor) {
        boundary_cleanup_materialized_value(null, tensor, interp.current_scope);
    }
    if (tensor.tag != TENSOR || tensor.tensor_val == null) {
        return tensor_runtime_raise(interp, "Tensor: constructor produced invalid backing storage");
    }
    if (tensor.tensor_val.element_count != count) {
        return tensor_runtime_raise(interp, "Tensor: inferred shape count mismatch");
    }

    usz offset = 0;
    if (count > 0 || rank == 0) {
        err = tensor_flatten_inferred_data(tensor.tensor_val, data_arg, &offset, interp);
        if (err != null) return err;
    }
    if (offset != count) {
        return tensor_runtime_raise(interp, "Tensor: inferred data length mismatch");
    }

    keep_tensor = true;
    return tensor;
}

fn Value* tensor_construct_inferred_auto(Value* data_arg, Interp* interp) {
    return tensor_construct_inferred(data_arg, tensor_default_inferred_dtype(data_arg), interp);
}

fn Value* tensor_construct_from_iterator(Value* iterator_value, TensorDType dtype, Interp* interp) {
    Value*[1] iterator_args = { iterator_value };
    Value* materialized_data = prim_to_array(iterator_args[0:1], null, interp);
    if (materialized_data == null) return tensor_runtime_raise(interp, "Tensor: failed to consume iterator data");
    if (materialized_data.tag == ERROR) return materialized_data;
    if (!is_array(materialized_data)) return tensor_runtime_raise(interp, "Tensor: iterator data did not produce an array");
    defer boundary_cleanup_materialized_value(null, materialized_data, interp.current_scope);
    return tensor_construct_inferred(materialized_data, dtype, interp);
}

fn Value* tensor_construct_from_iterator_auto(Value* iterator_value, Interp* interp) {
    Value*[1] iterator_args = { iterator_value };
    Value* materialized_data = prim_to_array(iterator_args[0:1], null, interp);
    if (materialized_data == null) return tensor_runtime_raise(interp, "Tensor: failed to consume iterator data");
    if (materialized_data.tag == ERROR) return materialized_data;
    if (!is_array(materialized_data)) return tensor_runtime_raise(interp, "Tensor: iterator data did not produce an array");
    defer boundary_cleanup_materialized_value(null, materialized_data, interp.current_scope);
    return tensor_construct_inferred_auto(materialized_data, interp);
}

fn Value* prim_tensor(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 1) {
        if (args[0] != null && args[0].tag == ITERATOR) return tensor_construct_from_iterator_auto(args[0], interp);
        return tensor_construct_inferred_auto(args[0], interp);
    }
    if (args.len == 2) {
        TensorDType dtype = TENSOR_DTYPE_DOUBLE;
        if (tensor_dtype_arg_to_dtype(args[0], interp, &dtype)) {
            if (args[1] != null && args[1].tag == ITERATOR) return tensor_construct_from_iterator(args[1], dtype, interp);
            return tensor_construct_inferred(args[1], dtype, interp);
        }
        if (tensor_dtype_arg_to_dtype(args[1], interp, &dtype)) {
            if (args[0] != null && args[0].tag == ITERATOR) return tensor_construct_from_iterator(args[0], dtype, interp);
            return tensor_construct_inferred(args[0], dtype, interp);
        }
        return tensor_type_raise(interp, "Tensor: expected data with optional Float64, Float32, Complex128, Complex64, BigInteger, BigFloat, or BigComplex dtype");
    }
    if (args.len == 3) return tensor_construct_from_shape_and_data(args[0], args[1], args[2], interp);
    return primitive_arity_raise(interp, "Tensor", "1, 2, or 3 arguments: data [dtype] or dtype shape data-or-scalar", args.len);
}

fn Value* prim_realize(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) {
        return primitive_arity_raise(interp, "realize", "1 or 2 arguments: expr [out]", args.len);
    }
    if (args[0] != null && args[0].tag == ERROR) return args[0];
    if (args.len == 2 && args[1] != null && args[1].tag == ERROR) return args[1];

    if (args.len == 1) {
        if (is_tensor(args[0])) return tensor_realize_to_value(args[0], interp);
        return tensor_type_raise(interp, "realize: expected Tensor source");
    }

    if (!is_tensor(args[1])) return tensor_type_raise(interp, "realize: destination must be a Tensor");
    TensorVal* dest = args[1].tensor_val;
    if (dest != null && dest.device == TENSOR_DEVICE_CUDA) {
        if (is_numeric_value(args[0])) {
            Value* err = tensor_fill_cuda_destination(dest, args[0], interp);
            if (err != null) return err;
            return args[1];
        }
        return tensor_copy_into_destination(args[0], args[1], interp);
    }
    if (dest != null && dest.device == TENSOR_DEVICE_VULKAN) {
        if (is_numeric_value(args[0])) {
            Value* err = tensor_fill_vulkan_destination(dest, args[0], interp);
            if (err != null) return err;
            return args[1];
        }
        return tensor_copy_into_destination(args[0], args[1], interp);
    }
    if (dest == null) return tensor_runtime_raise(interp, "realize: destination tensor backing storage is missing");
    if (!dest.mutable) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/immutable-destination", "realize: destination Tensor is immutable", null);
    }
    Value* dest_check = tensor_require_cpu_destination(dest, interp);
    if (dest_check != null) return dest_check;

    if (is_numeric_value(args[0])) {
        Value* err = tensor_fill_from_data_arg(dest, args[0], interp);
        if (err != null) return err;
        return args[1];
    }
    return tensor_copy_into_destination(args[0], args[1], interp);
}

fn Value* prim_tensor_p(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "tensor?: expected 1 argument");
    return is_tensor(args[0]) ? make_symbol(interp, interp.sym_true) : make_nil(interp);
}

fn Value* prim_dtype(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || !is_tensor(args[0])) return raise_error(interp, "dtype: expected Tensor");
    TensorVal* tensor = args[0].tensor_val;
    if (tensor == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "dtype: tensor backing storage is missing");
    return make_symbol(interp, tensor_dtype_symbol(tensor.dtype, interp));
}

fn SymbolId tensor_device_symbol(TensorDeviceKind device, Interp* interp) {
    switch (device) {
        case TENSOR_DEVICE_CPU:
            return interp.symbols.intern("cpu");
        case TENSOR_DEVICE_CUDA:
            return interp.symbols.intern("cuda");
        case TENSOR_DEVICE_VULKAN:
            return interp.symbols.intern("vulkan");
    }
}

fn Value* prim_device(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "device", "1 argument: tensor", args.len);
    if (!is_tensor(args[0])) return raise_error(interp, "device: expected Tensor");
    TensorVal* tensor = args[0].tensor_val;
    if (tensor == null) return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "device: tensor backing storage is missing");
    return make_symbol(interp, tensor_device_symbol(tensor.device, interp));
}

fn Value* tensor_usz_metadata_value(Interp* interp, usz value, char[] label) {
    if (value > (usz)long.max) return raise_error(interp, label);
    return make_int(interp, (long)value);
}

fn Value* tensor_metadata_usz_array(Interp* interp, usz* values, usz count, char[] oom_message, char[] range_message) {
    Value* out = make_array_checked(interp, count, oom_message);
    if (out == null || out.tag == ERROR) return out;
    if (out.tag != ARRAY || out.array_val == null) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "tensor-layout: result array backing storage is missing", null);
    }
    for (usz i = 0; i < count; i++) {
        Value* item = tensor_usz_metadata_value(interp, values == null ? 0 : values[i], range_message);
        if (item == null || item.tag == ERROR) return item;
        out.array_val.items[i] = item;
    }
    out.array_val.length = count;
    return out;
}

fn Value* tensor_layout_set_field(Interp* interp, Value* out, char[] key_name, Value* value) {
    if (value == null || value.tag == ERROR) return value;
    if (!hashmap_set_checked(out.hashmap_val, make_symbol(interp, interp.symbols.intern(key_name)), value, interp)) {
        return raise_error(interp, "tensor-layout: out of memory while building result");
    }
    return null;
}

fn SymbolId tensor_payload_kind_symbol(TensorPayloadKind kind, Interp* interp) {
    switch (kind) {
        case TENSOR_PAYLOAD_CONCRETE:
            return interp.symbols.intern("concrete");
        case TENSOR_PAYLOAD_MAP:
            return interp.symbols.intern("map");
        case TENSOR_PAYLOAD_CONTRACT:
            return interp.symbols.intern("contract");
        case TENSOR_PAYLOAD_VIEW:
            return interp.symbols.intern("view");
    }
}

fn SymbolId tensor_write_policy_symbol(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return interp.symbols.intern("invalid");
    if (tensor.is_view) return tensor.mutable ? interp.symbols.intern("mutable-view") : interp.symbols.intern("read-only-view");
    return tensor.mutable ? interp.symbols.intern("mutable") : interp.symbols.intern("immutable");
}

fn SymbolId tensor_owner_symbol(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return interp.symbols.intern("invalid");
    if (tensor.is_view) return interp.symbols.intern("view-source");
    if (tensor.kind == TENSOR_PAYLOAD_CONCRETE) return interp.symbols.intern("self");
    return interp.symbols.intern("expression");
}
