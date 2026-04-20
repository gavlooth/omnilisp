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
    return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "kernel/run: custom kernel execution is not implemented for any backend yet", null);
}
