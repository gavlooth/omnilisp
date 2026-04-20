module lisp;

fn Value* nn_oom(Interp* interp) @inline {
    return raise_error_with_payload_names(interp, "nn", "nn/out-of-memory", "nn: out of memory", null);
}

fn Value* nn_make_dict(Interp* interp, uint capacity) @inline {
    return make_hashmap_checked(interp, capacity, "nn: out of memory");
}

fn Value* nn_dict_set(Value* dict, Interp* interp, char[] key, Value* value) @inline {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return nn_oom(interp);
    if (value != null && value.tag == ERROR) return value;
    SymbolId key_sym = interp.symbols.intern(key);
    if (key_sym == INVALID_SYMBOL_ID) return nn_oom(interp);
    if (!hashmap_set_symbol_checked(dict.hashmap_val, key_sym, value, interp)) return nn_oom(interp);
    return null;
}

fn Value* nn_dict_set_symbol(Value* dict, Interp* interp, char[] key, char[] symbol_name) @inline {
    SymbolId value_sym = interp.symbols.intern(symbol_name);
    if (value_sym == INVALID_SYMBOL_ID) return nn_oom(interp);
    return nn_dict_set(dict, interp, key, make_symbol(interp, value_sym));
}

fn Value* nn_dict_get(Value* dict, Interp* interp, char[] key) @inline {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return null;
    SymbolId key_sym = interp.symbols.intern(key);
    if (key_sym == INVALID_SYMBOL_ID) return null;
    return hashmap_get(dict.hashmap_val, make_symbol(interp, key_sym));
}

fn bool nn_symbol_is(Value* value, Interp* interp, char[] name) @inline {
    if (value == null || value.tag != SYMBOL) return false;
    SymbolId sym = interp.symbols.intern(name);
    return sym != INVALID_SYMBOL_ID && value.sym_val == sym;
}

fn bool nn_symbol_name_is(SymbolId value, Interp* interp, char[] name) @inline {
    SymbolId sym = interp.symbols.intern(name);
    return sym != INVALID_SYMBOL_ID && value == sym;
}

fn bool nn_is_boolean(Value* value, Interp* interp) @inline {
    return value != null && value.tag == SYMBOL &&
        (value.sym_val == interp.sym_true || value.sym_val == interp.sym_false);
}

fn bool nn_is_nonnegative_int(Value* value) @inline {
    return value != null && value.tag == INT && value.int_val >= 0;
}

fn bool nn_is_positive_int(Value* value) @inline {
    return value != null && value.tag == INT && value.int_val > 0;
}

fn Value* nn_invalid(Interp* interp, char[] path, char[] expected, Value* actual) {
    Value* payload = nn_make_dict(interp, 8);
    if (payload == null || payload.tag == ERROR) return payload;
    if (Value* err = nn_dict_set(payload, interp, "path", make_string(interp, path))) return err;
    if (Value* err = nn_dict_set(payload, interp, "expected", make_string(interp, expected))) return err;
    if (actual != null) {
        if (Value* err = nn_dict_set(payload, interp, "actual-kind", make_symbol(interp, value_type_name(actual, interp)))) return err;
    } else {
        if (Value* err = nn_dict_set(payload, interp, "actual-kind", make_nil(interp))) return err;
    }
    return raise_error_with_payload_names(interp, "nn", "nn/invalid-spec", "nn/validate: invalid Omni Neural DataSpec", payload);
}

fn Value* nn_required_field(Value* spec, Interp* interp, char[] field, char[] path, char[] expected) {
    Value* field_value = nn_dict_get(spec, interp, field);
    if (field_value == null) return nn_invalid(interp, path, expected, null);
    return field_value;
}

fn bool nn_kind_is(Value* spec, Interp* interp, char[] kind_name) {
    Value* kind = nn_dict_get(spec, interp, "kind");
    return nn_symbol_is(kind, interp, kind_name);
}

fn Value* nn_validate_init_spec(Value* value, Interp* interp, char[] path) {
    if (value == null || value.tag == NIL) return null;
    if (value.tag != HASHMAP || value.hashmap_val == null) {
        return nn_invalid(interp, path, "nil or Dictionary with symbol kind", value);
    }
    Value* kind = nn_required_field(value, interp, "kind", path, "initializer kind symbol");
    if (kind != null && kind.tag == ERROR) return kind;
    if (kind == null || kind.tag != SYMBOL) return nn_invalid(interp, path, "initializer kind symbol", kind);
    if (nn_symbol_is(kind, interp, "zeros") ||
        nn_symbol_is(kind, interp, "ones") ||
        nn_symbol_is(kind, interp, "uniform") ||
        nn_symbol_is(kind, interp, "normal") ||
        nn_symbol_is(kind, interp, "xavier-uniform") ||
        nn_symbol_is(kind, interp, "glorot-uniform") ||
        nn_symbol_is(kind, interp, "kaiming-uniform") ||
        nn_symbol_is(kind, interp, "kaiming-normal")) {
        return null;
    }
    return nn_invalid(interp, path, "supported initializer kind", kind);
}

fn Value* nn_validate_activation_symbol(Value* value, Interp* interp, char[] path, bool allow_nil) {
    if (allow_nil && (value == null || value.tag == NIL)) return null;
    if (value == null || value.tag != SYMBOL) return nn_invalid(interp, path, "activation symbol", value);
    if (nn_symbol_is(value, interp, "relu") ||
        nn_symbol_is(value, interp, "sigmoid") ||
        nn_symbol_is(value, interp, "tanh") ||
        nn_symbol_is(value, interp, "gelu") ||
        nn_symbol_is(value, interp, "softmax")) {
        return null;
    }
    return nn_invalid(interp, path, "relu, sigmoid, tanh, gelu, or softmax", value);
}

fn Value* nn_validate_pair(Value* value, Interp* interp, char[] path, bool nonnegative) {
    if (value == null || value.tag != ARRAY || value.array_val == null || value.array_val.length != 2) {
        return nn_invalid(interp, path, "Array of two integers", value);
    }
    for (usz i = 0; i < 2; i++) {
        Value* item = value.array_val.items[i];
        bool ok = nonnegative ? nn_is_nonnegative_int(item) : nn_is_positive_int(item);
        if (!ok) return nn_invalid(interp, path, nonnegative ? "two non-negative integers" : "two positive integers", item);
    }
    return null;
}

fn Value* nn_validate_positive_field(Value* spec, Interp* interp, char[] field, char[] path) {
    Value* field_value = nn_required_field(spec, interp, field, path, "positive integer");
    if (field_value != null && field_value.tag == ERROR) return field_value;
    if (!nn_is_positive_int(field_value)) return nn_invalid(interp, path, "positive integer", field_value);
    return null;
}

fn Value* nn_validate_nonnegative_field(Value* spec, Interp* interp, char[] field, char[] path) {
    Value* field_value = nn_required_field(spec, interp, field, path, "non-negative integer");
    if (field_value != null && field_value.tag == ERROR) return field_value;
    if (!nn_is_nonnegative_int(field_value)) return nn_invalid(interp, path, "non-negative integer", field_value);
    return null;
}

fn Value* nn_validate_dense(Value* spec, Interp* interp) {
    if (Value* err = nn_validate_positive_field(spec, interp, "input-features", "spec.input-features")) return err;
    if (Value* err = nn_validate_positive_field(spec, interp, "output-features", "spec.output-features")) return err;
    Value* use_bias = nn_dict_get(spec, interp, "use-bias");
    if (use_bias != null && !nn_is_boolean(use_bias, interp)) return nn_invalid(interp, "spec.use-bias", "boolean", use_bias);
    if (Value* err = nn_validate_activation_symbol(nn_dict_get(spec, interp, "activation"), interp, "spec.activation", true)) return err;
    if (Value* err = nn_validate_init_spec(nn_dict_get(spec, interp, "kernel-init"), interp, "spec.kernel-init")) return err;
    if (Value* err = nn_validate_init_spec(nn_dict_get(spec, interp, "bias-init"), interp, "spec.bias-init")) return err;
    return null;
}

fn Value* nn_validate_activation(Value* spec, Interp* interp) {
    Value* activation = nn_required_field(spec, interp, "activation", "spec.activation", "activation symbol");
    if (activation != null && activation.tag == ERROR) return activation;
    return nn_validate_activation_symbol(activation, interp, "spec.activation", false);
}

fn Value* nn_validate_flatten(Value* spec, Interp* interp) {
    Value* start = nn_dict_get(spec, interp, "start-axis");
    Value* end = nn_dict_get(spec, interp, "end-axis");
    if (start != null && !nn_is_nonnegative_int(start)) return nn_invalid(interp, "spec.start-axis", "non-negative integer", start);
    if (end != null && !nn_is_nonnegative_int(end)) return nn_invalid(interp, "spec.end-axis", "non-negative integer", end);
    if (start != null && end != null && start.int_val > end.int_val) {
        return nn_invalid(interp, "spec.end-axis", "integer >= start-axis", end);
    }
    return null;
}

fn Value* nn_validate_conv1d(Value* spec, Interp* interp) {
    if (Value* err = nn_validate_positive_field(spec, interp, "in-channels", "spec.in-channels")) return err;
    if (Value* err = nn_validate_positive_field(spec, interp, "out-channels", "spec.out-channels")) return err;
    if (Value* err = nn_validate_positive_field(spec, interp, "kernel", "spec.kernel")) return err;
    if (Value* err = nn_validate_positive_field(spec, interp, "stride", "spec.stride")) return err;
    if (Value* err = nn_validate_nonnegative_field(spec, interp, "padding", "spec.padding")) return err;
    if (Value* err = nn_validate_positive_field(spec, interp, "dilation", "spec.dilation")) return err;
    if (Value* err = nn_validate_positive_field(spec, interp, "groups", "spec.groups")) return err;
    Value* in_channels = nn_dict_get(spec, interp, "in-channels");
    Value* out_channels = nn_dict_get(spec, interp, "out-channels");
    Value* groups = nn_dict_get(spec, interp, "groups");
    if (in_channels.int_val % groups.int_val != 0 || out_channels.int_val % groups.int_val != 0) {
        return nn_invalid(interp, "spec.groups", "groups dividing input and output channels", groups);
    }
    return nn_validate_activation_symbol(nn_dict_get(spec, interp, "activation"), interp, "spec.activation", true);
}

fn Value* nn_validate_conv2d(Value* spec, Interp* interp) {
    if (Value* err = nn_validate_positive_field(spec, interp, "in-channels", "spec.in-channels")) return err;
    if (Value* err = nn_validate_positive_field(spec, interp, "out-channels", "spec.out-channels")) return err;
    if (Value* err = nn_validate_pair(nn_required_field(spec, interp, "kernel", "spec.kernel", "Array of two positive integers"), interp, "spec.kernel", false)) return err;
    if (Value* err = nn_validate_pair(nn_required_field(spec, interp, "stride", "spec.stride", "Array of two positive integers"), interp, "spec.stride", false)) return err;
    if (Value* err = nn_validate_pair(nn_required_field(spec, interp, "padding", "spec.padding", "Array of two non-negative integers"), interp, "spec.padding", true)) return err;
    if (Value* err = nn_validate_pair(nn_required_field(spec, interp, "dilation", "spec.dilation", "Array of two positive integers"), interp, "spec.dilation", false)) return err;
    if (Value* err = nn_validate_positive_field(spec, interp, "groups", "spec.groups")) return err;
    Value* in_channels = nn_dict_get(spec, interp, "in-channels");
    Value* out_channels = nn_dict_get(spec, interp, "out-channels");
    Value* groups = nn_dict_get(spec, interp, "groups");
    if (in_channels.int_val % groups.int_val != 0 || out_channels.int_val % groups.int_val != 0) {
        return nn_invalid(interp, "spec.groups", "groups dividing input and output channels", groups);
    }
    return nn_validate_activation_symbol(nn_dict_get(spec, interp, "activation"), interp, "spec.activation", true);
}

fn Value* nn_validate_pool2d(Value* spec, Interp* interp) {
    if (Value* err = nn_validate_pair(nn_required_field(spec, interp, "window", "spec.window", "Array of two positive integers"), interp, "spec.window", false)) return err;
    if (Value* err = nn_validate_pair(nn_required_field(spec, interp, "stride", "spec.stride", "Array of two positive integers"), interp, "spec.stride", false)) return err;
    if (Value* err = nn_validate_pair(nn_required_field(spec, interp, "padding", "spec.padding", "Array of two non-negative integers"), interp, "spec.padding", true)) return err;
    Value* window = nn_dict_get(spec, interp, "window");
    Value* padding = nn_dict_get(spec, interp, "padding");
    if (padding.array_val.items[0].int_val >= window.array_val.items[0].int_val ||
        padding.array_val.items[1].int_val >= window.array_val.items[1].int_val) {
        return nn_invalid(interp, "spec.padding", "padding smaller than window on each axis", padding);
    }
    return null;
}

fn Value* nn_validate_layer_spec(Value* spec, Interp* interp) {
    if (spec == null || spec.tag != HASHMAP || spec.hashmap_val == null) {
        return nn_invalid(interp, "spec", "Dictionary layer spec", spec);
    }
    Value* kind = nn_required_field(spec, interp, "kind", "spec.kind", "layer kind symbol");
    if (kind != null && kind.tag == ERROR) return kind;
    if (kind == null || kind.tag != SYMBOL) return nn_invalid(interp, "spec.kind", "layer kind symbol", kind);
    if (nn_symbol_is(kind, interp, "sequential")) {
        Value* layers = nn_required_field(spec, interp, "layers", "spec.layers", "non-empty Array of layer specs");
        if (layers != null && layers.tag == ERROR) return layers;
        if (layers == null || layers.tag != ARRAY || layers.array_val == null || layers.array_val.length == 0) {
            return nn_invalid(interp, "spec.layers", "non-empty Array of layer specs", layers);
        }
        for (usz i = 0; i < layers.array_val.length; i++) {
            if (Value* err = nn_validate_layer_spec(layers.array_val.items[i], interp)) return err;
        }
        return null;
    }
    if (nn_symbol_is(kind, interp, "dense")) return nn_validate_dense(spec, interp);
    if (nn_symbol_is(kind, interp, "activation")) return nn_validate_activation(spec, interp);
    if (nn_symbol_is(kind, interp, "flatten")) return nn_validate_flatten(spec, interp);
    if (nn_symbol_is(kind, interp, "conv1d")) return nn_validate_conv1d(spec, interp);
    if (nn_symbol_is(kind, interp, "conv2d")) return nn_validate_conv2d(spec, interp);
    if (nn_symbol_is(kind, interp, "max-pool2d")) return nn_validate_pool2d(spec, interp);
    if (nn_symbol_is(kind, interp, "avg-pool2d")) return nn_validate_pool2d(spec, interp);
    return nn_invalid(interp, "spec.kind", "supported layer kind", kind);
}

fn Value* nn_validate_tree(Value* tree, Interp* interp, char[] path) {
    if (tree == null || tree.tag == NIL || tree.tag == TENSOR) return null;
    if (tree.tag == ARRAY) {
        if (tree.array_val == null) return nn_invalid(interp, path, "valid array tree", tree);
        for (usz i = 0; i < tree.array_val.length; i++) {
            if (Value* err = nn_validate_tree(tree.array_val.items[i], interp, path)) return err;
        }
        return null;
    }
    if (is_cons(tree)) {
        Value* cur = tree;
        while (is_cons(cur)) {
            if (Value* err = nn_validate_tree(cur.cons_val.car, interp, path)) return err;
            cur = cur.cons_val.cdr;
        }
        if (!is_nil(cur)) return nn_invalid(interp, path, "proper list tree", cur);
        return null;
    }
    if (tree.tag == HASHMAP) {
        if (tree.hashmap_val == null) return nn_invalid(interp, path, "valid dictionary tree", tree);
        for (uint i = 0; i < tree.hashmap_val.capacity; i++) {
            HashEntry* entry = &tree.hashmap_val.entries[i];
            if (entry.key == null) continue;
            if (Value* err = nn_validate_tree(entry.value, interp, path)) return err;
        }
        return null;
    }
    return nn_invalid(interp, path, "nil, Tensor, Array, List, or Dictionary tree", tree);
}

fn Value* nn_validate_model(Value* spec, Interp* interp) {
    Value* nested_spec = nn_required_field(spec, interp, "spec", "model.spec", "valid layer spec");
    if (nested_spec != null && nested_spec.tag == ERROR) return nested_spec;
    if (Value* err = nn_validate_layer_spec(nested_spec, interp)) return err;
    Value* params = nn_required_field(spec, interp, "params", "model.params", "parameter tree");
    if (params != null && params.tag == ERROR) return params;
    if (Value* err = nn_validate_tree(params, interp, "model.params")) return err;
    Value* state = nn_required_field(spec, interp, "state", "model.state", "state tree");
    if (state != null && state.tag == ERROR) return state;
    if (Value* err = nn_validate_tree(state, interp, "model.state")) return err;
    Value* mode = nn_required_field(spec, interp, "mode", "model.mode", "eval or train symbol");
    if (mode != null && mode.tag == ERROR) return mode;
    if (!nn_symbol_is(mode, interp, "eval") && !nn_symbol_is(mode, interp, "train")) return nn_invalid(interp, "model.mode", "eval or train symbol", mode);
    Value* dtype = nn_required_field(spec, interp, "dtype", "model.dtype", "Float64 or Float32 symbol");
    if (dtype != null && dtype.tag == ERROR) return dtype;
    if (!nn_symbol_is(dtype, interp, "Float64") && !nn_symbol_is(dtype, interp, "Float32")) return nn_invalid(interp, "model.dtype", "Float64 or Float32 symbol", dtype);
    Value* device = nn_required_field(spec, interp, "device", "model.device", "cpu, cuda, or vulkan symbol");
    if (device != null && device.tag == ERROR) return device;
    if (!nn_symbol_is(device, interp, "cpu") && !nn_symbol_is(device, interp, "cuda") && !nn_symbol_is(device, interp, "vulkan")) return nn_invalid(interp, "model.device", "cpu, cuda, or vulkan symbol", device);
    Value* metadata = nn_dict_get(spec, interp, "metadata");
    if (metadata != null && metadata.tag != HASHMAP && metadata.tag != NIL) return nn_invalid(interp, "model.metadata", "Dictionary or nil", metadata);
    return null;
}

fn Value* nn_validate_dataspec(Value* spec, Interp* interp) {
    if (spec == null || spec.tag != HASHMAP || spec.hashmap_val == null) {
        return nn_invalid(interp, "spec", "Dictionary Omni Neural DataSpec", spec);
    }
    Value* kind = nn_required_field(spec, interp, "kind", "spec.kind", "DataSpec kind symbol");
    if (kind != null && kind.tag == ERROR) return kind;
    if (kind == null || kind.tag != SYMBOL) return nn_invalid(interp, "spec.kind", "DataSpec kind symbol", kind);
    if (nn_symbol_is(kind, interp, "model")) return nn_validate_model(spec, interp);
    if (nn_symbol_is(kind, interp, "parameters")) {
        Value* tree = nn_required_field(spec, interp, "tree", "parameters.tree", "parameter tree");
        if (tree != null && tree.tag == ERROR) return tree;
        return nn_validate_tree(tree, interp, "parameters.tree");
    }
    if (nn_symbol_is(kind, interp, "state")) {
        Value* tree = nn_required_field(spec, interp, "tree", "state.tree", "state tree");
        if (tree != null && tree.tag == ERROR) return tree;
        return nn_validate_tree(tree, interp, "state.tree");
    }
    return nn_validate_layer_spec(spec, interp);
}

fn Value* prim_nn_validate(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "nn/validate", "1 argument: spec", args.len);
    if (Value* err = nn_validate_dataspec(args[0], interp)) return err;
    return args[0];
}

fn Value* nn_default_init(Interp* interp, char[] kind) {
    Value* init = nn_make_dict(interp, 2);
    if (init == null || init.tag == ERROR) return init;
    if (Value* err = nn_dict_set_symbol(init, interp, "kind", kind)) return err;
    return init;
}

fn Value* nn_default_options_get(Value* options, Interp* interp, char[] field, Value* fallback) {
    Value* value = nn_dict_get(options, interp, field);
    return value != null ? value : fallback;
}

fn Value* prim_nn_dense(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args.len > 3) return primitive_arity_raise(interp, "nn/dense", "2 or 3 arguments: input-features output-features [options]", args.len);
    Value* options = args.len == 3 ? args[2] : null;
    if (options != null && (options.tag != HASHMAP || options.hashmap_val == null)) return primitive_arg_type_raise(interp, "nn/dense", 2, "Dictionary options", options);
    Value* spec = nn_make_dict(interp, 12);
    if (spec == null || spec.tag == ERROR) return spec;
    if (Value* err = nn_dict_set_symbol(spec, interp, "kind", "dense")) return err;
    if (Value* err = nn_dict_set(spec, interp, "input-features", args[0])) return err;
    if (Value* err = nn_dict_set(spec, interp, "output-features", args[1])) return err;
    Value* use_bias = nn_default_options_get(options, interp, "use-bias", make_symbol(interp, interp.sym_true));
    if (Value* err = nn_dict_set(spec, interp, "use-bias", use_bias)) return err;
    if (Value* err = nn_dict_set(spec, interp, "activation", nn_default_options_get(options, interp, "activation", make_nil(interp)))) return err;
    Value* kernel_init = nn_dict_get(options, interp, "kernel-init");
    if (kernel_init == null) kernel_init = nn_default_init(interp, "kaiming-uniform");
    if (kernel_init != null && kernel_init.tag == ERROR) return kernel_init;
    if (Value* err = nn_dict_set(spec, interp, "kernel-init", kernel_init)) return err;
    Value* bias_init = nn_dict_get(options, interp, "bias-init");
    if (bias_init == null) bias_init = nn_default_init(interp, "zeros");
    if (bias_init != null && bias_init.tag == ERROR) return bias_init;
    if (Value* err = nn_dict_set(spec, interp, "bias-init", bias_init)) return err;
    if (Value* err = nn_validate_dataspec(spec, interp)) return err;
    return spec;
}

fn Value* prim_nn_activation(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "nn/activation", "1 argument: activation", args.len);
    Value* spec = nn_make_dict(interp, 4);
    if (spec == null || spec.tag == ERROR) return spec;
    if (Value* err = nn_dict_set_symbol(spec, interp, "kind", "activation")) return err;
    if (Value* err = nn_dict_set(spec, interp, "activation", args[0])) return err;
    if (Value* err = nn_validate_dataspec(spec, interp)) return err;
    return spec;
}

fn Value* nn_activation_ctor(Interp* interp, char[] activation) {
    Value* spec = nn_make_dict(interp, 4);
    if (spec == null || spec.tag == ERROR) return spec;
    if (Value* err = nn_dict_set_symbol(spec, interp, "kind", "activation")) return err;
    if (Value* err = nn_dict_set_symbol(spec, interp, "activation", activation)) return err;
    return spec;
}

fn Value* prim_nn_relu(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 0) return primitive_arity_raise(interp, "nn/relu", "0 arguments", args.len);
    return nn_activation_ctor(interp, "relu");
}

fn Value* prim_nn_sigmoid(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 0) return primitive_arity_raise(interp, "nn/sigmoid", "0 arguments", args.len);
    return nn_activation_ctor(interp, "sigmoid");
}

fn Value* prim_nn_tanh(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 0) return primitive_arity_raise(interp, "nn/tanh", "0 arguments", args.len);
    return nn_activation_ctor(interp, "tanh");
}

fn Value* prim_nn_gelu(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 0) return primitive_arity_raise(interp, "nn/gelu", "0 arguments", args.len);
    return nn_activation_ctor(interp, "gelu");
}

fn Value* prim_nn_softmax(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 0) return primitive_arity_raise(interp, "nn/softmax", "0 arguments", args.len);
    return nn_activation_ctor(interp, "softmax");
}

fn Value* prim_nn_flatten(Value*[] args, Env* env, Interp* interp) {
    if (args.len > 2) return primitive_arity_raise(interp, "nn/flatten", "0, 1, or 2 arguments: [start-axis [end-axis]]", args.len);
    Value* spec = nn_make_dict(interp, 6);
    if (spec == null || spec.tag == ERROR) return spec;
    if (Value* err = nn_dict_set_symbol(spec, interp, "kind", "flatten")) return err;
    if (Value* err = nn_dict_set(spec, interp, "start-axis", args.len >= 1 ? args[0] : make_int(interp, 1))) return err;
    if (Value* err = nn_dict_set(spec, interp, "end-axis", args.len >= 2 ? args[1] : make_int(interp, 1))) return err;
    if (Value* err = nn_validate_dataspec(spec, interp)) return err;
    return spec;
}

fn Value* prim_nn_sequential(Value*[] args, Env* env, Interp* interp) {
    if (args.len == 0) return primitive_arity_raise(interp, "nn/sequential", "at least 1 layer spec", args.len);
    Value* layers = make_array_checked(interp, args.len, "nn/sequential: out of memory");
    if (layers == null || layers.tag == ERROR) return layers;
    if (layers.array_val == null) return nn_oom(interp);
    for (usz i = 0; i < args.len; i++) {
        layers.array_val.items[i] = args[i];
        layers.array_val.length++;
    }
    Value* spec = nn_make_dict(interp, 4);
    if (spec == null || spec.tag == ERROR) return spec;
    if (Value* err = nn_dict_set_symbol(spec, interp, "kind", "sequential")) return err;
    if (Value* err = nn_dict_set(spec, interp, "layers", layers)) return err;
    if (Value* err = nn_validate_dataspec(spec, interp)) return err;
    return spec;
}

fn Value* prim_nn_conv1d(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 7) return primitive_arity_raise(interp, "nn/conv1d", "7 arguments: in-channels out-channels kernel stride padding dilation groups", args.len);
    Value* spec = nn_make_dict(interp, 12);
    if (spec == null || spec.tag == ERROR) return spec;
    char[][] fields = { "in-channels", "out-channels", "kernel", "stride", "padding", "dilation", "groups" };
    if (Value* err = nn_dict_set_symbol(spec, interp, "kind", "conv1d")) return err;
    for (usz i = 0; i < fields.len; i++) {
        if (Value* err = nn_dict_set(spec, interp, fields[i], args[i])) return err;
    }
    if (Value* err = nn_validate_dataspec(spec, interp)) return err;
    return spec;
}

fn Value* prim_nn_conv2d(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 7) return primitive_arity_raise(interp, "nn/conv2d", "7 arguments: in-channels out-channels kernel stride padding dilation groups", args.len);
    Value* spec = nn_make_dict(interp, 12);
    if (spec == null || spec.tag == ERROR) return spec;
    char[][] fields = { "in-channels", "out-channels", "kernel", "stride", "padding", "dilation", "groups" };
    if (Value* err = nn_dict_set_symbol(spec, interp, "kind", "conv2d")) return err;
    for (usz i = 0; i < fields.len; i++) {
        if (Value* err = nn_dict_set(spec, interp, fields[i], args[i])) return err;
    }
    if (Value* err = nn_validate_dataspec(spec, interp)) return err;
    return spec;
}

fn Value* nn_pool2d_ctor(Value*[] args, Interp* interp, char[] prim_name, char[] kind_name) {
    if (args.len != 3) return primitive_arity_raise(interp, prim_name, "3 arguments: window stride padding", args.len);
    Value* spec = nn_make_dict(interp, 6);
    if (spec == null || spec.tag == ERROR) return spec;
    if (Value* err = nn_dict_set_symbol(spec, interp, "kind", kind_name)) return err;
    if (Value* err = nn_dict_set(spec, interp, "window", args[0])) return err;
    if (Value* err = nn_dict_set(spec, interp, "stride", args[1])) return err;
    if (Value* err = nn_dict_set(spec, interp, "padding", args[2])) return err;
    if (Value* err = nn_validate_dataspec(spec, interp)) return err;
    return spec;
}

fn Value* prim_nn_max_pool2d(Value*[] args, Env* env, Interp* interp) {
    return nn_pool2d_ctor(args, interp, "nn/max-pool2d", "max-pool2d");
}

fn Value* prim_nn_avg_pool2d(Value*[] args, Env* env, Interp* interp) {
    return nn_pool2d_ctor(args, interp, "nn/avg-pool2d", "avg-pool2d");
}
