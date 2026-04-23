module lisp;

const double NN_INIT_PI = 3.14159265358979323846;
const uint NN_INIT_LCG_MULTIPLIER = 1664525;
const uint NN_INIT_LCG_INCREMENT = 1013904223;
const double NN_INIT_UINT32_SCALE = 4294967296.0;

struct NnInitContext {
    TensorDType dtype;
    SymbolId dtype_symbol;
    SymbolId device_symbol;
    uint rng_state;
    uint seed_initial;
}

struct NnLayerInit {
    Value* params;
    Value* state;
}

fn Value* nn_init_invalid(Interp* interp, char[] path, char[] expected, Value* actual) @inline {
    return nn_invalid(interp, path, expected, actual);
}

fn uint nn_rng_next(NnInitContext* ctx) @inline {
    ctx.rng_state = ctx.rng_state * NN_INIT_LCG_MULTIPLIER + NN_INIT_LCG_INCREMENT;
    return ctx.rng_state;
}

fn double nn_rng_uniform01(NnInitContext* ctx) @inline {
    return ((double)nn_rng_next(ctx) + 0.5) / NN_INIT_UINT32_SCALE;
}

fn double nn_rng_uniform(NnInitContext* ctx, double low, double high) @inline {
    return low + (high - low) * nn_rng_uniform01(ctx);
}

fn double nn_rng_normal(NnInitContext* ctx, double mean, double stddev) {
    double u1 = nn_rng_uniform01(ctx);
    double u2 = nn_rng_uniform01(ctx);
    double radius = c_sqrt(-2.0 * c_log(u1));
    double theta = 2.0 * NN_INIT_PI * u2;
    return mean + stddev * radius * c_cos(theta);
}

fn Value* nn_init_empty_dict(Interp* interp) @inline {
    return nn_make_dict(interp, 2);
}

fn Value* nn_init_options(Value* options, Interp* interp, NnInitContext* ctx, Value** mode_out) {
    if (options != null && (options.tag != HASHMAP || options.hashmap_val == null)) {
        return primitive_arg_type_raise(interp, "nn/init", 1, "Dictionary options", options);
    }

    Value* dtype_value = nn_dict_get(options, interp, "dtype");
    if (dtype_value == null) dtype_value = make_symbol(interp, interp.sym_Float32);
    if (!tensor_dtype_arg_to_dtype(dtype_value, interp, &ctx.dtype) ||
        (ctx.dtype != TENSOR_DTYPE_DOUBLE && ctx.dtype != TENSOR_DTYPE_FLOAT32)) {
        return nn_init_invalid(interp, "options.dtype", "Float64 or Float32", dtype_value);
    }
    ctx.dtype_symbol = ctx.dtype == TENSOR_DTYPE_DOUBLE
        ? interp.symbols.intern("Float64")
        : interp.symbols.intern("Float32");
    if (ctx.dtype_symbol == INVALID_SYMBOL_ID) return nn_oom(interp);

    Value* device_value = nn_dict_get(options, interp, "device");
    if (device_value == null) device_value = make_symbol(interp, interp.symbols.intern("cpu"));
    if (device_value == null || device_value.tag != SYMBOL ||
        (!nn_symbol_is(device_value, interp, "cpu") &&
         !nn_symbol_is(device_value, interp, "cuda") &&
         !nn_symbol_is(device_value, interp, "vulkan"))) {
        return nn_init_invalid(interp, "options.device", "cpu, cuda, or vulkan symbol", device_value);
    }
    ctx.device_symbol = device_value.sym_val;

    Value* seed_value = nn_dict_get(options, interp, "seed");
    if (seed_value == null) {
        ctx.rng_state = (uint)1;
    } else if (seed_value.tag == INT && seed_value.int_val >= 0 && seed_value.int_val <= (long)uint.max) {
        ctx.rng_state = (uint)seed_value.int_val;
        if (ctx.rng_state == 0) ctx.rng_state = (uint)1;
    } else {
        return nn_init_invalid(interp, "options.seed", "non-negative 32-bit integer", seed_value);
    }
    ctx.seed_initial = ctx.rng_state;

    Value* mode_value = nn_dict_get(options, interp, "mode");
    if (mode_value == null) mode_value = make_symbol(interp, interp.symbols.intern("eval"));
    if (!nn_symbol_is(mode_value, interp, "eval") && !nn_symbol_is(mode_value, interp, "train")) {
        return nn_init_invalid(interp, "options.mode", "eval or train symbol", mode_value);
    }
    *mode_out = mode_value;
    return null;
}

fn double nn_init_scale(Value* init_spec, Interp* interp, double fan_in, double fan_out, double fallback) {
    Value* kind = nn_dict_get(init_spec, interp, "kind");
    if (nn_symbol_is(kind, interp, "xavier-uniform") ||
        nn_symbol_is(kind, interp, "glorot-uniform")) {
        return c_sqrt(6.0 / (fan_in + fan_out));
    }
    if (nn_symbol_is(kind, interp, "kaiming-uniform")) {
        return c_sqrt(6.0 / fan_in);
    }
    if (nn_symbol_is(kind, interp, "kaiming-normal")) {
        return c_sqrt(2.0 / fan_in);
    }
    return fallback;
}

fn double nn_init_next_value(Value* init_spec, Interp* interp, NnInitContext* ctx, double fan_in, double fan_out) {
    Value* kind = nn_dict_get(init_spec, interp, "kind");
    if (nn_symbol_is(kind, interp, "zeros")) return 0.0;
    if (nn_symbol_is(kind, interp, "ones")) return 1.0;
    if (nn_symbol_is(kind, interp, "uniform")) return nn_rng_uniform(ctx, -0.05, 0.05);
    if (nn_symbol_is(kind, interp, "normal")) return nn_rng_normal(ctx, 0.0, 0.05);
    if (nn_symbol_is(kind, interp, "xavier-uniform") || nn_symbol_is(kind, interp, "glorot-uniform")) {
        double limit = nn_init_scale(init_spec, interp, fan_in, fan_out, 0.05);
        return nn_rng_uniform(ctx, -limit, limit);
    }
    if (nn_symbol_is(kind, interp, "kaiming-uniform")) {
        double limit = nn_init_scale(init_spec, interp, fan_in, fan_out, 0.05);
        return nn_rng_uniform(ctx, -limit, limit);
    }
    if (nn_symbol_is(kind, interp, "kaiming-normal")) {
        double stddev = nn_init_scale(init_spec, interp, fan_in, fan_out, 0.05);
        return nn_rng_normal(ctx, 0.0, stddev);
    }
    return 0.0;
}

fn Value* nn_init_tensor(Interp* interp, NnInitContext* ctx, usz[] shape, Value* init_spec, double fan_in, double fan_out) {
    Value* tensor = make_tensor_uninitialized_checked(interp, ctx.dtype, shape, "nn/init: failed to allocate parameter tensor");
    if (tensor == null || tensor.tag == ERROR) return tensor;
    if (tensor.tag != TENSOR || tensor.tensor_val == null || !tensor_has_valid_storage(tensor.tensor_val)) {
        return tensor_runtime_raise(interp, "nn/init: parameter tensor backing storage is missing");
    }
    TensorVal* tv = tensor.tensor_val;
    if (ctx.dtype == TENSOR_DTYPE_DOUBLE) {
        double* data = (double*)tv.data;
        for (usz i = 0; i < tv.element_count; i++) data[i] = nn_init_next_value(init_spec, interp, ctx, fan_in, fan_out);
    } else {
        float* data = (float*)tv.data;
        for (usz i = 0; i < tv.element_count; i++) data[i] = (float)nn_init_next_value(init_spec, interp, ctx, fan_in, fan_out);
    }

    if (nn_symbol_name_is(ctx.device_symbol, interp, "cpu")) return tensor;
    Value* device = make_symbol(interp, ctx.device_symbol);
    Value*[2] to_device_args = { tensor, device };
    return prim_to_device(to_device_args[0:2], null, interp);
}

fn bool nn_spec_bool_field(Value* spec, Interp* interp, char[] field, bool fallback) {
    Value* value = nn_dict_get(spec, interp, field);
    if (value == null) return fallback;
    return value.tag == SYMBOL && value.sym_val == interp.sym_true;
}

fn Value* nn_layer_param_dict(Interp* interp, Value* weights, Value* bias) {
    Value* params = nn_make_dict(interp, bias == null ? 2 : 4);
    if (params == null || params.tag == ERROR) return params;
    if (weights != null) {
        if (Value* err = nn_dict_set(params, interp, "weights", weights)) return err;
    }
    if (bias != null) {
        if (Value* err = nn_dict_set(params, interp, "bias", bias)) return err;
    }
    return params;
}

fn Value* nn_init_dense(Value* spec, Interp* interp, NnInitContext* ctx, NnLayerInit* out) {
    long input_features_raw = nn_dict_get(spec, interp, "input-features").int_val;
    long output_features_raw = nn_dict_get(spec, interp, "output-features").int_val;
    if (input_features_raw < 0 || output_features_raw < 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "nn/init: dense dimensions must be non-negative", null);
    }
    usz input_features = (usz)input_features_raw;
    usz output_features = (usz)output_features_raw;
    usz[2] weight_shape = { output_features, input_features };
    Value* weights = nn_init_tensor(
        interp,
        ctx,
        weight_shape[0:2],
        nn_dict_get(spec, interp, "kernel-init"),
        (double)input_features,
        (double)output_features);
    if (weights == null || weights.tag == ERROR) return weights;

    Value* bias = null;
    if (nn_spec_bool_field(spec, interp, "use-bias", true)) {
        usz[1] bias_shape = { output_features };
        bias = nn_init_tensor(
            interp,
            ctx,
            bias_shape[0:1],
            nn_dict_get(spec, interp, "bias-init"),
            (double)input_features,
            (double)output_features);
        if (bias == null || bias.tag == ERROR) return bias;
    }

    out.params = nn_layer_param_dict(interp, weights, bias);
    if (out.params == null || out.params.tag == ERROR) return out.params;
    out.state = nn_init_empty_dict(interp);
    if (out.state == null || out.state.tag == ERROR) return out.state;
    return null;
}

fn Value* nn_init_batch_normalization(Value* spec, Interp* interp, NnInitContext* ctx, NnLayerInit* out) {
    usz channels = (usz)nn_dict_get(spec, interp, "channels").int_val;
    usz[1] shape = { channels };
    Value* scale = nn_init_tensor(interp, ctx, shape[0:1], nn_dict_get(spec, interp, "scale-init"), (double)channels, (double)channels);
    if (scale == null || scale.tag == ERROR) return scale;
    Value* bias = nn_init_tensor(interp, ctx, shape[0:1], nn_dict_get(spec, interp, "bias-init"), (double)channels, (double)channels);
    if (bias == null || bias.tag == ERROR) return bias;
    Value* zeros = nn_default_init(interp, "zeros");
    if (zeros == null || zeros.tag == ERROR) return zeros;
    Value* ones = nn_default_init(interp, "ones");
    if (ones == null || ones.tag == ERROR) return ones;
    Value* running_mean = nn_init_tensor(interp, ctx, shape[0:1], zeros, (double)channels, (double)channels);
    if (running_mean == null || running_mean.tag == ERROR) return running_mean;
    Value* running_variance = nn_init_tensor(interp, ctx, shape[0:1], ones, (double)channels, (double)channels);
    if (running_variance == null || running_variance.tag == ERROR) return running_variance;

    out.params = nn_make_dict(interp, 4);
    if (out.params == null || out.params.tag == ERROR) return out.params;
    if (Value* err = nn_dict_set(out.params, interp, "scale", scale)) return err;
    if (Value* err = nn_dict_set(out.params, interp, "bias", bias)) return err;

    out.state = nn_make_dict(interp, 4);
    if (out.state == null || out.state.tag == ERROR) return out.state;
    if (Value* err = nn_dict_set(out.state, interp, "running-mean", running_mean)) return err;
    if (Value* err = nn_dict_set(out.state, interp, "running-variance", running_variance)) return err;
    return null;
}

fn Value* nn_init_conv1d(Value* spec, Interp* interp, NnInitContext* ctx, NnLayerInit* out) {
    long in_channels_raw = nn_dict_get(spec, interp, "in-channels").int_val;
    long out_channels_raw = nn_dict_get(spec, interp, "out-channels").int_val;
    long kernel_raw = nn_dict_get(spec, interp, "kernel").int_val;
    long groups_raw = nn_dict_get(spec, interp, "groups").int_val;
    if (in_channels_raw < 0 || out_channels_raw < 0 || kernel_raw < 0 || groups_raw < 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "nn/init: conv1d dimensions must be non-negative", null);
    }
    usz in_channels = (usz)in_channels_raw;
    usz out_channels = (usz)out_channels_raw;
    usz kernel = (usz)kernel_raw;
    usz groups = (usz)groups_raw;
    usz in_per_group = in_channels / groups;
    usz[3] weight_shape = { out_channels, in_per_group, kernel };
    double fan_in = (double)(in_per_group * kernel);
    double fan_out = (double)((out_channels / groups) * kernel);
    Value* weights = nn_init_tensor(interp, ctx, weight_shape[0:3], nn_dict_get(spec, interp, "kernel-init"), fan_in, fan_out);
    if (weights == null || weights.tag == ERROR) return weights;

    Value* bias = null;
    if (nn_spec_bool_field(spec, interp, "use-bias", true)) {
        usz[1] bias_shape = { out_channels };
        bias = nn_init_tensor(interp, ctx, bias_shape[0:1], nn_dict_get(spec, interp, "bias-init"), fan_in, fan_out);
        if (bias == null || bias.tag == ERROR) return bias;
    }

    out.params = nn_layer_param_dict(interp, weights, bias);
    if (out.params == null || out.params.tag == ERROR) return out.params;
    out.state = nn_init_empty_dict(interp);
    if (out.state == null || out.state.tag == ERROR) return out.state;
    return null;
}

fn Value* nn_init_conv2d(Value* spec, Interp* interp, NnInitContext* ctx, NnLayerInit* out) {
    long in_channels_raw = nn_dict_get(spec, interp, "in-channels").int_val;
    long out_channels_raw = nn_dict_get(spec, interp, "out-channels").int_val;
    if (in_channels_raw < 0 || out_channels_raw < 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "nn/init: conv2d dimensions must be non-negative", null);
    }
    Value* kernel = nn_dict_get(spec, interp, "kernel");
    long kernel_h_raw = kernel.array_val.items[0].int_val;
    long kernel_w_raw = kernel.array_val.items[1].int_val;
    long groups_raw = nn_dict_get(spec, interp, "groups").int_val;
    if (kernel_h_raw < 0 || kernel_w_raw < 0 || groups_raw < 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", "nn/init: conv2d dimensions must be non-negative", null);
    }
    usz in_channels = (usz)in_channels_raw;
    usz out_channels = (usz)out_channels_raw;
    usz kernel_h = (usz)kernel_h_raw;
    usz kernel_w = (usz)kernel_w_raw;
    usz groups = (usz)groups_raw;
    usz in_per_group = in_channels / groups;
    usz[4] weight_shape = { out_channels, in_per_group, kernel_h, kernel_w };
    double fan_in = (double)(in_per_group * kernel_h * kernel_w);
    double fan_out = (double)((out_channels / groups) * kernel_h * kernel_w);
    Value* weights = nn_init_tensor(interp, ctx, weight_shape[0:4], nn_dict_get(spec, interp, "kernel-init"), fan_in, fan_out);
    if (weights == null || weights.tag == ERROR) return weights;

    Value* bias = null;
    if (nn_spec_bool_field(spec, interp, "use-bias", true)) {
        usz[1] bias_shape = { out_channels };
        bias = nn_init_tensor(interp, ctx, bias_shape[0:1], nn_dict_get(spec, interp, "bias-init"), fan_in, fan_out);
        if (bias == null || bias.tag == ERROR) return bias;
    }

    out.params = nn_layer_param_dict(interp, weights, bias);
    if (out.params == null || out.params.tag == ERROR) return out.params;
    out.state = nn_init_empty_dict(interp);
    if (out.state == null || out.state.tag == ERROR) return out.state;
    return null;
}

fn Value* nn_init_layer(Value* spec, Interp* interp, NnInitContext* ctx, NnLayerInit* out) {
    *out = {};
    if (nn_kind_is(spec, interp, "dense")) return nn_init_dense(spec, interp, ctx, out);
    if (nn_kind_is(spec, interp, "batch-normalization")) return nn_init_batch_normalization(spec, interp, ctx, out);
    if (nn_kind_is(spec, interp, "conv1d")) return nn_init_conv1d(spec, interp, ctx, out);
    if (nn_kind_is(spec, interp, "conv2d")) return nn_init_conv2d(spec, interp, ctx, out);
    if (nn_kind_is(spec, interp, "sequential")) {
        Value* layers = nn_dict_get(spec, interp, "layers");
        Value* params_array = make_array_checked(interp, layers.array_val.length, "nn/init: out of memory");
        if (params_array == null || params_array.tag == ERROR) return params_array;
        Value* state_array = make_array_checked(interp, layers.array_val.length, "nn/init: out of memory");
        if (state_array == null || state_array.tag == ERROR) return state_array;
        if (params_array.array_val == null || state_array.array_val == null) return nn_oom(interp);
        for (usz i = 0; i < layers.array_val.length; i++) {
            NnLayerInit child = {};
            if (Value* err = nn_init_layer(layers.array_val.items[i], interp, ctx, &child)) return err;
            params_array.array_val.items[i] = child.params;
            params_array.array_val.length++;
            state_array.array_val.items[i] = child.state;
            state_array.array_val.length++;
        }
        out.params = params_array;
        out.state = state_array;
        return null;
    }
    if (nn_kind_is(spec, interp, "activation") ||
        nn_kind_is(spec, interp, "flatten") ||
        nn_kind_is(spec, interp, "max-pool2d") ||
        nn_kind_is(spec, interp, "avg-pool2d")) {
        out.params = nn_init_empty_dict(interp);
        if (out.params == null || out.params.tag == ERROR) return out.params;
        out.state = nn_init_empty_dict(interp);
        if (out.state == null || out.state.tag == ERROR) return out.state;
        return null;
    }
    return nn_invalid(interp, "spec.kind", "supported layer kind", nn_dict_get(spec, interp, "kind"));
}

fn Value* nn_init_metadata(Interp* interp, NnInitContext* ctx) {
    Value* metadata = nn_make_dict(interp, 4);
    if (metadata == null || metadata.tag == ERROR) return metadata;
    if (Value* err = nn_dict_set(metadata, interp, "seed", make_int(interp, (long)ctx.seed_initial))) return err;
    return metadata;
}

fn Value* prim_nn_init(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) return primitive_arity_raise(interp, "nn/init", "1 or 2 arguments: spec [options]", args.len);
    if (Value* err = nn_validate_dataspec(args[0], interp)) return err;

    NnInitContext ctx = {};
    Value* mode = null;
    Value* options = args.len == 2 ? args[1] : null;
    if (Value* err = nn_init_options(options, interp, &ctx, &mode)) return err;

    NnLayerInit initialized = {};
    if (Value* err = nn_init_layer(args[0], interp, &ctx, &initialized)) return err;

    Value* model = nn_make_dict(interp, 12);
    if (model == null || model.tag == ERROR) return model;
    if (Value* err = nn_dict_set_symbol(model, interp, "kind", "model")) return err;
    if (Value* err = nn_dict_set(model, interp, "spec", args[0])) return err;
    if (Value* err = nn_dict_set(model, interp, "params", initialized.params)) return err;
    if (Value* err = nn_dict_set(model, interp, "state", initialized.state)) return err;
    if (Value* err = nn_dict_set(model, interp, "mode", mode)) return err;
    if (Value* err = nn_dict_set(model, interp, "dtype", make_symbol(interp, ctx.dtype_symbol))) return err;
    if (Value* err = nn_dict_set(model, interp, "device", make_symbol(interp, ctx.device_symbol))) return err;
    if (Value* err = nn_dict_set(model, interp, "metadata", nn_init_metadata(interp, &ctx))) return err;
    if (Value* err = nn_validate_dataspec(model, interp)) return err;
    return model;
}
