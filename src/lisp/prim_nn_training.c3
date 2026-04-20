module lisp;

struct NnTrainHead {
    Value* dense_spec;
    Value* dense_params;
    Value* activation;
    usz dense_index;
    usz layer_count;
    bool sequential;
}

fn Value* nn_train_invalid(Interp* interp, char[] path, char[] expected, Value* actual) @inline {
    return nn_invalid(interp, path, expected, actual);
}

fn Value* nn_train_require_model(Value* model, Interp* interp, bool require_train) {
    if (Value* err = nn_require_model(model, interp)) return err;
    if (!require_train) return null;
    Value* mode = nn_model_field(model, interp, "mode", "model.mode", "train mode");
    if (mode == null || mode.tag == ERROR) return mode;
    if (!nn_symbol_is(mode, interp, "train")) return nn_train_invalid(interp, "model.mode", "train mode", mode);
    return null;
}

fn bool nn_train_option_key_allowed(Value* key, Interp* interp, char[] allowed) {
    return key != null && key.tag == SYMBOL && nn_symbol_name_is(key.sym_val, interp, allowed);
}

fn Value* nn_train_validate_options(Value* options, Interp* interp, char[] prim_name, uint arg_index) {
    if (options == null || options.tag == NIL) return null;
    if (options.tag != HASHMAP || options.hashmap_val == null) return primitive_arg_type_raise(interp, prim_name, arg_index, "Dictionary options", options);
    for (uint i = 0; i < options.hashmap_val.capacity; i++) {
        HashEntry* entry = &options.hashmap_val.entries[i];
        if (entry.key == null) continue;
        if (!nn_train_option_key_allowed(entry.key, interp, "loss")) {
            return nn_train_invalid(interp, "options", "Dictionary with optional loss", entry.key);
        }
    }
    return null;
}

fn Value* nn_train_default_loss(Value* activation, Interp* interp) {
    SymbolId loss = INVALID_SYMBOL_ID;
    if (activation != null && nn_symbol_is(activation, interp, "softmax")) {
        loss = interp.symbols.intern("softmax-cross-entropy");
    } else {
        loss = interp.symbols.intern("mean-squared-error");
    }
    if (loss == INVALID_SYMBOL_ID) return nn_oom(interp);
    return make_symbol(interp, loss);
}

fn Value* nn_train_loss(Value* options, Value* activation, Interp* interp) {
    Value* loss = nn_dict_get(options, interp, "loss");
    if (loss == null) return nn_train_default_loss(activation, interp);
    if (loss.tag != SYMBOL ||
        (!nn_symbol_is(loss, interp, "mean-squared-error") &&
         !nn_symbol_is(loss, interp, "softmax-cross-entropy"))) {
        return nn_train_invalid(interp, "options.loss", "mean-squared-error or softmax-cross-entropy", loss);
    }
    return loss;
}

fn Value* nn_train_extract_dense(Value* spec, Value* params, Interp* interp, NnTrainHead* out) {
    out.dense_spec = spec;
    out.dense_params = params;
    out.activation = nn_dict_get(spec, interp, "activation");
    if (out.activation != null && is_nil(out.activation)) out.activation = null;
    out.dense_index = 0;
    out.layer_count = 1;
    out.sequential = false;
    return null;
}

fn Value* nn_train_extract_sequential(Value* spec, Value* params, Interp* interp, NnTrainHead* out) {
    Value* layers = nn_dict_get(spec, interp, "layers");
    if (layers == null || layers.tag != ARRAY || layers.array_val == null) {
        return nn_train_invalid(interp, "spec.layers", "Array layer specs", layers);
    }
    if (params == null || params.tag != ARRAY || params.array_val == null || params.array_val.length != layers.array_val.length) {
        return nn_train_invalid(interp, "model.params", "parameter array matching sequential layers", params);
    }
    if (layers.array_val.length != 1 && layers.array_val.length != 2) {
        return nn_train_invalid(interp, "spec.layers", "one dense layer or dense plus activation", layers);
    }
    Value* dense_spec = layers.array_val.items[0];
    if (!nn_kind_is(dense_spec, interp, "dense")) return nn_train_invalid(interp, "spec.layers.[0]", "dense layer", dense_spec);
    Value* activation = nn_dict_get(dense_spec, interp, "activation");
    if (activation != null && is_nil(activation)) activation = null;
    if (layers.array_val.length == 2) {
        Value* activation_spec = layers.array_val.items[1];
        if (!nn_kind_is(activation_spec, interp, "activation")) return nn_train_invalid(interp, "spec.layers.[1]", "activation layer", activation_spec);
        if (activation != null) return nn_train_invalid(interp, "spec.layers", "activation in dense or following activation layer, not both", activation);
        activation = nn_dict_get(activation_spec, interp, "activation");
    }
    out.dense_spec = dense_spec;
    out.dense_params = params.array_val.items[0];
    out.activation = activation;
    out.dense_index = 0;
    out.layer_count = layers.array_val.length;
    out.sequential = true;
    return null;
}

fn Value* nn_train_extract_head(Value* spec, Value* params, Interp* interp, NnTrainHead* out) {
    *out = {};
    if (nn_kind_is(spec, interp, "dense")) return nn_train_extract_dense(spec, params, interp, out);
    if (nn_kind_is(spec, interp, "sequential")) return nn_train_extract_sequential(spec, params, interp, out);
    return nn_train_invalid(interp, "model.spec", "dense or sequential dense training spec", spec);
}

fn Value* nn_train_build_ml_grad_spec(NnTrainHead* head, Value* input, Value* targets, Value* loss, Interp* interp) {
    Value* weights = nn_apply_required_tensor_param(head.dense_params, interp, "weights", "params.weights");
    if (weights == null || weights.tag == ERROR) return weights;
    bool use_bias = nn_spec_bool_field(head.dense_spec, interp, "use-bias", true);
    Value* bias = null;
    if (use_bias) {
        bias = nn_apply_required_tensor_param(head.dense_params, interp, "bias", "params.bias");
        if (bias == null || bias.tag == ERROR) return bias;
    }

    Value* spec = nn_make_dict(interp, 10);
    if (spec == null || spec.tag == ERROR) return spec;
    if (nn_symbol_is(loss, interp, "softmax-cross-entropy")) {
        if (head.activation != null && !nn_symbol_is(head.activation, interp, "softmax")) {
            return nn_train_invalid(interp, "options.loss", "softmax-cross-entropy with no activation or softmax activation", loss);
        }
        if (Value* err = nn_dict_set_symbol(spec, interp, "kind", "linear-softmax-cross-entropy")) return err;
    } else if (head.activation != null) {
        if (head.activation.tag != SYMBOL || !ml_grad_activation_supported(head.activation.sym_val, interp)) {
            return nn_train_invalid(interp, "model.spec.activation", "relu, sigmoid, tanh, or gelu for MSE training", head.activation);
        }
        if (Value* err = nn_dict_set_symbol(spec, interp, "kind", "linear-activation-mean-squared-error")) return err;
        if (Value* err = nn_dict_set(spec, interp, "activation", head.activation)) return err;
    } else {
        if (Value* err = nn_dict_set_symbol(spec, interp, "kind", "linear-mean-squared-error")) return err;
    }
    if (Value* err = nn_dict_set(spec, interp, "input", input)) return err;
    if (Value* err = nn_dict_set(spec, interp, "weights", weights)) return err;
    if (bias != null) {
        if (Value* err = nn_dict_set(spec, interp, "bias", bias)) return err;
    }
    if (Value* err = nn_dict_set(spec, interp, "targets", targets)) return err;
    return spec;
}

fn Value* nn_train_filter_dense_gradients(Value* params, Value* raw_gradients, Interp* interp) {
    Value* weights_grad = nn_dict_get(raw_gradients, interp, "weights");
    if (weights_grad == null || weights_grad.tag == ERROR) return weights_grad;
    Value* filtered = nn_make_dict(interp, 4);
    if (filtered == null || filtered.tag == ERROR) return filtered;
    if (Value* err = nn_dict_set(filtered, interp, "weights", weights_grad)) return err;
    Value* bias_param = nn_dict_get(params, interp, "bias");
    if (bias_param != null && !is_nil(bias_param)) {
        Value* bias_grad = nn_dict_get(raw_gradients, interp, "bias");
        if (bias_grad == null || bias_grad.tag == ERROR) return bias_grad;
        if (Value* err = nn_dict_set(filtered, interp, "bias", bias_grad)) return err;
    }
    return filtered;
}

fn Value* nn_train_lift_gradient_tree(NnTrainHead* head, Value* raw_gradients, Interp* interp) {
    Value* dense_gradients = nn_train_filter_dense_gradients(head.dense_params, raw_gradients, interp);
    if (dense_gradients == null || dense_gradients.tag == ERROR) return dense_gradients;
    if (!head.sequential) return dense_gradients;
    Value* tree = make_array_checked(interp, head.layer_count, "nn/grad: failed to allocate gradient tree");
    if (tree == null || tree.tag == ERROR) return tree;
    for (usz i = 0; i < head.layer_count; i++) {
        Value* child = i == head.dense_index ? dense_gradients : nn_make_dict(interp, 2);
        if (child == null || child.tag == ERROR) return child;
        tree.array_val.items[i] = child;
        tree.array_val.length++;
    }
    return tree;
}

fn Value* nn_train_grad_result(Value* model, Value* input, Value* targets, Value* options, Interp* interp, char[] prim_name, uint options_index) {
    if (Value* err = nn_train_require_model(model, interp, true)) return err;
    if (Value* err = nn_train_validate_options(options, interp, prim_name, options_index)) return err;
    Value* spec = nn_model_field(model, interp, "spec", "model.spec", "layer spec");
    Value* params = nn_model_field(model, interp, "params", "model.params", "parameter tree");
    if (spec == null || spec.tag == ERROR) return spec;
    if (params == null || params.tag == ERROR) return params;
    NnTrainHead head = {};
    if (Value* err = nn_train_extract_head(spec, params, interp, &head)) return err;
    Value* loss = nn_train_loss(options, head.activation, interp);
    if (loss == null || loss.tag == ERROR) return loss;
    Value* grad_spec = nn_train_build_ml_grad_spec(&head, input, targets, loss, interp);
    if (grad_spec == null || grad_spec.tag == ERROR) return grad_spec;
    Value*[1] grad_args = { grad_spec };
    Value* raw = prim_ml_grad(grad_args[0:1], null, interp);
    if (raw == null || raw.tag == ERROR) return raw;
    Value* lifted = nn_train_lift_gradient_tree(&head, nn_dict_get(raw, interp, "parameter-gradients"), interp);
    if (lifted == null || lifted.tag == ERROR) return lifted;
    if (Value* err = nn_dict_set(raw, interp, "parameter-gradients", lifted)) return err;
    if (Value* err = nn_dict_set_symbol(raw, interp, "kind", "nn-gradients")) return err;
    if (Value* err = nn_dict_set(raw, interp, "loss-kind", loss)) return err;
    return raw;
}

fn Value* nn_train_model_with_params(Value* model, Value* params, Interp* interp) {
    Value* next = nn_make_dict(interp, 12);
    if (next == null || next.tag == ERROR) return next;
    if (Value* err = nn_dict_set_symbol(next, interp, "kind", "model")) return err;
    if (Value* err = nn_dict_set(next, interp, "spec", nn_model_field(model, interp, "spec", "model.spec", "layer spec"))) return err;
    if (Value* err = nn_dict_set(next, interp, "params", params)) return err;
    if (Value* err = nn_dict_set(next, interp, "state", nn_model_field(model, interp, "state", "model.state", "state tree"))) return err;
    if (Value* err = nn_dict_set(next, interp, "mode", nn_model_field(model, interp, "mode", "model.mode", "train mode"))) return err;
    if (Value* err = nn_dict_set(next, interp, "dtype", nn_model_field(model, interp, "dtype", "model.dtype", "Float64 or Float32 symbol"))) return err;
    if (Value* err = nn_dict_set(next, interp, "device", nn_model_field(model, interp, "device", "model.device", "device symbol"))) return err;
    if (Value* err = nn_dict_set(next, interp, "metadata", nn_model_field(model, interp, "metadata", "model.metadata", "metadata dictionary"))) return err;
    if (Value* err = nn_validate_dataspec(next, interp)) return err;
    return next;
}

fn Value* prim_nn_forward(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2 && args.len != 4 && args.len != 5) {
        return primitive_arity_raise(interp, "nn/forward", "2 arguments: model input, or 4/5 arguments: spec params state input [options]", args.len);
    }
    return prim_nn_apply(args, env, interp);
}

fn Value* prim_nn_grad(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 3 && args.len != 4) return primitive_arity_raise(interp, "nn/grad", "3 or 4 arguments: model input targets [options]", args.len);
    Value* options = args.len == 4 ? args[3] : null;
    return nn_train_grad_result(args[0], args[1], args[2], options, interp, "nn/grad", 3);
}

fn Value* prim_nn_train_step(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 5 && args.len != 6) return primitive_arity_raise(interp, "nn/train-step", "5 or 6 arguments: model input targets optimizer-spec optimizer-state [options]", args.len);
    Value* options = args.len == 6 ? args[5] : null;
    Value* grad = nn_train_grad_result(args[0], args[1], args[2], options, interp, "nn/train-step", 5);
    if (grad == null || grad.tag == ERROR) return grad;
    Value* params = nn_model_field(args[0], interp, "params", "model.params", "parameter tree");
    if (params == null || params.tag == ERROR) return params;
    Value* param_gradients = nn_dict_get(grad, interp, "parameter-gradients");
    Value*[4] opt_args = { args[3], params, param_gradients, args[4] };
    Value* opt = prim_ml_optimizer_step(opt_args[0:4], null, interp);
    if (opt == null || opt.tag == ERROR) return opt;
    Value* updated_params = nn_dict_get(opt, interp, "parameters");
    Value* next_model = nn_train_model_with_params(args[0], updated_params, interp);
    if (next_model == null || next_model.tag == ERROR) return next_model;

    Value* result = nn_make_dict(interp, 14);
    if (result == null || result.tag == ERROR) return result;
    if (Value* err = nn_dict_set_symbol(result, interp, "kind", "train-step")) return err;
    if (Value* err = nn_dict_set(result, interp, "model", next_model)) return err;
    if (Value* err = nn_dict_set(result, interp, "parameters", updated_params)) return err;
    if (Value* err = nn_dict_set(result, interp, "optimizer-state", nn_dict_get(opt, interp, "state"))) return err;
    if (Value* err = nn_dict_set(result, interp, "gradients", param_gradients)) return err;
    if (Value* err = nn_dict_set(result, interp, "gradient-result", grad)) return err;
    if (Value* err = nn_dict_set(result, interp, "optimizer-step", opt)) return err;
    if (Value* err = nn_dict_set(result, interp, "loss", nn_dict_get(grad, interp, "loss"))) return err;
    if (Value* err = nn_dict_set(result, interp, "loss-kind", nn_dict_get(grad, interp, "loss-kind"))) return err;
    if (Value* err = nn_dict_set(result, interp, "output", nn_dict_get(grad, interp, "output"))) return err;
    if (Value* err = nn_dict_set(result, interp, "input-gradient", nn_dict_get(grad, interp, "input-gradient"))) return err;
    return result;
}
