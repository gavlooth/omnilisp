module lisp;

fn Value* ml_optimizer_checkpoint_invalid(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "tensor", "tensor/invalid-argument", message, null);
}

fn Value* ml_optimizer_checkpoint_parse_number(Value* spec, Interp* interp, char[] key, bool required, double default_value, double* out_value) {
    Value* value = nn_dict_get(spec, interp, key);
    if (value == null) {
        if (required) return ml_optimizer_checkpoint_invalid(interp, "ml/save-optimizer: optimizer spec is missing a required numeric field");
        *out_value = default_value;
        return null;
    }
    if (!try_numeric_to_double(value, out_value) || !conversion_double_is_finite(*out_value)) return ml_optimizer_checkpoint_invalid(interp, "ml/save-optimizer: optimizer numeric fields must be finite numbers");
    return null;
}

fn Value* ml_optimizer_checkpoint_validate_spec(Value* spec, Interp* interp) {
    if (spec == null || spec.tag != HASHMAP || spec.hashmap_val == null) return ml_optimizer_checkpoint_invalid(interp, "ml/save-optimizer: optimizer spec must be a Dictionary");
    double learning_rate = 0.0;
    double weight_decay = 0.0;
    double clip_norm = 0.0;
    Value* err = ml_optimizer_checkpoint_parse_number(spec, interp, "learning-rate", true, 0.0, &learning_rate);
    if (err != null) return err;
    err = ml_optimizer_checkpoint_parse_number(spec, interp, "weight-decay", false, 0.0, &weight_decay);
    if (err != null) return err;
    err = ml_optimizer_checkpoint_parse_number(spec, interp, "clip-norm", false, 0.0, &clip_norm);
    if (err != null) return err;
    if (learning_rate < 0.0 || weight_decay < 0.0 || clip_norm < 0.0) return ml_optimizer_checkpoint_invalid(interp, "ml/save-optimizer: learning-rate, weight-decay, and clip-norm must be non-negative");

    Value* kind = nn_dict_get(spec, interp, "kind");
    if (nn_symbol_is(kind, interp, "sgd")) {
        double momentum = 0.0;
        err = ml_optimizer_checkpoint_parse_number(spec, interp, "momentum", false, 0.0, &momentum);
        if (err != null) return err;
        if (momentum < 0.0 || momentum >= 1.0) return ml_optimizer_checkpoint_invalid(interp, "ml/save-optimizer: SGD momentum must be in [0, 1)");
        return null;
    }
    if (nn_symbol_is(kind, interp, "adam") || nn_symbol_is(kind, interp, "adamw")) {
        double beta1 = ML_OPTIMIZER_DEFAULT_ADAM_BETA1;
        double beta2 = ML_OPTIMIZER_DEFAULT_ADAM_BETA2;
        double epsilon = ML_OPTIMIZER_DEFAULT_EPSILON;
        err = ml_optimizer_checkpoint_parse_number(spec, interp, "beta1", false, ML_OPTIMIZER_DEFAULT_ADAM_BETA1, &beta1);
        if (err != null) return err;
        err = ml_optimizer_checkpoint_parse_number(spec, interp, "beta2", false, ML_OPTIMIZER_DEFAULT_ADAM_BETA2, &beta2);
        if (err != null) return err;
        err = ml_optimizer_checkpoint_parse_number(spec, interp, "epsilon", false, ML_OPTIMIZER_DEFAULT_EPSILON, &epsilon);
        if (err != null) return err;
        if (beta1 < 0.0 || beta1 >= 1.0 || beta2 < 0.0 || beta2 >= 1.0 || epsilon <= 0.0) return ml_optimizer_checkpoint_invalid(interp, "ml/save-optimizer: Adam beta values must be in [0, 1) and epsilon must be positive");
        return null;
    }
    if (nn_symbol_is(kind, interp, "rmsprop")) {
        double alpha = ML_OPTIMIZER_DEFAULT_RMSPROP_ALPHA;
        double epsilon = ML_OPTIMIZER_DEFAULT_EPSILON;
        double momentum = 0.0;
        err = ml_optimizer_checkpoint_parse_number(spec, interp, "alpha", false, ML_OPTIMIZER_DEFAULT_RMSPROP_ALPHA, &alpha);
        if (err != null) return err;
        err = ml_optimizer_checkpoint_parse_number(spec, interp, "epsilon", false, ML_OPTIMIZER_DEFAULT_EPSILON, &epsilon);
        if (err != null) return err;
        err = ml_optimizer_checkpoint_parse_number(spec, interp, "momentum", false, 0.0, &momentum);
        if (err != null) return err;
        if (alpha < 0.0 || alpha >= 1.0 || epsilon <= 0.0 || momentum < 0.0 || momentum >= 1.0) return ml_optimizer_checkpoint_invalid(interp, "ml/save-optimizer: RMSProp alpha and momentum must be in [0, 1), and epsilon must be positive");
        return null;
    }
    return ml_optimizer_checkpoint_invalid(interp, "ml/save-optimizer: unsupported optimizer kind");
}

fn Value* ml_optimizer_checkpoint_bundle(Value* spec, Value* state, Interp* interp) {
    Value* err = ml_optimizer_checkpoint_validate_spec(spec, interp);
    if (err != null) return err;
    if (state == null || state.tag != HASHMAP || state.hashmap_val == null) return ml_optimizer_checkpoint_invalid(interp, "ml/save-optimizer: optimizer state must be a Dictionary");
    Value* bundle = nn_make_dict(interp, 8);
    if (bundle == null || bundle.tag == ERROR) return bundle;
    err = nn_dict_set_symbol(bundle, interp, "kind", "optimizer");
    if (err != null) return err;
    err = nn_dict_set(bundle, interp, "spec", spec);
    if (err != null) return err;
    err = nn_dict_set(bundle, interp, "state", state);
    if (err != null) return err;
    return bundle;
}

fn Value* ml_optimizer_checkpoint_validate_bundle(Value* bundle, Interp* interp) {
    if (bundle == null || bundle.tag != HASHMAP || bundle.hashmap_val == null) return ml_optimizer_checkpoint_invalid(interp, "ml/load-optimizer: checkpoint payload must be an optimizer Dictionary");
    if (!nn_symbol_is(nn_dict_get(bundle, interp, "kind"), interp, "optimizer")) return ml_optimizer_checkpoint_invalid(interp, "ml/load-optimizer: checkpoint payload kind must be optimizer");
    return ml_optimizer_checkpoint_bundle(nn_dict_get(bundle, interp, "spec"), nn_dict_get(bundle, interp, "state"), interp);
}

<*
@require interp != null
*>
fn Value* prim_ml_save_optimizer(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 2 || args.len > 3) return primitive_arity_raise(interp, "ml/save-optimizer", "2 or 3 arguments: spec state [path]", args.len);
    Value* bundle = ml_optimizer_checkpoint_bundle(args[0], args[1], interp);
    if (bundle == null || bundle.tag == ERROR) return bundle;
    Value* json = nn_ckpt_wrap(bundle, interp, "optimizer");
    Value*[2] save_args = { bundle, args.len == 3 ? args[2] : make_nil(interp) };
    return nn_ckpt_save_text_or_path(json, save_args[0:(args.len == 3 ? 2 : 1)], interp, "ml/save-optimizer");
}

<*
@require interp != null
*>
fn Value* prim_ml_load_optimizer(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "ml/load-optimizer", "1 argument: JSON checkpoint string or path", args.len);
    Value* payload = nn_ckpt_parse_root(args[0], interp, "optimizer", "ml/load-optimizer", "ml/load-optimizer: failed to read checkpoint path");
    if (payload == null || payload.tag == ERROR) return payload;
    Value* bundle = nn_ckpt_decode_value(payload, interp);
    if (bundle == null || bundle.tag == ERROR) return bundle;
    return ml_optimizer_checkpoint_validate_bundle(bundle, interp);
}
