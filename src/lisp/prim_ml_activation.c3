module lisp;

import main;

fn Value* ml_activation_scalar_for_dtype(TensorDType dtype, Interp* interp, double f64_value, float f32_value, char[] primitive_name) {
    switch (dtype) {
        case TENSOR_DTYPE_DOUBLE:
            return make_double(interp, f64_value);
        case TENSOR_DTYPE_FLOAT32:
            return make_float32(interp, f32_value);
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

fn Value* ml_activation_validate_input(Value* input, TensorVal** out_source, Interp* interp, bool allow_float64, char[] primitive_name) {
    if (!is_tensor(input)) return tensor_type_raise(interp, primitive_name);
    TensorVal* source = null;
    Value* err = tensor_map_source_storage(input, &source, interp);
    if (err != null) return err;
    if (source == null) return tensor_runtime_raise(interp, primitive_name);
    if (source.dtype != TENSOR_DTYPE_FLOAT32 && !(allow_float64 && source.dtype == TENSOR_DTYPE_DOUBLE)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", primitive_name, null);
    }
    *out_source = source;
    return null;
}

fn Value* ml_activation_map_unary(Value* func, Value* input, Env* env, Interp* interp) {
    Value*[2] map_args = { func, input };
    return prim_tensor_map(map_args[..], env, interp);
}

fn Value* ml_activation_map_binary(Value* func, Value* left, Value* right, Env* env, Interp* interp) {
    Value*[3] map_args = { func, left, right };
    return prim_tensor_map(map_args[..], env, interp);
}

fn Value* prim_ml_relu(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "ml/relu", "1 argument: input", args.len);
    TensorVal* source = null;
    Value* err = ml_activation_validate_input(args[0], &source, interp, true, "ml/relu: currently supports Float64 and Float32 Tensor inputs");
    if (err != null) return err;

    Value* max_func = ml_activation_lookup_primitive(interp, "max", "ml/relu: max primitive is unavailable");
    if (max_func == null || max_func.tag == ERROR) return max_func;
    Value* zero = ml_activation_scalar_for_dtype(source.dtype, interp, 0.0, 0.0f, "ml/relu: currently supports Float64 and Float32 Tensor inputs");
    if (zero == null || zero.tag == ERROR) return zero;

    return ml_activation_map_binary(max_func, args[0], zero, env, interp);
}

fn Value* prim_ml_sigmoid(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "ml/sigmoid", "1 argument: input", args.len);
    TensorVal* source = null;
    Value* err = ml_activation_validate_input(args[0], &source, interp, false, "ml/sigmoid: currently supports Float32 Tensor inputs");
    if (err != null) return err;

    Value* sub_func = ml_activation_lookup_primitive(interp, "-", "ml/sigmoid: subtraction primitive is unavailable");
    if (sub_func == null || sub_func.tag == ERROR) return sub_func;
    Value* exp_func = ml_activation_lookup_primitive(interp, "exp", "ml/sigmoid: exp primitive is unavailable");
    if (exp_func == null || exp_func.tag == ERROR) return exp_func;
    Value* add_func = ml_activation_lookup_primitive(interp, "+", "ml/sigmoid: addition primitive is unavailable");
    if (add_func == null || add_func.tag == ERROR) return add_func;
    Value* div_func = ml_activation_lookup_primitive(interp, "/", "ml/sigmoid: division primitive is unavailable");
    if (div_func == null || div_func.tag == ERROR) return div_func;

    Value* one = ml_activation_scalar_for_dtype(source.dtype, interp, 1.0, 1.0f, "ml/sigmoid: currently supports Float32 Tensor inputs");
    if (one == null || one.tag == ERROR) return one;
    Value* neg_input = ml_activation_map_unary(sub_func, args[0], env, interp);
    if (neg_input == null || neg_input.tag == ERROR) return neg_input;
    Value* exp_neg_input = ml_activation_map_unary(exp_func, neg_input, env, interp);
    if (exp_neg_input == null || exp_neg_input.tag == ERROR) return exp_neg_input;
    Value* denom = ml_activation_map_binary(add_func, exp_neg_input, one, env, interp);
    if (denom == null || denom.tag == ERROR) return denom;
    return ml_activation_map_binary(div_func, one, denom, env, interp);
}

fn Value* prim_ml_tanh(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "ml/tanh", "1 argument: input", args.len);
    TensorVal* source = null;
    Value* err = ml_activation_validate_input(args[0], &source, interp, false, "ml/tanh: currently supports Float32 Tensor inputs");
    if (err != null) return err;

    Value* tanh_func = ml_activation_lookup_primitive(interp, "tanh", "ml/tanh: tanh primitive is unavailable");
    if (tanh_func == null || tanh_func.tag == ERROR) return tanh_func;
    return ml_activation_map_unary(tanh_func, args[0], env, interp);
}

fn Value* prim_ml_gelu(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "ml/gelu", "1 argument: input", args.len);
    TensorVal* source = null;
    Value* err = ml_activation_validate_input(args[0], &source, interp, false, "ml/gelu: currently supports Float32 Tensor inputs");
    if (err != null) return err;

    Value* add_func = ml_activation_lookup_primitive(interp, "+", "ml/gelu: addition primitive is unavailable");
    if (add_func == null || add_func.tag == ERROR) return add_func;
    Value* mul_func = ml_activation_lookup_primitive(interp, "*", "ml/gelu: multiplication primitive is unavailable");
    if (mul_func == null || mul_func.tag == ERROR) return mul_func;
    Value* tanh_func = ml_activation_lookup_primitive(interp, "tanh", "ml/gelu: tanh primitive is unavailable");
    if (tanh_func == null || tanh_func.tag == ERROR) return tanh_func;

    Value* one = ml_activation_scalar_for_dtype(source.dtype, interp, 1.0, 1.0f, "ml/gelu: currently supports Float32 Tensor inputs");
    if (one == null || one.tag == ERROR) return one;
    Value* half = ml_activation_scalar_for_dtype(source.dtype, interp, 0.5, 0.5f, "ml/gelu: currently supports Float32 Tensor inputs");
    if (half == null || half.tag == ERROR) return half;
    Value* cube_scale = ml_activation_scalar_for_dtype(source.dtype, interp, 0.044715, 0.044715f, "ml/gelu: currently supports Float32 Tensor inputs");
    if (cube_scale == null || cube_scale.tag == ERROR) return cube_scale;
    Value* sqrt_two_over_pi = ml_activation_scalar_for_dtype(source.dtype, interp, 0.7978845608028654, 0.7978846f, "ml/gelu: currently supports Float32 Tensor inputs");
    if (sqrt_two_over_pi == null || sqrt_two_over_pi.tag == ERROR) return sqrt_two_over_pi;

    Value* x_squared = ml_activation_map_binary(mul_func, args[0], args[0], env, interp);
    if (x_squared == null || x_squared.tag == ERROR) return x_squared;
    Value* x_cubed = ml_activation_map_binary(mul_func, x_squared, args[0], env, interp);
    if (x_cubed == null || x_cubed.tag == ERROR) return x_cubed;
    Value* scaled_cube = ml_activation_map_binary(mul_func, x_cubed, cube_scale, env, interp);
    if (scaled_cube == null || scaled_cube.tag == ERROR) return scaled_cube;
    Value* inner_sum = ml_activation_map_binary(add_func, args[0], scaled_cube, env, interp);
    if (inner_sum == null || inner_sum.tag == ERROR) return inner_sum;
    Value* tanh_arg = ml_activation_map_binary(mul_func, inner_sum, sqrt_two_over_pi, env, interp);
    if (tanh_arg == null || tanh_arg.tag == ERROR) return tanh_arg;
    Value* tanh_value = ml_activation_map_unary(tanh_func, tanh_arg, env, interp);
    if (tanh_value == null || tanh_value.tag == ERROR) return tanh_value;
    Value* one_plus_tanh = ml_activation_map_binary(add_func, one, tanh_value, env, interp);
    if (one_plus_tanh == null || one_plus_tanh.tag == ERROR) return one_plus_tanh;
    Value* half_x = ml_activation_map_binary(mul_func, half, args[0], env, interp);
    if (half_x == null || half_x.tag == ERROR) return half_x;
    return ml_activation_map_binary(mul_func, half_x, one_plus_tanh, env, interp);
}
