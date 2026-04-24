module lisp;

import std::core::mem;
import main;

struct NnSummaryCounts {
    usz layers;
    usz param_tensors;
    usz param_elements;
}

struct NnForwardResult {
    Value* output;
    Value* state;
    bool state_changed;
}

fn Value* nn_apply_invalid(Interp* interp, char[] path, char[] expected, Value* actual) @inline {
    return nn_invalid(interp, path, expected, actual);
}

fn Value* nn_require_model(Value* model, Interp* interp) {
    if (Value* err = nn_validate_dataspec(model, interp)) return err;
    if (!nn_kind_is(model, interp, "model")) {
        return nn_apply_invalid(interp, "model.kind", "model bundle", nn_dict_get(model, interp, "kind"));
    }
    return null;
}

fn Value* nn_model_field(Value* model, Interp* interp, char[] field, char[] path, char[] expected) {
    Value* value = nn_dict_get(model, interp, field);
    if (value == null) return nn_apply_invalid(interp, path, expected, null);
    return value;
}

fn Value* nn_model_accessor(Value*[] args, Interp* interp, char[] prim_name, char[] field, char[] path, char[] expected) {
    if (args.len != 1) return primitive_arity_raise(interp, prim_name, "1 argument: model", args.len);
    if (Value* err = nn_require_model(args[0], interp)) return err;
    return nn_model_field(args[0], interp, field, path, expected);
}

fn Value* nn_apply_validate_options(Value* options, Interp* interp) {
    if (options == null || options.tag != HASHMAP || options.hashmap_val == null) {
        return primitive_arg_type_raise(interp, "nn/apply", 4, "Dictionary options", options);
    }
    for (uint i = 0; i < options.hashmap_val.capacity; i++) {
        HashEntry* entry = &options.hashmap_val.entries[i];
        if (entry.key != null) {
            return nn_apply_invalid(interp, "options", "empty Dictionary reserved for future nn/apply options", options);
        }
    }
    return null;
}

<*
@require interp != null
*>
fn Value* prim_nn_spec(Value*[] args, Env* env, Interp* interp) {
    return nn_model_accessor(args, interp, "nn/spec", "spec", "model.spec", "layer spec");
}

<*
@require interp != null
*>
fn Value* prim_nn_parameters(Value*[] args, Env* env, Interp* interp) {
    return nn_model_accessor(args, interp, "nn/parameters", "params", "model.params", "parameter tree");
}

<*
@require interp != null
*>
fn Value* prim_nn_state(Value*[] args, Env* env, Interp* interp) {
    return nn_model_accessor(args, interp, "nn/state", "state", "model.state", "state tree");
}

<*
@require interp != null
*>
fn Value* prim_nn_mode(Value*[] args, Env* env, Interp* interp) {
    return nn_model_accessor(args, interp, "nn/mode", "mode", "model.mode", "eval or train symbol");
}

fn Value* nn_apply_required_tensor_param(Value* params, Interp* interp, char[] field, char[] path) {
    if (params == null || params.tag != HASHMAP || params.hashmap_val == null) {
        return nn_apply_invalid(interp, path, "Dictionary parameter layer", params);
    }
    Value* value = nn_dict_get(params, interp, field);
    if (value == null || !is_tensor(value)) return nn_apply_invalid(interp, path, "Tensor parameter", value);
    return value;
}

fn Value* nn_apply_optional_tensor_param(Value* params, Interp* interp, char[] field, char[] path) {
    if (params == null || params.tag != HASHMAP || params.hashmap_val == null) {
        return nn_apply_invalid(interp, path, "Dictionary parameter layer", params);
    }
    Value* value = nn_dict_get(params, interp, field);
    if (value == null || value.tag == NIL) return null;
    if (!is_tensor(value)) return nn_apply_invalid(interp, path, "Tensor parameter", value);
    return value;
}

fn Value* nn_apply_required_tensor_state(Value* state, Interp* interp, char[] field, char[] path) {
    if (state == null || state.tag != HASHMAP || state.hashmap_val == null) {
        return nn_apply_invalid(interp, path, "Dictionary state layer", state);
    }
    Value* value = nn_dict_get(state, interp, field);
    if (value == null || !is_tensor(value)) return nn_apply_invalid(interp, path, "Tensor state", value);
    return value;
}

fn Value* nn_apply_activation(Value* activation, Value* input, Interp* interp) {
    if (nn_symbol_is(activation, interp, "relu")) {
        Value*[1] call_args = { input };
        return prim_ml_relu(call_args[0:1], null, interp);
    }
    if (nn_symbol_is(activation, interp, "leaky-relu")) {
        Value*[1] call_args = { input };
        return prim_ml_leaky_relu(call_args[0:1], null, interp);
    }
    if (nn_symbol_is(activation, interp, "sigmoid")) {
        Value*[1] call_args = { input };
        return prim_ml_sigmoid(call_args[0:1], null, interp);
    }
    if (nn_symbol_is(activation, interp, "tanh")) {
        Value*[1] call_args = { input };
        return prim_ml_tanh(call_args[0:1], null, interp);
    }
    if (nn_symbol_is(activation, interp, "gelu")) {
        Value*[1] call_args = { input };
        return prim_ml_gelu(call_args[0:1], null, interp);
    }
    if (nn_symbol_is(activation, interp, "softmax")) {
        Value* axis = make_int(interp, -1);
        Value*[2] call_args = { input, axis };
        return prim_ml_softmax(call_args[0:2], null, interp);
    }
    return nn_apply_invalid(interp, "spec.activation", "supported activation", activation);
}

fn Value* nn_apply_maybe_activation(Value* spec, Value* input, Interp* interp) {
    Value* activation = nn_dict_get(spec, interp, "activation");
    if (activation == null || activation.tag == NIL) return input;
    return nn_apply_activation(activation, input, interp);
}

fn Value* nn_apply_dense(Value* spec, Value* params, Value* input, Interp* interp) {
    Value* weights = nn_apply_required_tensor_param(params, interp, "weights", "params.weights");
    if (weights == null || weights.tag == ERROR) return weights;
    Value* bias = null;
    if (nn_spec_bool_field(spec, interp, "use-bias", true)) {
        bias = nn_apply_required_tensor_param(params, interp, "bias", "params.bias");
        if (bias == null || bias.tag == ERROR) return bias;
    }
    Value*[3] call_args = { input, weights, bias };
    Value* projected = bias == null
        ? prim_ml_linear(call_args[0:2], null, interp)
        : prim_ml_linear(call_args[0:3], null, interp);
    if (projected == null || projected.tag == ERROR) return projected;
    return nn_apply_maybe_activation(spec, projected, interp);
}

fn Value* nn_apply_batch_normalization(Value* spec, Value* params, Value* state, Value* input, Interp* interp) {
    Value* scale = nn_apply_required_tensor_param(params, interp, "scale", "params.scale");
    if (scale == null || scale.tag == ERROR) return scale;
    Value* bias = nn_apply_required_tensor_param(params, interp, "bias", "params.bias");
    if (bias == null || bias.tag == ERROR) return bias;
    Value* running_mean = nn_apply_required_tensor_state(state, interp, "running-mean", "state.running-mean");
    if (running_mean == null || running_mean.tag == ERROR) return running_mean;
    Value* running_variance = nn_apply_required_tensor_state(state, interp, "running-variance", "state.running-variance");
    if (running_variance == null || running_variance.tag == ERROR) return running_variance;
    Value*[7] call_args = {
        input,
        scale,
        bias,
        running_mean,
        running_variance,
        nn_dict_get(spec, interp, "channel-axis"),
        nn_dict_get(spec, interp, "epsilon")
    };
    return prim_ml_batch_normalization(call_args[0:7], null, interp);
}

fn Value* nn_add_channel_bias_cpu(Value* output_value, Value* bias_value, Interp* interp) {
    if (bias_value == null || bias_value.tag == NIL) return output_value;
    if (!is_tensor(output_value) || !is_tensor(bias_value)) {
        return nn_apply_invalid(interp, "params.bias", "Tensor bias for convolution output", bias_value);
    }

    TensorVal* output = null;
    Value* concrete_output_value = null;
    Value* err = tensor_expr_resolve_concrete_cpu(
        output_value,
        &output,
        &concrete_output_value,
        interp,
        "nn/apply: convolution output backing storage is missing",
        "nn/apply: convolution bias add requires CPU tensors until reshape/broadcast lowering is available"
    );
    if (err != null) return err;
    bool cleanup_output = concrete_output_value != output_value;
    defer if (cleanup_output) {
        boundary_cleanup_materialized_value(null, concrete_output_value, interp.current_scope);
    }

    TensorVal* bias = null;
    Value* concrete_bias_value = null;
    err = tensor_expr_resolve_concrete_cpu(
        bias_value,
        &bias,
        &concrete_bias_value,
        interp,
        "nn/apply: convolution bias backing storage is missing",
        "nn/apply: convolution bias add requires CPU tensors until reshape/broadcast lowering is available"
    );
    if (err != null) return err;
    bool cleanup_bias = concrete_bias_value != bias_value;
    defer if (cleanup_bias) {
        boundary_cleanup_materialized_value(null, concrete_bias_value, interp.current_scope);
    }

    if (output == null || bias == null || output.dtype != bias.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "nn/apply: convolution output and bias dtype mismatch", null);
    }
    if ((output.rank != 3 && output.rank != 4) || bias.rank != 1 || output.shape == null || bias.shape == null || output.shape[1] != bias.shape[0]) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "nn/apply: convolution bias shape mismatch", null);
    }
    if (!tensor_dense_row_major(output) || !tensor_dense_row_major(bias)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "nn/apply: convolution bias add requires dense row-major tensors", null);
    }
    if (output.dtype != TENSOR_DTYPE_DOUBLE && output.dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "nn/apply: convolution bias add supports Float64 and Float32 tensors", null);
    }

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = output.rank == 0 ? scalar_shape[0:0] : output.shape[0:output.rank];
    Value* result = make_tensor_uninitialized_checked(interp, output.dtype, shape_slice, "nn/apply: failed to allocate biased convolution output");
    if (result == null || result.tag == ERROR) return result;
    if (result.tag != TENSOR || !tensor_has_valid_storage(result.tensor_val)) {
        return tensor_runtime_raise(interp, "nn/apply: biased convolution output backing storage is missing");
    }

    usz channels = output.shape[1];
    usz spatial = 1;
    for (usz axis = 2; axis < output.rank; axis++) {
        if (!tensor_checked_mul_usz(spatial, output.shape[axis], &spatial)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "nn/apply: convolution output is too large", null);
        }
    }
    usz batch = output.shape[0];
    if (output.dtype == TENSOR_DTYPE_DOUBLE) {
        double* src = (double*)output.data;
        double* bias_data = (double*)bias.data;
        double* dst = (double*)result.tensor_val.data;
        for (usz n = 0; n < batch; n++) {
            for (usz c = 0; c < channels; c++) {
                double b = bias_data[c];
                for (usz s = 0; s < spatial; s++) {
                    usz idx = (n * channels + c) * spatial + s;
                    dst[idx] = src[idx] + b;
                }
            }
        }
    } else {
        float* src = (float*)output.data;
        float* bias_data = (float*)bias.data;
        float* dst = (float*)result.tensor_val.data;
        for (usz n = 0; n < batch; n++) {
            for (usz c = 0; c < channels; c++) {
                float b = bias_data[c];
                for (usz s = 0; s < spatial; s++) {
                    usz idx = (n * channels + c) * spatial + s;
                    dst[idx] = src[idx] + b;
                }
            }
        }
    }
    return result;
}

fn Value* nn_apply_conv1d(Value* spec, Value* params, Value* input, Interp* interp) {
    Value* weights = nn_apply_required_tensor_param(params, interp, "weights", "params.weights");
    if (weights == null || weights.tag == ERROR) return weights;
    Value* bias = null;
    if (nn_spec_bool_field(spec, interp, "use-bias", true)) {
        bias = nn_apply_required_tensor_param(params, interp, "bias", "params.bias");
        if (bias == null || bias.tag == ERROR) return bias;
    }
    Value*[6] call_args = {
        input,
        weights,
        nn_dict_get(spec, interp, "stride"),
        nn_dict_get(spec, interp, "padding"),
        nn_dict_get(spec, interp, "dilation"),
        nn_dict_get(spec, interp, "groups")
    };
    Value* out = prim_ml_conv1d(call_args[0:6], null, interp);
    if (out == null || out.tag == ERROR) return out;
    if (bias != null) {
        out = nn_add_channel_bias_cpu(out, bias, interp);
        if (out == null || out.tag == ERROR) return out;
    }
    return nn_apply_maybe_activation(spec, out, interp);
}

fn Value* nn_apply_conv2d(Value* spec, Value* params, Value* input, Interp* interp) {
    Value* weights = nn_apply_required_tensor_param(params, interp, "weights", "params.weights");
    if (weights == null || weights.tag == ERROR) return weights;
    Value* bias = null;
    if (nn_spec_bool_field(spec, interp, "use-bias", true)) {
        bias = nn_apply_required_tensor_param(params, interp, "bias", "params.bias");
        if (bias == null || bias.tag == ERROR) return bias;
    }
    Value* stride = nn_dict_get(spec, interp, "stride");
    Value* padding = nn_dict_get(spec, interp, "padding");
    Value* dilation = nn_dict_get(spec, interp, "dilation");
    Value*[9] call_args = {
        input,
        weights,
        stride.array_val.items[0],
        stride.array_val.items[1],
        padding.array_val.items[0],
        padding.array_val.items[1],
        dilation.array_val.items[0],
        dilation.array_val.items[1],
        nn_dict_get(spec, interp, "groups")
    };
    Value* out = prim_ml_conv2d(call_args[0:9], null, interp);
    if (out == null || out.tag == ERROR) return out;
    if (bias != null) {
        out = nn_add_channel_bias_cpu(out, bias, interp);
        if (out == null || out.tag == ERROR) return out;
    }
    return nn_apply_maybe_activation(spec, out, interp);
}

fn Value* nn_apply_pool2d(Value* spec, Value* input, Interp* interp, bool average) {
    Value* window = nn_dict_get(spec, interp, "window");
    Value* stride = nn_dict_get(spec, interp, "stride");
    Value* padding = nn_dict_get(spec, interp, "padding");
    Value*[7] call_args = {
        input,
        window.array_val.items[0],
        window.array_val.items[1],
        stride.array_val.items[0],
        stride.array_val.items[1],
        padding.array_val.items[0],
        padding.array_val.items[1]
    };
    return average
        ? prim_ml_avg_pool2d(call_args[0:7], null, interp)
        : prim_ml_max_pool2d(call_args[0:7], null, interp);
}

fn Value* nn_apply_flatten(Value* spec, Value* input_value, Interp* interp) {
    if (!is_tensor(input_value)) return tensor_type_raise(interp, "nn/apply: flatten expected Tensor input");
    TensorVal* input = null;
    Value* concrete_input_value = null;
    Value* err = tensor_expr_resolve_concrete_cpu(
        input_value,
        &input,
        &concrete_input_value,
        interp,
        "nn/apply: flatten input backing storage is missing",
        "nn/apply: flatten requires CPU tensors until Tensor reshape/view lowering is available"
    );
    if (err != null) return err;
    bool cleanup_input = concrete_input_value != input_value;
    defer if (cleanup_input) {
        boundary_cleanup_materialized_value(null, concrete_input_value, interp.current_scope);
    }

    if (input == null || !tensor_has_valid_storage(input) || !tensor_dense_row_major(input)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "nn/apply: flatten requires dense row-major Tensor input", null);
    }
    Value* start_value = nn_dict_get(spec, interp, "start-axis");
    Value* end_value = nn_dict_get(spec, interp, "end-axis");
    usz start_axis = (usz)start_value.int_val;
    usz end_axis = (usz)end_value.int_val;
    if (input.rank == 0 || start_axis >= input.rank || end_axis >= input.rank || start_axis > end_axis) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/axis-out-of-range", "nn/apply: flatten axis out of range", null);
    }

    usz flattened = 1;
    for (usz axis = start_axis; axis <= end_axis; axis++) {
        if (!tensor_checked_mul_usz(flattened, input.shape[axis], &flattened)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "nn/apply: flattened shape is too large", null);
        }
    }
    usz result_rank = input.rank - (end_axis - start_axis);
    usz* result_shape = null;
    if (result_rank > 0) {
        result_shape = (usz*)mem::malloc(usz.sizeof * result_rank);
        if (result_shape == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "nn/apply: failed to allocate flattened shape", null);
        }
    }
    defer if (result_shape != null) mem::free(result_shape);
    for (usz axis = 0; axis < start_axis; axis++) result_shape[axis] = input.shape[axis];
    result_shape[start_axis] = flattened;
    for (usz axis = end_axis + 1; axis < input.rank; axis++) {
        result_shape[start_axis + 1 + (axis - end_axis - 1)] = input.shape[axis];
    }
    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
    Value* result = make_tensor_uninitialized_checked(interp, input.dtype, shape_slice, "nn/apply: failed to allocate flattened tensor");
    if (result == null || result.tag == ERROR) return result;
    if (result.tag != TENSOR || !tensor_has_valid_storage(result.tensor_val)) {
        return tensor_runtime_raise(interp, "nn/apply: flattened tensor backing storage is missing");
    }
    if (!tensor_copy_concrete_storage(result.tensor_val, input)) {
        return tensor_runtime_raise(interp, "nn/apply: failed to copy flattened tensor storage");
    }
    return result;
}

fn Value* nn_batch_norm_channel_axis(Value* spec, TensorVal* input, Interp* interp, usz* out_axis) {
    if (input == null || input.rank == 0 || input.shape == null || input.strides == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "nn/forward: batch-normalization input must have rank >= 1", null);
    }
    Value* axis_value = nn_dict_get(spec, interp, "channel-axis");
    if (axis_value == null || axis_value.tag != INT) {
        return nn_apply_invalid(interp, "spec.channel-axis", "integer channel axis", axis_value);
    }
    long axis = axis_value.int_val;
    long rank = (long)input.rank;
    if (axis < 0) axis += rank;
    if (axis < 0 || axis >= rank) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/axis-out-of-range", "nn/forward: batch-normalization channel axis out of range", null);
    }
    *out_axis = (usz)axis;
    return null;
}

fn Value* nn_batch_norm_resolve_cpu(Value* value, Interp* interp, TensorVal** out, Value** out_value, char[] storage_label, char[] unsupported_label) {
    Value* err = tensor_expr_resolve_concrete_cpu(value, out, out_value, interp, storage_label, unsupported_label);
    if (err != null) return err;
    if (*out == null || !tensor_dense_row_major(*out)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", unsupported_label, null);
    }
    if ((*out).dtype != TENSOR_DTYPE_DOUBLE && (*out).dtype != TENSOR_DTYPE_FLOAT32) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "nn/forward: batch-normalization supports Float64 and Float32 CPU tensors", null);
    }
    return null;
}

fn Value* nn_batch_norm_reject_train_non_cpu_placement(Value* input, Value* scale, Value* bias, Value* running_mean, Value* running_variance, Interp* interp) {
    if (tensor_expr_has_non_cpu_device(input) ||
        tensor_expr_has_non_cpu_device(scale) ||
        tensor_expr_has_non_cpu_device(bias) ||
        tensor_expr_has_non_cpu_device(running_mean) ||
        tensor_expr_has_non_cpu_device(running_variance)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "nn/forward: train-mode batch-normalization requires CPU input, affine params, and running state; Vulkan running-state updates are unsupported", null);
    }
    return null;
}

fn Value* nn_batch_norm_make_stat_tensor(Interp* interp, TensorDType dtype, double* values, usz channels, char[] oom_message) {
    usz[1] shape = { channels };
    Value* tensor = make_tensor_uninitialized_checked(interp, dtype, shape[0:1], oom_message);
    if (tensor == null || tensor.tag == ERROR) return tensor;
    if (tensor.tag != TENSOR || !tensor_has_valid_storage(tensor.tensor_val)) {
        return tensor_runtime_raise(interp, oom_message);
    }
    if (dtype == TENSOR_DTYPE_DOUBLE) {
        double* data = (double*)tensor.tensor_val.data;
        for (usz i = 0; i < channels; i++) data[i] = values[i];
    } else {
        float* data = (float*)tensor.tensor_val.data;
        for (usz i = 0; i < channels; i++) data[i] = (float)values[i];
    }
    return tensor;
}

fn Value* nn_batch_norm_compute_stats(Value* input_value, Value* spec, Interp* interp, TensorVal** out_input, Value** out_input_value, Value** out_mean, Value** out_variance) {
    *out_input = null;
    *out_input_value = null;
    *out_mean = null;
    *out_variance = null;
    Value* err = nn_batch_norm_resolve_cpu(
        input_value,
        interp,
        out_input,
        out_input_value,
        "nn/forward: batch-normalization input backing storage is missing",
        "nn/forward: training batch-normalization currently requires CPU dense row-major tensors"
    );
    if (err != null) return err;
    TensorVal* input = *out_input;
    usz channel_axis = 0;
    if (Value* axis_err = nn_batch_norm_channel_axis(spec, input, interp, &channel_axis)) return axis_err;
    usz channels = input.shape[channel_axis];
    if (channels == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "nn/forward: batch-normalization channel axis must be non-empty", null);
    }
    if (input.element_count == 0 || input.element_count % channels != 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "nn/forward: batch-normalization cannot compute stats for empty channel samples", null);
    }
    usz samples_per_channel = input.element_count / channels;
    if (samples_per_channel == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", "nn/forward: batch-normalization cannot compute stats for empty channel samples", null);
    }

    double* mean = (double*)mem::malloc(double.sizeof * channels);
    if (mean == null) return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "nn/forward: failed to allocate batch mean workspace", null);
    defer mem::free(mean);
    double* variance = (double*)mem::malloc(double.sizeof * channels);
    if (variance == null) return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "nn/forward: failed to allocate batch variance workspace", null);
    defer mem::free(variance);
    for (usz c = 0; c < channels; c++) {
        mean[c] = 0.0;
        variance[c] = 0.0;
    }

    usz channel_stride = input.strides[channel_axis];
    if (input.dtype == TENSOR_DTYPE_DOUBLE) {
        double* data = (double*)input.data;
        for (usz i = 0; i < input.element_count; i++) {
            usz channel = (i / channel_stride) % channels;
            mean[channel] += data[input.storage_offset + i];
        }
        for (usz c = 0; c < channels; c++) mean[c] /= (double)samples_per_channel;
        for (usz i = 0; i < input.element_count; i++) {
            usz channel = (i / channel_stride) % channels;
            double centered = data[input.storage_offset + i] - mean[channel];
            variance[channel] += centered * centered;
        }
    } else {
        float* data = (float*)input.data;
        for (usz i = 0; i < input.element_count; i++) {
            usz channel = (i / channel_stride) % channels;
            mean[channel] += (double)data[input.storage_offset + i];
        }
        for (usz c = 0; c < channels; c++) mean[c] /= (double)samples_per_channel;
        for (usz i = 0; i < input.element_count; i++) {
            usz channel = (i / channel_stride) % channels;
            double centered = (double)data[input.storage_offset + i] - mean[channel];
            variance[channel] += centered * centered;
        }
    }
    for (usz c = 0; c < channels; c++) variance[c] /= (double)samples_per_channel;
    *out_mean = nn_batch_norm_make_stat_tensor(interp, input.dtype, mean, channels, "nn/forward: failed to allocate batch mean tensor");
    if (*out_mean == null || (*out_mean).tag == ERROR) return *out_mean;
    *out_variance = nn_batch_norm_make_stat_tensor(interp, input.dtype, variance, channels, "nn/forward: failed to allocate batch variance tensor");
    if (*out_variance == null || (*out_variance).tag == ERROR) return *out_variance;
    return null;
}

fn Value* nn_batch_norm_update_running(Value* running_value, Value* batch_value, double momentum, Interp* interp, char[] path, Value** out) {
    *out = null;
    TensorVal* running = null;
    Value* concrete_running_value = null;
    Value* err = nn_batch_norm_resolve_cpu(
        running_value,
        interp,
        &running,
        &concrete_running_value,
        "nn/forward: batch-normalization running state backing storage is missing",
        "nn/forward: training batch-normalization running state currently requires CPU dense row-major tensors"
    );
    if (err != null) return err;
    defer if (concrete_running_value != null && concrete_running_value != running_value) {
        boundary_cleanup_materialized_value(null, concrete_running_value, interp.current_scope);
    }
    TensorVal* batch = null;
    Value* concrete_batch_value = null;
    err = nn_batch_norm_resolve_cpu(
        batch_value,
        interp,
        &batch,
        &concrete_batch_value,
        "nn/forward: batch-normalization batch stat backing storage is missing",
        "nn/forward: batch-normalization batch stat must be CPU dense row-major"
    );
    if (err != null) return err;
    defer if (concrete_batch_value != null && concrete_batch_value != batch_value) {
        boundary_cleanup_materialized_value(null, concrete_batch_value, interp.current_scope);
    }
    if (running.dtype != batch.dtype) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/dtype-mismatch", "nn/forward: batch-normalization running state dtype mismatch", null);
    }
    if (running.rank != 1 || batch.rank != 1 || running.shape == null || batch.shape == null || running.shape[0] != batch.shape[0]) {
        return nn_apply_invalid(interp, path, "rank-1 running stat tensor matching batch channels", running_value);
    }
    Value* next = make_tensor_uninitialized_checked(interp, running.dtype, running.shape[0:1], "nn/forward: failed to allocate updated batch-normalization running stat");
    if (next == null || next.tag == ERROR) return next;
    if (next.tag != TENSOR || !tensor_has_valid_storage(next.tensor_val)) {
        return tensor_runtime_raise(interp, "nn/forward: updated batch-normalization running stat backing storage is missing");
    }
    double keep = 1.0 - momentum;
    if (running.dtype == TENSOR_DTYPE_DOUBLE) {
        double* old_data = (double*)running.data;
        double* batch_data = (double*)batch.data;
        double* next_data = (double*)next.tensor_val.data;
        for (usz i = 0; i < running.shape[0]; i++) {
            next_data[i] = old_data[running.storage_offset + i] * keep + batch_data[batch.storage_offset + i] * momentum;
        }
    } else {
        float* old_data = (float*)running.data;
        float* batch_data = (float*)batch.data;
        float* next_data = (float*)next.tensor_val.data;
        for (usz i = 0; i < running.shape[0]; i++) {
            next_data[i] = (float)((double)old_data[running.storage_offset + i] * keep + (double)batch_data[batch.storage_offset + i] * momentum);
        }
    }
    *out = next;
    return null;
}

fn Value* nn_forward_batch_normalization(Value* spec, Value* params, Value* state, Value* input, Interp* interp, NnForwardResult* out) {
    Value* scale = nn_apply_required_tensor_param(params, interp, "scale", "params.scale");
    if (scale == null || scale.tag == ERROR) return scale;
    Value* bias = nn_apply_required_tensor_param(params, interp, "bias", "params.bias");
    if (bias == null || bias.tag == ERROR) return bias;
    Value* running_mean = nn_apply_required_tensor_state(state, interp, "running-mean", "state.running-mean");
    if (running_mean == null || running_mean.tag == ERROR) return running_mean;
    Value* running_variance = nn_apply_required_tensor_state(state, interp, "running-variance", "state.running-variance");
    if (running_variance == null || running_variance.tag == ERROR) return running_variance;
    if (Value* placement_err = nn_batch_norm_reject_train_non_cpu_placement(input, scale, bias, running_mean, running_variance, interp)) return placement_err;

    TensorVal* input_tensor = null;
    Value* concrete_input = null;
    Value* batch_mean = null;
    Value* batch_variance = null;
    if (Value* err = nn_batch_norm_compute_stats(input, spec, interp, &input_tensor, &concrete_input, &batch_mean, &batch_variance)) return err;
    defer if (concrete_input != null && concrete_input != input) {
        boundary_cleanup_materialized_value(null, concrete_input, interp.current_scope);
    }

    Value*[7] call_args = {
        concrete_input,
        scale,
        bias,
        batch_mean,
        batch_variance,
        nn_dict_get(spec, interp, "channel-axis"),
        nn_dict_get(spec, interp, "epsilon")
    };
    Value* output = prim_ml_batch_normalization(call_args[0:7], null, interp);
    if (output == null || output.tag == ERROR) return output;
    double momentum = 0.0;
    if (!try_numeric_to_double(nn_dict_get(spec, interp, "momentum"), &momentum) || !conversion_double_is_finite(momentum)) {
        return nn_apply_invalid(interp, "spec.momentum", "finite number in [0, 1]", nn_dict_get(spec, interp, "momentum"));
    }
    Value* next_mean = null;
    if (Value* err = nn_batch_norm_update_running(running_mean, batch_mean, momentum, interp, "state.running-mean", &next_mean)) return err;
    Value* next_variance = null;
    if (Value* err = nn_batch_norm_update_running(running_variance, batch_variance, momentum, interp, "state.running-variance", &next_variance)) return err;
    Value* next_state = nn_make_dict(interp, 4);
    if (next_state == null || next_state.tag == ERROR) return next_state;
    if (Value* err = nn_dict_set(next_state, interp, "running-mean", next_mean)) return err;
    if (Value* err = nn_dict_set(next_state, interp, "running-variance", next_variance)) return err;
    out.output = output;
    out.state = next_state;
    out.state_changed = true;
    return null;
}

fn Value* nn_forward_layer(Value* spec, Value* params, Value* state, Value* input, bool train, Interp* interp, NnForwardResult* out) {
    *out = {};
    if (train && nn_kind_is(spec, interp, "batch-normalization")) {
        return nn_forward_batch_normalization(spec, params, state, input, interp, out);
    }
    if (nn_kind_is(spec, interp, "sequential")) {
        Value* layers = nn_dict_get(spec, interp, "layers");
        if (params == null || params.tag != ARRAY || params.array_val == null ||
            state == null || state.tag != ARRAY || state.array_val == null ||
            params.array_val.length != layers.array_val.length ||
            state.array_val.length != layers.array_val.length) {
            return nn_apply_invalid(interp, "params", "parameter/state arrays matching sequential layers", params);
        }
        Value* current = input;
        Value* next_state_array = null;
        for (usz i = 0; i < layers.array_val.length; i++) {
            NnForwardResult child = {};
            if (Value* err = nn_forward_layer(layers.array_val.items[i], params.array_val.items[i], state.array_val.items[i], current, train, interp, &child)) return err;
            current = child.output;
            if (child.state_changed && next_state_array == null) {
                next_state_array = make_array_checked(interp, layers.array_val.length, "nn/forward: out of memory");
                if (next_state_array == null || next_state_array.tag == ERROR) return next_state_array;
                if (next_state_array.array_val == null) return nn_oom(interp);
                for (usz j = 0; j < i; j++) {
                    next_state_array.array_val.items[j] = state.array_val.items[j];
                    next_state_array.array_val.length++;
                }
            }
            if (next_state_array != null) {
                next_state_array.array_val.items[i] = child.state_changed ? child.state : state.array_val.items[i];
                next_state_array.array_val.length++;
            }
        }
        out.output = current;
        out.state = next_state_array != null ? next_state_array : state;
        out.state_changed = next_state_array != null;
        return null;
    }
    Value* output = nn_apply_layer(spec, params, state, input, interp);
    if (output == null || output.tag == ERROR) return output;
    out.output = output;
    out.state = state;
    out.state_changed = false;
    return null;
}

fn Value* nn_apply_layer(Value* spec, Value* params, Value* state, Value* input, Interp* interp) {
    if (nn_kind_is(spec, interp, "sequential")) {
        Value* layers = nn_dict_get(spec, interp, "layers");
        if (params == null || params.tag != ARRAY || params.array_val == null ||
            state == null || state.tag != ARRAY || state.array_val == null ||
            params.array_val.length != layers.array_val.length ||
            state.array_val.length != layers.array_val.length) {
            return nn_apply_invalid(interp, "params", "parameter/state arrays matching sequential layers", params);
        }
        Value* current = input;
        for (usz i = 0; i < layers.array_val.length; i++) {
            current = nn_apply_layer(layers.array_val.items[i], params.array_val.items[i], state.array_val.items[i], current, interp);
            if (current == null || current.tag == ERROR) return current;
        }
        return current;
    }
    if (nn_kind_is(spec, interp, "dense")) return nn_apply_dense(spec, params, input, interp);
    if (nn_kind_is(spec, interp, "batch-normalization")) return nn_apply_batch_normalization(spec, params, state, input, interp);
    if (nn_kind_is(spec, interp, "conv1d")) return nn_apply_conv1d(spec, params, input, interp);
    if (nn_kind_is(spec, interp, "conv2d")) return nn_apply_conv2d(spec, params, input, interp);
    if (nn_kind_is(spec, interp, "max-pool2d")) return nn_apply_pool2d(spec, input, interp, false);
    if (nn_kind_is(spec, interp, "avg-pool2d")) return nn_apply_pool2d(spec, input, interp, true);
    if (nn_kind_is(spec, interp, "flatten")) return nn_apply_flatten(spec, input, interp);
    if (nn_kind_is(spec, interp, "activation")) return nn_apply_activation(nn_dict_get(spec, interp, "activation"), input, interp);
    return nn_apply_invalid(interp, "spec.kind", "supported inference layer kind", nn_dict_get(spec, interp, "kind"));
}

fn Value* nn_parse_apply(Value*[] args, Interp* interp, Value** spec_out, Value** params_out, Value** state_out, Value** input_out) {
    *spec_out = null;
    *params_out = null;
    *state_out = null;
    *input_out = null;
    if (args.len == 2) {
        if (Value* err = nn_require_model(args[0], interp)) return err;
        *spec_out = nn_model_field(args[0], interp, "spec", "model.spec", "layer spec");
        *params_out = nn_model_field(args[0], interp, "params", "model.params", "parameter tree");
        *state_out = nn_model_field(args[0], interp, "state", "model.state", "state tree");
        *input_out = args[1];
        return null;
    }
    if (args.len == 4 || args.len == 5) {
        *spec_out = args[0];
        *params_out = args[1];
        *state_out = args[2];
        *input_out = args[3];
        if (args.len == 5) {
            if (Value* err = nn_apply_validate_options(args[4], interp)) return err;
        }
        if (Value* err = nn_validate_layer_spec(*spec_out, interp)) return err;
        if (Value* err = nn_validate_tree(*params_out, interp, "params")) return err;
        if (Value* err = nn_validate_tree(*state_out, interp, "state")) return err;
        return null;
    }
    return primitive_arity_raise(interp, "nn/apply", "2 arguments: model input, or 4/5 arguments: spec params state input [options]", args.len);
}

fn bool nn_forward_option_key_allowed(Value* key, Interp* interp) {
    return key != null && key.tag == SYMBOL && nn_symbol_name_is(key.sym_val, interp, "mode");
}

fn Value* nn_forward_validate_options(Value* options, Interp* interp, Value** mode_out) {
    *mode_out = make_symbol(interp, interp.symbols.intern("eval"));
    if (*mode_out == null || (*mode_out).tag == ERROR) return *mode_out;
    if (options == null || options.tag == NIL) return null;
    if (options.tag != HASHMAP || options.hashmap_val == null) {
        return primitive_arg_type_raise(interp, "nn/forward", 4, "Dictionary options", options);
    }
    for (uint i = 0; i < options.hashmap_val.capacity; i++) {
        HashEntry* entry = &options.hashmap_val.entries[i];
        if (entry.key == null) continue;
        if (!nn_forward_option_key_allowed(entry.key, interp)) {
            return nn_apply_invalid(interp, "options", "Dictionary with optional mode", entry.key);
        }
    }
    Value* mode = nn_dict_get(options, interp, "mode");
    if (mode != null) {
        if (!nn_symbol_is(mode, interp, "eval") && !nn_symbol_is(mode, interp, "train")) {
            return nn_apply_invalid(interp, "options.mode", "eval or train symbol", mode);
        }
        *mode_out = mode;
    }
    return null;
}

fn Value* nn_parse_forward(Value*[] args, Interp* interp, Value** model_out, Value** spec_out, Value** params_out, Value** state_out, Value** input_out, Value** mode_out) {
    *model_out = null;
    *spec_out = null;
    *params_out = null;
    *state_out = null;
    *input_out = null;
    *mode_out = null;
    if (args.len == 2) {
        if (Value* err = nn_require_model(args[0], interp)) return err;
        *model_out = args[0];
        *spec_out = nn_model_field(args[0], interp, "spec", "model.spec", "layer spec");
        *params_out = nn_model_field(args[0], interp, "params", "model.params", "parameter tree");
        *state_out = nn_model_field(args[0], interp, "state", "model.state", "state tree");
        *mode_out = nn_model_field(args[0], interp, "mode", "model.mode", "eval or train symbol");
        *input_out = args[1];
        return null;
    }
    if (args.len == 4 || args.len == 5) {
        *spec_out = args[0];
        *params_out = args[1];
        *state_out = args[2];
        *input_out = args[3];
        if (Value* err = nn_forward_validate_options(args.len == 5 ? args[4] : null, interp, mode_out)) return err;
        if (Value* err = nn_validate_layer_spec(*spec_out, interp)) return err;
        if (Value* err = nn_validate_tree(*params_out, interp, "params")) return err;
        if (Value* err = nn_validate_tree(*state_out, interp, "state")) return err;
        return null;
    }
    return primitive_arity_raise(interp, "nn/forward", "2 arguments: model input, or 4/5 arguments: spec params state input [options]", args.len);
}

fn Value* nn_model_with_state(Value* model, Value* state, Interp* interp) {
    Value* next = nn_make_dict(interp, 12);
    if (next == null || next.tag == ERROR) return next;
    if (Value* err = nn_dict_set_symbol(next, interp, "kind", "model")) return err;
    if (Value* err = nn_dict_set(next, interp, "spec", nn_model_field(model, interp, "spec", "model.spec", "layer spec"))) return err;
    if (Value* err = nn_dict_set(next, interp, "params", nn_model_field(model, interp, "params", "model.params", "parameter tree"))) return err;
    if (Value* err = nn_dict_set(next, interp, "state", state)) return err;
    if (Value* err = nn_dict_set(next, interp, "mode", nn_model_field(model, interp, "mode", "model.mode", "eval or train symbol"))) return err;
    if (Value* err = nn_dict_set(next, interp, "dtype", nn_model_field(model, interp, "dtype", "model.dtype", "Float64 or Float32 symbol"))) return err;
    if (Value* err = nn_dict_set(next, interp, "device", nn_model_field(model, interp, "device", "model.device", "device symbol"))) return err;
    if (Value* err = nn_dict_set(next, interp, "metadata", nn_model_field(model, interp, "metadata", "model.metadata", "metadata dictionary"))) return err;
    if (Value* err = nn_validate_dataspec(next, interp)) return err;
    return next;
}

fn Value* nn_forward_payload(Value* model, Value* spec, Value* params, NnForwardResult* forward, Interp* interp) {
    if (!forward.state_changed) return forward.output;
    Value* result = nn_make_dict(interp, model == null ? 10 : 12);
    if (result == null || result.tag == ERROR) return result;
    if (Value* err = nn_dict_set_symbol(result, interp, "kind", "nn-forward")) return err;
    if (Value* err = nn_dict_set(result, interp, "output", forward.output)) return err;
    if (Value* err = nn_dict_set(result, interp, "spec", spec)) return err;
    if (Value* err = nn_dict_set(result, interp, "params", params)) return err;
    if (Value* err = nn_dict_set(result, interp, "state", forward.state)) return err;
    if (model != null) {
        Value* next_model = nn_model_with_state(model, forward.state, interp);
        if (next_model == null || next_model.tag == ERROR) return next_model;
        if (Value* err = nn_dict_set(result, interp, "model", next_model)) return err;
    }
    return result;
}

fn Value* nn_forward_execute(Value*[] args, Interp* interp) {
    Value* model = null;
    Value* spec = null;
    Value* params = null;
    Value* state = null;
    Value* input = null;
    Value* mode = null;
    if (Value* err = nn_parse_forward(args, interp, &model, &spec, &params, &state, &input, &mode)) return err;
    if (!is_tensor(input)) return tensor_type_raise(interp, "nn/forward: input must be a Tensor");
    bool train = nn_symbol_is(mode, interp, "train");
    NnForwardResult forward = {};
    if (Value* err = nn_forward_layer(spec, params, state, input, train, interp, &forward)) return err;
    return nn_forward_payload(model, spec, params, &forward, interp);
}

<*
@require interp != null
*>
fn Value* prim_nn_apply(Value*[] args, Env* env, Interp* interp) {
    Value* spec = null;
    Value* params = null;
    Value* state = null;
    Value* input = null;
    if (Value* err = nn_parse_apply(args, interp, &spec, &params, &state, &input)) return err;
    if (!is_tensor(input)) return tensor_type_raise(interp, "nn/apply: input must be a Tensor");
    return nn_apply_layer(spec, params, state, input, interp);
}

<*
@require interp != null
*>
fn Value* prim_nn_predict(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return primitive_arity_raise(interp, "nn/predict", "2 arguments: model input", args.len);
    if (Value* err = nn_require_model(args[0], interp)) return err;
    Value* mode = nn_model_field(args[0], interp, "mode", "model.mode", "eval mode");
    if (mode == null || mode.tag == ERROR) return mode;
    if (!nn_symbol_is(mode, interp, "eval")) return nn_apply_invalid(interp, "model.mode", "eval mode", mode);
    Value*[2] call_args = { args[0], args[1] };
    return prim_nn_apply(call_args[0:2], null, interp);
}

fn bool nn_summary_add_usz(usz* target, usz amount) {
    if (target == null || *target > usz.max - amount) return false;
    *target += amount;
    return true;
}

fn Value* nn_summary_count_params(Value* tree, Interp* interp, NnSummaryCounts* counts) {
    if (tree == null || tree.tag == NIL) return null;
    if (is_tensor(tree)) {
        if (!nn_summary_add_usz(&counts.param_tensors, 1)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "nn/summary: parameter tensor count overflow", null);
        }
        TensorVal* tensor = tree.tensor_val;
        if (tensor == null) return nn_apply_invalid(interp, "model.params", "valid Tensor parameter", tree);
        if (!nn_summary_add_usz(&counts.param_elements, tensor.element_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "nn/summary: parameter element count overflow", null);
        }
        return null;
    }
    if (tree.tag == ARRAY && tree.array_val != null) {
foreach (item : tree.array_val.items[0:tree.array_val.length]) {
            if (Value* err = nn_summary_count_params(item, interp, counts)) return err;
}
        return null;
    }
    if (tree.tag == HASHMAP && tree.hashmap_val != null) {
        for (uint i = 0; i < tree.hashmap_val.capacity; i++) {
            HashEntry* entry = &tree.hashmap_val.entries[i];
            if (entry.key == null) continue;
            if (Value* err = nn_summary_count_params(entry.value, interp, counts)) return err;
        }
        return null;
    }
    if (is_cons(tree)) {
        Value* cur = tree;
        while (is_cons(cur)) {
            if (Value* err = nn_summary_count_params(cur.cons_val.car, interp, counts)) return err;
            cur = cur.cons_val.cdr;
        }
        if (!is_nil(cur)) return nn_apply_invalid(interp, "model.params", "proper list parameter tree", cur);
        return null;
    }
    return nn_apply_invalid(interp, "model.params", "nil, Tensor, Array, List, or Dictionary parameter tree", tree);
}

fn Value* nn_summary_count_layer_tree(Value* spec, Interp* interp, NnSummaryCounts* counts) {
    if (spec == null || counts == null) return nn_apply_invalid(interp, "spec", "layer spec", spec);
    if (nn_kind_is(spec, interp, "sequential")) {
        Value* layers = nn_dict_get(spec, interp, "layers");
        if (layers == null || layers.tag != ARRAY || layers.array_val == null) {
            return nn_apply_invalid(interp, "spec.layers", "Array layer specs", layers);
        }
foreach (item : layers.array_val.items[0:layers.array_val.length]) {
            if (Value* err = nn_summary_count_layer_tree(item, interp, counts)) return err;
}
        return null;
    }
    if (!nn_summary_add_usz(&counts.layers, 1)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "nn/summary: layer count overflow", null);
    }
    return null;
}

fn Value* nn_make_count_value(Interp* interp, usz count, char[] label) {
    if (count > (usz)long.max) return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", label, null);
    return make_int(interp, (long)count);
}

<*
@require interp != null
*>
fn Value* prim_nn_summary(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1) return primitive_arity_raise(interp, "nn/summary", "1 argument: model", args.len);
    if (Value* err = nn_require_model(args[0], interp)) return err;
    Value* spec = nn_model_field(args[0], interp, "spec", "model.spec", "layer spec");
    if (spec == null || spec.tag == ERROR) return spec;
    Value* params = nn_model_field(args[0], interp, "params", "model.params", "parameter tree");
    if (params == null || params.tag == ERROR) return params;
    NnSummaryCounts counts = {};
    if (Value* err = nn_summary_count_layer_tree(spec, interp, &counts)) return err;
    if (Value* err = nn_summary_count_params(params, interp, &counts)) return err;

    Value* summary = nn_make_dict(interp, 16);
    if (summary == null || summary.tag == ERROR) return summary;
    if (Value* err = nn_dict_set_symbol(summary, interp, "kind", "summary")) return err;
    if (Value* err = nn_dict_set(summary, interp, "mode", nn_model_field(args[0], interp, "mode", "model.mode", "eval or train symbol"))) return err;
    if (Value* err = nn_dict_set(summary, interp, "dtype", nn_model_field(args[0], interp, "dtype", "model.dtype", "Float64 or Float32 symbol"))) return err;
    if (Value* err = nn_dict_set(summary, interp, "device", nn_model_field(args[0], interp, "device", "model.device", "device symbol"))) return err;
    if (Value* err = nn_dict_set(summary, interp, "layers", nn_make_count_value(interp, counts.layers, "nn/summary: layer count too large"))) return err;
    if (Value* err = nn_dict_set(summary, interp, "parameter-tensors", nn_make_count_value(interp, counts.param_tensors, "nn/summary: parameter tensor count too large"))) return err;
    if (Value* err = nn_dict_set(summary, interp, "parameter-elements", nn_make_count_value(interp, counts.param_elements, "nn/summary: parameter element count too large"))) return err;
    return summary;
}
