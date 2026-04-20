module lisp;

import std::core::mem;
import main;

extern fn double ml_c_exp(double x) @extern("exp");
extern fn double ml_c_log(double x) @extern("log");

fn Value* ml_stable_exp_stats_cpu(
    TensorVal* source,
    usz* axes,
    usz axis_count,
    usz output_count,
    Interp* interp,
    char[] index_label,
    double** out_maxes,
    double** out_exp_sums
) {
    if (source == null || axes == null || out_maxes == null || out_exp_sums == null) {
        return tensor_runtime_raise(interp, index_label);
    }
    *out_maxes = null;
    *out_exp_sums = null;

    usz bytes = 0;
    if (!tensor_checked_mul_usz(output_count, double.sizeof, &bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml stable reduction: accumulator is too large", null);
    }
    double* maxes = (double*)mem::malloc(bytes);
    if (maxes == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml stable reduction: failed to allocate max accumulator", null);
    }
    bool keep_maxes = false;
    defer if (!keep_maxes) mem::free(maxes);

    double* exp_sums = (double*)mem::malloc(bytes);
    if (exp_sums == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml stable reduction: failed to allocate exp accumulator", null);
    }
    bool keep_exp_sums = false;
    defer if (!keep_exp_sums) mem::free(exp_sums);

    usz seen_bytes = 0;
    if (!tensor_checked_mul_usz(output_count, bool.sizeof, &seen_bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml stable reduction: accumulator flags are too large", null);
    }
    bool* seen = (bool*)mem::malloc(seen_bytes);
    if (seen == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml stable reduction: failed to allocate accumulator flags", null);
    }
    defer mem::free(seen);

    for (usz i = 0; i < output_count; i++) {
        maxes[i] = 0.0;
        exp_sums[i] = 0.0;
        seen[i] = false;
    }

    for (usz i = 0; i < source.element_count; i++) {
        usz result_index = 0;
        double value = 0.0;
        if (!ml_reduction_result_index(source, i, axes, axis_count, &result_index) ||
            result_index >= output_count ||
            !ml_reduction_read(source, i, &value)) {
            return tensor_runtime_raise(interp, index_label);
        }
        if (!seen[result_index] || value > maxes[result_index]) {
            maxes[result_index] = value;
            seen[result_index] = true;
        }
    }

    for (usz i = 0; i < source.element_count; i++) {
        usz result_index = 0;
        double value = 0.0;
        if (!ml_reduction_result_index(source, i, axes, axis_count, &result_index) ||
            result_index >= output_count ||
            !ml_reduction_read(source, i, &value)) {
            return tensor_runtime_raise(interp, index_label);
        }
        exp_sums[result_index] += ml_c_exp(value - maxes[result_index]);
    }

    keep_maxes = true;
    keep_exp_sums = true;
    *out_maxes = maxes;
    *out_exp_sums = exp_sums;
    return null;
}

fn Value* prim_ml_logsumexp(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return primitive_arity_raise(interp, "ml/logsumexp", "2 arguments: input axes", args.len);
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "ml/logsumexp: expected Tensor input");

    TensorVal* source = null;
    Value* source_value = null;
    Value* err = tensor_expr_resolve_concrete_cpu(
        args[0],
        &source,
        &source_value,
        interp,
        "ml/logsumexp: tensor backing storage is missing",
        "ml/logsumexp: backend kernel is not implemented for non-CPU tensors"
    );
    if (err != null) return err;
    bool cleanup_source = source_value != args[0];
    defer if (cleanup_source) {
        boundary_cleanup_materialized_value(null, source_value, interp.current_scope);
    }

    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "ml/logsumexp: tensor backing storage is missing");
    if (!ml_reduction_supported_dtype(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/logsumexp: currently supports Float64 and Float32 tensors", null);
    }

    usz* axes = null;
    usz axis_count = 0;
    err = ml_reduction_parse_axes(
        args[1],
        source,
        interp,
        "ml/logsumexp: axes must be an integer, array, or proper list of integers",
        "ml/logsumexp: axis out of range",
        "ml/logsumexp: duplicate reduction axis",
        "ml/logsumexp: expected at least one reduction axis",
        &axes,
        &axis_count
    );
    if (err != null) return err;
    defer mem::free(axes);

    usz* result_shape = null;
    usz result_rank = 0;
    usz reduction_count = 0;
    err = ml_reduction_result_shape(
        source,
        axes,
        axis_count,
        interp,
        "ml/logsumexp: result shape is too large",
        "ml/logsumexp: cannot reduce an empty axis",
        &result_shape,
        &result_rank,
        &reduction_count
    );
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "ml/logsumexp: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/logsumexp: result tensor backing storage is missing");
    }
    if (out_value.tensor_val.element_count == 0) {
        keep_out = true;
        return out_value;
    }

    double* maxes = null;
    double* exp_sums = null;
    err = ml_stable_exp_stats_cpu(
        source,
        axes,
        axis_count,
        out_value.tensor_val.element_count,
        interp,
        "ml/logsumexp: tensor index offset is invalid",
        &maxes,
        &exp_sums
    );
    if (err != null) return err;
    defer mem::free(maxes);
    defer mem::free(exp_sums);

    for (usz i = 0; i < out_value.tensor_val.element_count; i++) {
        exp_sums[i] = maxes[i] + ml_c_log(exp_sums[i]);
    }
    err = ml_reduction_write_result(out_value.tensor_val, exp_sums, interp, "ml/logsumexp: Float32 result is not representable");
    if (err != null) return err;
    keep_out = true;
    return out_value;
}

fn Value* prim_ml_softmax(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2) return primitive_arity_raise(interp, "ml/softmax", "2 arguments: input axis", args.len);
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "ml/softmax: expected Tensor input");

    TensorVal* source = null;
    Value* source_value = null;
    Value* err = tensor_expr_resolve_concrete_cpu(
        args[0],
        &source,
        &source_value,
        interp,
        "ml/softmax: tensor backing storage is missing",
        "ml/softmax: backend kernel is not implemented for non-CPU tensors"
    );
    if (err != null) return err;
    bool cleanup_source = source_value != args[0];
    defer if (cleanup_source) {
        boundary_cleanup_materialized_value(null, source_value, interp.current_scope);
    }

    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "ml/softmax: tensor backing storage is missing");
    if (!ml_reduction_supported_dtype(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/softmax: currently supports Float64 and Float32 tensors", null);
    }

    usz axis = 0;
    err = ml_reduction_parse_axis_value(
        args[1],
        source,
        interp,
        "ml/softmax: axis must be an integer",
        "ml/softmax: axis out of range",
        "ml/softmax: duplicate reduction axis",
        null,
        0,
        &axis
    );
    if (err != null) return err;

    usz[1] axes = { axis };
    usz* result_shape = null;
    usz result_rank = 0;
    usz reduction_count = 0;
    err = ml_reduction_result_shape(
        source,
        axes[..],
        1,
        interp,
        "ml/softmax: result shape is too large",
        "ml/softmax: cannot reduce an empty axis",
        &result_shape,
        &result_rank,
        &reduction_count
    );
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = source.rank == 0 ? scalar_shape[0:0] : source.shape[0:source.rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "ml/softmax: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml/softmax: result tensor backing storage is missing");
    }
    if (out_value.tensor_val.element_count == 0) {
        keep_out = true;
        return out_value;
    }

    usz slice_count = result_rank == 0 ? (usz)1 : out_value.tensor_val.element_count / reduction_count;
    double* maxes = null;
    double* exp_sums = null;
    err = ml_stable_exp_stats_cpu(
        source,
        axes[..],
        1,
        slice_count,
        interp,
        "ml/softmax: tensor index offset is invalid",
        &maxes,
        &exp_sums
    );
    if (err != null) return err;
    defer mem::free(maxes);
    defer mem::free(exp_sums);

    usz bytes = 0;
    if (!tensor_checked_mul_usz(out_value.tensor_val.element_count, double.sizeof, &bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/softmax: output buffer is too large", null);
    }
    double* softmax_values = (double*)mem::malloc(bytes);
    if (softmax_values == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/softmax: failed to allocate output buffer", null);
    }
    defer mem::free(softmax_values);

    for (usz i = 0; i < source.element_count; i++) {
        usz result_index = 0;
        double value = 0.0;
        if (!ml_reduction_result_index(source, i, axes[..], 1, &result_index) ||
            !ml_reduction_read(source, i, &value) ||
            result_index >= slice_count) {
            return tensor_runtime_raise(interp, "ml/softmax: tensor index offset is invalid");
        }
        softmax_values[i] = ml_c_exp(value - maxes[result_index]) / exp_sums[result_index];
    }

    err = ml_reduction_write_result(out_value.tensor_val, softmax_values, interp, "ml/softmax: Float32 result is not representable");
    if (err != null) return err;
    keep_out = true;
    return out_value;
}
