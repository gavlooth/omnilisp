module lisp;

import std::core::mem;
import main;

const int ML_REDUCTION_SUM = 0;
const int ML_REDUCTION_MEAN = 1;
const int ML_REDUCTION_VARIANCE = 2;

fn bool ml_reduction_supported_dtype(TensorDType dtype) @inline {
    return dtype == TENSOR_DTYPE_DOUBLE || dtype == TENSOR_DTYPE_FLOAT32;
}

fn Value* ml_reduction_parse_axis_value(
    Value* axis_value,
    TensorVal* tensor,
    Interp* interp,
    char[] type_label,
    char[] range_label,
    char[] duplicate_label,
    usz* prior_axes,
    usz prior_axis_count,
    usz* out_axis
) {
    if (out_axis == null) return tensor_runtime_raise(interp, "ml reduction: axis parser state is missing");
    *out_axis = 0;
    if (tensor == null) return tensor_runtime_raise(interp, "ml reduction: tensor backing storage is missing");
    if (tensor.rank > (usz)long.max) return raise_error_with_payload_names(interp, "tensor", "tensor/axis-out-of-range", range_label, null);
    if (!is_int(axis_value)) return tensor_type_raise(interp, type_label);

    long rank = (long)tensor.rank;
    long axis = axis_value.int_val;
    if (axis < 0) axis += rank;
    if (axis < 0 || axis >= rank) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/axis-out-of-range", range_label, null);
    }
    usz normalized = (usz)axis;
    if (tensor_axis_is_listed(normalized, prior_axes, prior_axis_count)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/axis-duplicate", duplicate_label, null);
    }
    *out_axis = normalized;
    return null;
}

fn Value* ml_reduction_parse_axes(
    Value* axes_arg,
    TensorVal* tensor,
    Interp* interp,
    char[] type_label,
    char[] range_label,
    char[] duplicate_label,
    char[] empty_label,
    usz** out_axes,
    usz* out_count
) {
    if (out_axes == null || out_count == null) return tensor_runtime_raise(interp, "ml reduction: axis parser state is missing");
    *out_axes = null;
    *out_count = 0;
    if (tensor == null) return tensor_runtime_raise(interp, "ml reduction: tensor backing storage is missing");

    usz count = 0;
    bool single_axis = is_int(axes_arg);
    if (single_axis) {
        count = 1;
    } else if (!tensor_container_count(axes_arg, &count) ||
        (!is_array(axes_arg) && !is_cons(axes_arg) && !is_nil(axes_arg))) {
        return tensor_type_raise(interp, type_label);
    }
    if (count == 0) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", empty_label, null);
    }
    if (count > usz.max / usz.sizeof) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml reduction: axis list is too large", null);
    }

    usz* axes = (usz*)mem::malloc(usz.sizeof * count);
    if (axes == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml reduction: failed to allocate axis list", null);
    }

    for (usz i = 0; i < count; i++) {
        Value* axis_value = single_axis ? axes_arg : tensor_container_nth(axes_arg, i);
        usz normalized = 0;
        Value* err = ml_reduction_parse_axis_value(
            axis_value,
            tensor,
            interp,
            type_label,
            range_label,
            duplicate_label,
            axes,
            i,
            &normalized
        );
        if (err != null) {
            mem::free(axes);
            return err;
        }
        axes[i] = normalized;
    }

    *out_axes = axes;
    *out_count = count;
    return null;
}

fn Value* ml_reduction_result_shape(
    TensorVal* source,
    usz* axes,
    usz axis_count,
    Interp* interp,
    char[] shape_label,
    char[] empty_axis_label,
    usz** out_shape,
    usz* out_rank,
    usz* out_reduction_count
) {
    if (out_shape == null || out_rank == null || out_reduction_count == null) {
        return tensor_runtime_raise(interp, "ml reduction: result shape state is missing");
    }
    *out_shape = null;
    *out_rank = 0;
    *out_reduction_count = 0;
    if (source == null || axes == null || axis_count == 0 || axis_count > source.rank) {
        return tensor_runtime_raise(interp, "ml reduction: tensor metadata is missing");
    }

    usz reduction_count = 1;
    for (usz i = 0; i < axis_count; i++) {
        usz dim = source.shape[axes[i]];
        if (dim == 0) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/shape-mismatch", empty_axis_label, null);
        }
        if (!tensor_checked_mul_usz(reduction_count, dim, &reduction_count)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", shape_label, null);
        }
    }

    usz result_rank = source.rank - axis_count;
    usz* result_shape = null;
    if (result_rank > 0) {
        if (result_rank > usz.max / usz.sizeof) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", shape_label, null);
        }
        result_shape = (usz*)mem::malloc(usz.sizeof * result_rank);
        if (result_shape == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", shape_label, null);
        }
        usz out_axis = 0;
        for (usz axis = 0; axis < source.rank; axis++) {
            if (tensor_axis_is_listed(axis, axes, axis_count)) continue;
            result_shape[out_axis] = source.shape[axis];
            out_axis++;
        }
    }

    *out_shape = result_shape;
    *out_rank = result_rank;
    *out_reduction_count = reduction_count;
    return null;
}

fn bool ml_reduction_result_index(TensorVal* source, usz logical_index, usz* axes, usz axis_count, usz* out_index) {
    if (source == null || out_index == null || logical_index >= source.element_count) return false;
    *out_index = 0;
    if (source.rank == 0) return true;

    usz remaining = logical_index;
    usz result_index = 0;
    usz result_stride = 1;
    for (usz i = source.rank; i > 0; i--) {
        usz axis = i - 1;
        usz dim = source.shape[axis];
        if (dim == 0) return false;
        usz coord = remaining % dim;
        remaining = remaining / dim;
        if (tensor_axis_is_listed(axis, axes, axis_count)) continue;

        usz offset = 0;
        if (!tensor_checked_mul_usz(coord, result_stride, &offset)) return false;
        if (offset > usz.max - result_index) return false;
        result_index += offset;
        if (!tensor_checked_mul_usz(result_stride, dim, &result_stride)) return false;
    }

    *out_index = result_index;
    return true;
}

fn bool ml_reduction_read(TensorVal* source, usz logical_index, double* out_value) {
    if (source == null || out_value == null) return false;
    usz storage_index = 0;
    if (!tensor_storage_offset_for_logical_index(source, logical_index, &storage_index)) return false;
    switch (source.dtype) {
        case TENSOR_DTYPE_DOUBLE:
            *out_value = ((double*)source.data)[storage_index];
            return true;
        case TENSOR_DTYPE_FLOAT32:
            *out_value = (double)((float*)source.data)[storage_index];
            return true;
        default:
            return false;
    }
}

fn Value* ml_reduction_write_result(TensorVal* out, double* values, Interp* interp, char[] value_label) {
    if (out == null || values == null) return tensor_runtime_raise(interp, "ml reduction: result tensor backing storage is missing");
    switch (out.dtype) {
        case TENSOR_DTYPE_DOUBLE: {
            double* out_data = (double*)out.data;
            for (usz i = 0; i < out.element_count; i++) out_data[i] = values[i];
            return null;
        }
        case TENSOR_DTYPE_FLOAT32: {
            float* out_data = (float*)out.data;
            for (usz i = 0; i < out.element_count; i++) {
                double value = values[i];
                if (!conversion_double_is_finite(value) ||
                    value > TENSOR_FLOAT32_FINITE_LIMIT ||
                    value < -TENSOR_FLOAT32_FINITE_LIMIT) {
                    return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", value_label, null);
                }
                out_data[i] = (float)value;
            }
            return null;
        }
        default:
            return tensor_runtime_raise(interp, "ml reduction: result tensor dtype is unsupported");
    }
}

fn Value* ml_reduction_eval_cpu(
    TensorVal* source,
    TensorVal* out,
    usz* axes,
    usz axis_count,
    usz reduction_count,
    int op,
    Interp* interp,
    char[] index_label,
    char[] value_label
) {
    if (source == null || out == null || axes == null) return tensor_runtime_raise(interp, index_label);
    if (out.element_count == 0) return null;

    usz bytes = 0;
    if (!tensor_checked_mul_usz(out.element_count, double.sizeof, &bytes)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml reduction: accumulator is too large", null);
    }
    double* accum = (double*)mem::malloc(bytes);
    if (accum == null) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml reduction: failed to allocate accumulator", null);
    }
    defer mem::free(accum);
    for (usz i = 0; i < out.element_count; i++) accum[i] = 0.0;

    for (usz i = 0; i < source.element_count; i++) {
        usz result_index = 0;
        double value = 0.0;
        if (!ml_reduction_result_index(source, i, axes, axis_count, &result_index) ||
            result_index >= out.element_count ||
            !ml_reduction_read(source, i, &value)) {
            return tensor_runtime_raise(interp, index_label);
        }
        accum[result_index] += value;
    }

    if (op == ML_REDUCTION_VARIANCE) {
        for (usz i = 0; i < out.element_count; i++) accum[i] = accum[i] / (double)reduction_count;

        double* squares = (double*)mem::malloc(bytes);
        if (squares == null) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/out-of-memory", "ml/variance: failed to allocate accumulator", null);
        }
        defer mem::free(squares);
        for (usz i = 0; i < out.element_count; i++) squares[i] = 0.0;

        for (usz i = 0; i < source.element_count; i++) {
            usz result_index = 0;
            double value = 0.0;
            if (!ml_reduction_result_index(source, i, axes, axis_count, &result_index) ||
                result_index >= out.element_count ||
                !ml_reduction_read(source, i, &value)) {
                return tensor_runtime_raise(interp, index_label);
            }
            double diff = value - accum[result_index];
            squares[result_index] += diff * diff;
        }
        for (usz i = 0; i < out.element_count; i++) squares[i] = squares[i] / (double)reduction_count;
        return ml_reduction_write_result(out, squares, interp, value_label);
    }

    if (op == ML_REDUCTION_MEAN) {
        for (usz i = 0; i < out.element_count; i++) accum[i] = accum[i] / (double)reduction_count;
    }
    return ml_reduction_write_result(out, accum, interp, value_label);
}

fn Value* ml_reduction_apply(Value*[] args, Interp* interp, int op, char[] name) {
    if (args.len != 2) return primitive_arity_raise(interp, name, "2 arguments: input axes", args.len);
    if (!is_tensor(args[0])) return tensor_type_raise(interp, "ml reduction: expected Tensor input");

    TensorVal* source = null;
    Value* source_value = null;
    Value* err = tensor_expr_resolve_concrete_cpu(
        args[0],
        &source,
        &source_value,
        interp,
        "ml reduction: tensor backing storage is missing",
        "ml reduction: backend kernel is not implemented for non-CPU tensors"
    );
    if (err != null) return err;
    bool cleanup_source = source_value != args[0];
    defer if (cleanup_source) {
        boundary_cleanup_materialized_value(null, source_value, interp.current_scope);
    }

    if (!tensor_has_valid_storage(source)) return tensor_runtime_raise(interp, "ml reduction: tensor backing storage is missing");
    if (!ml_reduction_supported_dtype(source.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml reduction: currently supports Float64 and Float32 tensors", null);
    }

    char[] type_label = op == ML_REDUCTION_SUM
        ? "ml/sum: axes must be an integer, array, or proper list of integers"
        : op == ML_REDUCTION_MEAN
            ? "ml/mean: axes must be an integer, array, or proper list of integers"
            : "ml/variance: axes must be an integer, array, or proper list of integers";
    char[] range_label = op == ML_REDUCTION_SUM
        ? "ml/sum: axis out of range"
        : op == ML_REDUCTION_MEAN ? "ml/mean: axis out of range" : "ml/variance: axis out of range";
    char[] duplicate_label = op == ML_REDUCTION_SUM
        ? "ml/sum: duplicate reduction axis"
        : op == ML_REDUCTION_MEAN ? "ml/mean: duplicate reduction axis" : "ml/variance: duplicate reduction axis";
    char[] empty_label = op == ML_REDUCTION_SUM
        ? "ml/sum: expected at least one reduction axis"
        : op == ML_REDUCTION_MEAN ? "ml/mean: expected at least one reduction axis" : "ml/variance: expected at least one reduction axis";
    char[] empty_axis_label = op == ML_REDUCTION_SUM
        ? "ml/sum: cannot reduce an empty axis"
        : op == ML_REDUCTION_MEAN ? "ml/mean: cannot reduce an empty axis" : "ml/variance: cannot reduce an empty axis";
    char[] shape_label = op == ML_REDUCTION_SUM
        ? "ml/sum: result shape is too large"
        : op == ML_REDUCTION_MEAN ? "ml/mean: result shape is too large" : "ml/variance: result shape is too large";
    char[] value_label = op == ML_REDUCTION_SUM
        ? "ml/sum: Float32 result is not finite"
        : op == ML_REDUCTION_MEAN ? "ml/mean: Float32 result is not finite" : "ml/variance: Float32 result is not finite";
    char[] index_label = op == ML_REDUCTION_SUM
        ? "ml/sum: tensor index offset is invalid"
        : op == ML_REDUCTION_MEAN ? "ml/mean: tensor index offset is invalid" : "ml/variance: tensor index offset is invalid";

    usz* axes = null;
    usz axis_count = 0;
    err = ml_reduction_parse_axes(args[1], source, interp, type_label, range_label, duplicate_label, empty_label, &axes, &axis_count);
    if (err != null) return err;
    defer mem::free(axes);

    usz* result_shape = null;
    usz result_rank = 0;
    usz reduction_count = 0;
    err = ml_reduction_result_shape(source, axes, axis_count, interp, shape_label, empty_axis_label, &result_shape, &result_rank, &reduction_count);
    if (err != null) return err;
    defer if (result_shape != null) mem::free(result_shape);

    usz[1] scalar_shape = { 0 };
    usz[] shape_slice = result_rank == 0 ? scalar_shape[0:0] : result_shape[0:result_rank];
    Value* out_value = make_tensor_uninitialized_checked(interp, source.dtype, shape_slice, "ml reduction: failed to allocate result tensor");
    if (out_value == null || out_value.tag == ERROR) return out_value;
    bool keep_out = false;
    defer if (!keep_out) {
        boundary_cleanup_materialized_value(null, out_value, interp.current_scope);
    }
    if (out_value.tag != TENSOR || !tensor_has_valid_storage(out_value.tensor_val)) {
        return tensor_runtime_raise(interp, "ml reduction: result tensor backing storage is missing");
    }

    err = ml_reduction_eval_cpu(source, out_value.tensor_val, axes, axis_count, reduction_count, op, interp, index_label, value_label);
    if (err != null) return err;
    keep_out = true;
    return out_value;
}

fn Value* prim_ml_sum(Value*[] args, Env* env, Interp* interp) {
    return ml_reduction_apply(args, interp, ML_REDUCTION_SUM, "ml/sum");
}

fn Value* prim_ml_mean(Value*[] args, Env* env, Interp* interp) {
    return ml_reduction_apply(args, interp, ML_REDUCTION_MEAN, "ml/mean");
}

fn Value* prim_ml_variance(Value*[] args, Env* env, Interp* interp) {
    return ml_reduction_apply(args, interp, ML_REDUCTION_VARIANCE, "ml/variance");
}
