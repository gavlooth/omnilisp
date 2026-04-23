module lisp;

import std::core::mem;
import std::io;

const usz ML_CONFUSION_MATRIX_MAX_CLASSES = 4096;

fn Value* ml_visualization_invalid(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "ml", "ml/invalid-visualization", message, null);
}

fn Value* ml_visualization_backend_unsupported(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "ml", "ml/backend-unsupported", message, null);
}

fn Value* ml_visualization_oom(Interp* interp) @inline {
    return raise_error_with_payload_names(interp, "ml", "ml/out-of-memory", "ml visualization: out of memory", null);
}

fn Value* ml_visualization_make_dict(Interp* interp, uint capacity) @inline {
    return make_hashmap_checked(interp, capacity, "ml visualization: out of memory");
}

fn Value* ml_visualization_dict_set(Value* dict, Interp* interp, char[] key, Value* value) @inline {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return ml_visualization_oom(interp);
    if (value != null && value.tag == ERROR) return value;
    SymbolId key_sym = interp.symbols.intern(key);
    if (key_sym == INVALID_SYMBOL_ID) return ml_visualization_oom(interp);
    if (!hashmap_set_symbol_checked(dict.hashmap_val, key_sym, value, interp)) return ml_visualization_oom(interp);
    return null;
}

fn Value* ml_visualization_dict_set_symbol(Value* dict, Interp* interp, char[] key, char[] symbol_name) @inline {
    SymbolId value_sym = interp.symbols.intern(symbol_name);
    if (value_sym == INVALID_SYMBOL_ID) return ml_visualization_oom(interp);
    return ml_visualization_dict_set(dict, interp, key, make_symbol(interp, value_sym));
}

struct MlVisualizationSeries {
    Value* y;
    Value* x;
    usz count;
}

struct MlConfusionInput {
    bool is_tensor;
    TensorVal* tensor;
    Value* materialized;
    Value* container;
    usz count;
}

enum MlImageRangeMode {
    ML_IMAGE_RANGE_AUTO,
    ML_IMAGE_RANGE_UNIT,
    ML_IMAGE_RANGE_BYTE,
}

struct MlImageExportSpec {
    TensorVal* tensor;
    Value* materialized;
    char[] path;
    usz width;
    usz height;
    usz channels;
    MlImageRangeMode range;
}

fn Value* ml_visualization_push_double(Value* array, Interp* interp, double value) {
    if (array == null || array.tag != ARRAY || array.array_val == null) return ml_visualization_oom(interp);
    Value* item = make_double(interp, value);
    Value* promoted = array_promote_or_error(item, interp);
    if (promoted == null || promoted.tag == ERROR) return promoted;
    array.array_val.items[array.array_val.length] = promoted;
    array.array_val.length++;
    return null;
}

fn bool ml_visualization_read_finite_number(Value* value, double* out) {
    if (!try_numeric_to_double(value, out)) return false;
    return conversion_double_is_finite(*out);
}

fn bool ml_visualization_is_pair(Value* value) {
    usz count = 0;
    return (is_array(value) || is_cons(value)) &&
        tensor_container_count(value, &count) &&
        count == 2;
}

fn Value* ml_visualization_series_from_container(
    Value* input,
    Interp* interp,
    bool allow_pairs,
    char[] primitive_name,
    MlVisualizationSeries* out
) {
    if (out == null) return ml_visualization_invalid(interp, "ml visualization: missing output series state");
    out.y = null;
    out.x = null;
    out.count = 0;

    usz count = 0;
    if (!tensor_container_count(input, &count) || (!is_array(input) && !is_cons(input) && !is_nil(input))) {
        return ml_visualization_invalid(interp, "ml visualization: expected Array, List, or Tensor numeric series");
    }
    if (count == 0) return ml_visualization_invalid(interp, "ml visualization: graph series must contain at least one sample");

    Value* first = tensor_container_nth(input, 0);
    bool pair_series = ml_visualization_is_pair(first);
    if (pair_series && !allow_pairs) {
        return ml_visualization_invalid(interp, "ml/loss-curve: expected scalar loss values, not x/y pairs");
    }

    out.y = make_array_checked(interp, count, "ml visualization: failed to allocate y series");
    if (out.y == null || out.y.tag == ERROR) return out.y;
    if (pair_series) {
        out.x = make_array_checked(interp, count, "ml visualization: failed to allocate x series");
        if (out.x == null || out.x.tag == ERROR) return out.x;
    }

    for (usz i = 0; i < count; i++) {
        Value* item = tensor_container_nth(input, i);
        if (pair_series) {
            if (!ml_visualization_is_pair(item)) return ml_visualization_invalid(interp, "ml/plot: all samples must be numeric x/y pairs");
            double x = 0.0;
            double y = 0.0;
            if (!ml_visualization_read_finite_number(tensor_container_nth(item, 0), &x) ||
                !ml_visualization_read_finite_number(tensor_container_nth(item, 1), &y)) {
                return ml_visualization_invalid(interp, "ml/plot: x/y samples must be finite real numbers");
            }
            if (Value* err = ml_visualization_push_double(out.x, interp, x)) return err;
            if (Value* err = ml_visualization_push_double(out.y, interp, y)) return err;
        } else {
            double y = 0.0;
            if (ml_visualization_is_pair(item)) return ml_visualization_invalid(interp, "ml visualization: cannot mix scalar samples and x/y pairs");
            if (!ml_visualization_read_finite_number(item, &y)) {
                return ml_visualization_invalid(interp, "ml visualization: samples must be finite real numbers");
            }
            if (Value* err = ml_visualization_push_double(out.y, interp, y)) return err;
        }
    }

    out.count = count;
    return null;
}

fn Value* ml_visualization_series_from_tensor(
    Value* input,
    Interp* interp,
    bool allow_pairs,
    char[] primitive_name,
    MlVisualizationSeries* out
) {
    if (out == null) return ml_visualization_invalid(interp, "ml visualization: missing output series state");
    out.y = null;
    out.x = null;
    out.count = 0;

    TensorVal* tensor = null;
    Value* materialized = null;
    Value* err = tensor_expr_resolve_concrete_cpu(
        input,
        &tensor,
        &materialized,
        interp,
        "ml visualization: tensor backing storage is missing",
        "ml visualization: currently supports concrete CPU tensor data"
    );
    if (err != null) return err;
    defer if (materialized != null && materialized != input) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);

    if (!ml_reduction_supported_dtype(tensor.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml visualization: only Float64 and Float32 tensor series are supported", null);
    }
    if (!tensor_dense_row_major(tensor)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml visualization: currently supports dense row-major tensors", null);
    }
    if (tensor.element_count == 0) return ml_visualization_invalid(interp, "ml visualization: graph series must contain at least one sample");

    bool pair_series = false;
    usz count = 0;
    if (tensor.rank == 1) {
        count = tensor.shape[0];
    } else if (tensor.rank == 2 && allow_pairs && tensor.shape[1] == 2) {
        pair_series = true;
        count = tensor.shape[0];
    } else if (allow_pairs) {
        return ml_visualization_invalid(interp, "ml/plot: expected rank-1 tensor or rank-2 [N 2] tensor");
    } else {
        return ml_visualization_invalid(interp, "ml/loss-curve: expected rank-1 tensor of loss values");
    }
    if (count == 0) return ml_visualization_invalid(interp, "ml visualization: graph series must contain at least one sample");

    out.y = make_array_checked(interp, count, "ml visualization: failed to allocate y series");
    if (out.y == null || out.y.tag == ERROR) return out.y;
    if (pair_series) {
        out.x = make_array_checked(interp, count, "ml visualization: failed to allocate x series");
        if (out.x == null || out.x.tag == ERROR) return out.x;
    }

    for (usz i = 0; i < count; i++) {
        if (pair_series) {
            double x = 0.0;
            double y = 0.0;
            if (!ml_reduction_read(tensor, i * (usz)2, &x) ||
                !ml_reduction_read(tensor, i * (usz)2 + (usz)1, &y) ||
                !conversion_double_is_finite(x) ||
                !conversion_double_is_finite(y)) {
                return ml_visualization_invalid(interp, "ml/plot: tensor x/y samples must be finite real numbers");
            }
            if (Value* push_err = ml_visualization_push_double(out.x, interp, x)) return push_err;
            if (Value* push_err = ml_visualization_push_double(out.y, interp, y)) return push_err;
        } else {
            double y = 0.0;
            if (!ml_reduction_read(tensor, i, &y) || !conversion_double_is_finite(y)) {
                return ml_visualization_invalid(interp, "ml visualization: tensor samples must be finite real numbers");
            }
            if (Value* push_err = ml_visualization_push_double(out.y, interp, y)) return push_err;
        }
    }

    out.count = count;
    return null;
}

fn Value* ml_visualization_series_from_value(
    Value* input,
    Interp* interp,
    bool allow_pairs,
    char[] primitive_name,
    MlVisualizationSeries* out
) {
    if (is_tensor(input)) return ml_visualization_series_from_tensor(input, interp, allow_pairs, primitive_name, out);
    return ml_visualization_series_from_container(input, interp, allow_pairs, primitive_name, out);
}

fn Value* ml_visualization_apply_options(Value* props, Value* options, Interp* interp, char[] primitive_name, char[] default_title) {
    if (props == null || props.tag != HASHMAP || props.hashmap_val == null) return ml_visualization_oom(interp);
    if (options != null && options.tag != HASHMAP) {
        return primitive_arg_type_raise(interp, primitive_name, 1, "Dictionary options", options);
    }

    if (Value* err = ml_visualization_dict_set_symbol(props, interp, "backend", "ftxui")) return err;
    Value* title = options != null ? nn_dict_get(options, interp, "title") : null;
    if (title == null) title = make_string(interp, default_title);
    if (title == null || title.tag == ERROR) return title;
    if (title.tag != STRING) return ml_visualization_invalid(interp, "ml visualization: title option must be a String");
    if (Value* err = ml_visualization_dict_set(props, interp, "title", title)) return err;

    Value* backend = options != null ? nn_dict_get(options, interp, "backend") : null;
    if (backend != null && !nn_symbol_is(backend, interp, "ftxui")) {
        return ml_visualization_backend_unsupported(interp, "ml visualization: only the 'ftxui backend is supported");
    }

    Value* width = options != null ? nn_dict_get(options, interp, "width") : null;
    if (width != null) {
        if (width.tag != INT || width.int_val <= 0) return ml_visualization_invalid(interp, "ml visualization: width option must be a positive Integer");
        if (Value* err = ml_visualization_dict_set(props, interp, "width", width)) return err;
    }

    Value* height = options != null ? nn_dict_get(options, interp, "height") : null;
    if (height != null) {
        if (height.tag != INT || height.int_val <= 0) return ml_visualization_invalid(interp, "ml visualization: height option must be a positive Integer");
        if (Value* err = ml_visualization_dict_set(props, interp, "height", height)) return err;
    }

    return null;
}

fn Value* ml_visualization_make_graph_node(
    MlVisualizationSeries* series,
    Value* options,
    Interp* interp,
    char[] primitive_name,
    char[] visualization_name,
    char[] default_title
) {
    if (series == null || series.y == null || series.count == 0) {
        return ml_visualization_invalid(interp, "ml visualization: graph series must contain at least one sample");
    }

    Value* props = ml_visualization_make_dict(interp, 12);
    if (props == null || props.tag == ERROR) return props;
    if (Value* err = ml_visualization_dict_set(props, interp, "series", series.y)) return err;
    if (series.x != null) {
        if (Value* err = ml_visualization_dict_set(props, interp, "x", series.x)) return err;
    }
    if (Value* err = ml_visualization_dict_set_symbol(props, interp, "visualization", visualization_name)) return err;
    if (Value* err = ml_visualization_apply_options(props, options, interp, primitive_name, default_title)) return err;

    Value* node = ml_visualization_make_dict(interp, 8);
    if (node == null || node.tag == ERROR) return node;
    if (Value* err = ml_visualization_dict_set_symbol(node, interp, "kind", "graph")) return err;
    if (Value* err = ml_visualization_dict_set(node, interp, "props", props)) return err;
    if (Value* err = ml_visualization_dict_set(node, interp, "children", make_nil(interp))) return err;
    return node;
}

fn Value* ml_visualization_stats_unavailable(Interp* interp, char[] reason) {
    Value* stats = ml_visualization_make_dict(interp, 8);
    if (stats == null || stats.tag == ERROR) return stats;
    if (Value* err = ml_visualization_dict_set_symbol(stats, interp, "status", "unavailable")) return err;
    if (Value* err = ml_visualization_dict_set_symbol(stats, interp, "reason", reason)) return err;
    if (Value* err = ml_visualization_dict_set(stats, interp, "finite", make_symbol(interp, interp.sym_false))) return err;
    return stats;
}

fn Value* ml_visualization_tensor_stats(TensorVal* tensor, Interp* interp) {
    if (tensor == null) return ml_visualization_stats_unavailable(interp, "missing-storage");
    if (tensor.element_count == 0) {
        Value* stats = ml_visualization_make_dict(interp, 8);
        if (stats == null || stats.tag == ERROR) return stats;
        if (Value* err = ml_visualization_dict_set_symbol(stats, interp, "status", "empty")) return err;
        if (Value* err = ml_visualization_dict_set(stats, interp, "count", make_int(interp, 0))) return err;
        if (Value* err = ml_visualization_dict_set(stats, interp, "finite", make_symbol(interp, interp.sym_true))) return err;
        return stats;
    }
    if (tensor.device != TENSOR_DEVICE_CPU) return ml_visualization_stats_unavailable(interp, "non-cpu-device");
    if (tensor.kind != TENSOR_PAYLOAD_CONCRETE) return ml_visualization_stats_unavailable(interp, "non-concrete-payload");
    if (!ml_reduction_supported_dtype(tensor.dtype)) return ml_visualization_stats_unavailable(interp, "unsupported-dtype");
    if (!tensor_has_valid_storage(tensor)) return ml_visualization_stats_unavailable(interp, "missing-storage");

    double first = 0.0;
    if (!ml_reduction_read(tensor, 0, &first) || !conversion_double_is_finite(first)) {
        Value* stats = ml_visualization_make_dict(interp, 8);
        if (stats == null || stats.tag == ERROR) return stats;
        if (Value* err = ml_visualization_dict_set_symbol(stats, interp, "status", "non-finite")) return err;
        if (Value* err = ml_visualization_dict_set(stats, interp, "finite", make_symbol(interp, interp.sym_false))) return err;
        return stats;
    }

    double min_value = first;
    double max_value = first;
    double sum = first;
    for (usz i = 1; i < tensor.element_count; i++) {
        double value = 0.0;
        if (!ml_reduction_read(tensor, i, &value) || !conversion_double_is_finite(value)) {
            Value* stats = ml_visualization_make_dict(interp, 8);
            if (stats == null || stats.tag == ERROR) return stats;
            if (Value* err = ml_visualization_dict_set_symbol(stats, interp, "status", "non-finite")) return err;
            if (Value* err = ml_visualization_dict_set(stats, interp, "finite", make_symbol(interp, interp.sym_false))) return err;
            return stats;
        }
        if (value < min_value) min_value = value;
        if (value > max_value) max_value = value;
        sum += value;
        if (!conversion_double_is_finite(sum)) return ml_visualization_stats_unavailable(interp, "numeric-overflow");
    }

    Value* stats = ml_visualization_make_dict(interp, 12);
    if (stats == null || stats.tag == ERROR) return stats;
    if (Value* err = ml_visualization_dict_set_symbol(stats, interp, "status", "available")) return err;
    if (Value* err = ml_visualization_dict_set(stats, interp, "finite", make_symbol(interp, interp.sym_true))) return err;
    if (Value* err = ml_visualization_dict_set(stats, interp, "count", tensor_usz_metadata_value(interp, tensor.element_count, "ml/tensor-summary: tensor element count is too large"))) return err;
    if (Value* err = ml_visualization_dict_set(stats, interp, "min", make_double(interp, min_value))) return err;
    if (Value* err = ml_visualization_dict_set(stats, interp, "max", make_double(interp, max_value))) return err;
    if (Value* err = ml_visualization_dict_set(stats, interp, "mean", make_double(interp, sum / (double)tensor.element_count))) return err;
    return stats;
}

fn Value* ml_visualization_tensor_summary(Value* tensor_value, Value* options, Interp* interp) {
    if (!is_tensor(tensor_value)) return primitive_arg_type_raise(interp, "ml/tensor-summary", 0, "Tensor", tensor_value);
    if (options != null && options.tag != HASHMAP) return primitive_arg_type_raise(interp, "ml/tensor-summary", 1, "Dictionary options", options);

    TensorVal* tensor = tensor_value.tensor_val;
    if (tensor == null || !tensor_has_valid_metadata(tensor)) {
        return raise_error_with_payload_names(interp, "runtime", "runtime/invalid-state", "ml/tensor-summary: tensor metadata is invalid", null);
    }

    Value* summary = ml_visualization_make_dict(interp, 24);
    if (summary == null || summary.tag == ERROR) return summary;
    if (Value* err = ml_visualization_dict_set_symbol(summary, interp, "kind", "tensor-summary")) return err;
    if (Value* err = ml_visualization_dict_set(summary, interp, "dtype", make_symbol(interp, tensor_dtype_symbol(tensor.dtype, interp)))) return err;
    if (Value* err = ml_visualization_dict_set(summary, interp, "device", make_symbol(interp, tensor_device_symbol(tensor.device, interp)))) return err;
    if (Value* err = ml_visualization_dict_set(summary, interp, "payload", make_symbol(interp, tensor_payload_kind_symbol(tensor.kind, interp)))) return err;
    if (Value* err = ml_visualization_dict_set_symbol(summary, interp, "layout", tensor_dense_row_major(tensor) ? "dense-row-major" : "strided")) return err;
    if (Value* err = ml_visualization_dict_set(summary, interp, "dense-row-major", make_symbol(interp, tensor_dense_row_major(tensor) ? interp.sym_true : interp.sym_false))) return err;
    if (Value* err = ml_visualization_dict_set(summary, interp, "shape", tensor_metadata_usz_array(interp, tensor.shape, tensor.rank, "ml/tensor-summary: failed to allocate shape array", "ml/tensor-summary: tensor dimension is too large"))) return err;
    if (Value* err = ml_visualization_dict_set(summary, interp, "strides", tensor_metadata_usz_array(interp, tensor.strides, tensor.rank, "ml/tensor-summary: failed to allocate strides array", "ml/tensor-summary: tensor stride is too large"))) return err;
    if (Value* err = ml_visualization_dict_set(summary, interp, "rank", tensor_usz_metadata_value(interp, tensor.rank, "ml/tensor-summary: tensor rank is too large"))) return err;
    if (Value* err = ml_visualization_dict_set(summary, interp, "element-count", tensor_usz_metadata_value(interp, tensor.element_count, "ml/tensor-summary: tensor element count is too large"))) return err;
    if (Value* err = ml_visualization_dict_set(summary, interp, "byte-length", tensor_usz_metadata_value(interp, tensor.byte_len, "ml/tensor-summary: tensor byte length is too large"))) return err;
    if (Value* err = ml_visualization_dict_set(summary, interp, "stats", ml_visualization_tensor_stats(tensor, interp))) return err;
    if (Value* err = ml_visualization_apply_options(summary, options, interp, "ml/tensor-summary", "Tensor Summary")) return err;
    return summary;
}

fn bool ml_confusion_class_id_from_double(double value, usz* out) {
    if (out == null || !conversion_double_is_finite(value)) return false;
    if (value < 0.0 || value >= (double)ML_CONFUSION_MATRIX_MAX_CLASSES) return false;
    long id = (long)value;
    if (value != (double)id) return false;
    *out = (usz)id;
    return true;
}

fn bool ml_confusion_class_id_from_value(Value* value, usz* out) {
    double numeric = 0.0;
    if (!try_numeric_to_double(value, &numeric)) return false;
    return ml_confusion_class_id_from_double(numeric, out);
}

fn Value* ml_confusion_prepare_input(Value* input, Interp* interp, char[] role, MlConfusionInput* out) {
    if (out == null) return ml_visualization_invalid(interp, "ml/confusion-matrix: missing input state");
    out.is_tensor = false;
    out.tensor = null;
    out.materialized = null;
    out.container = null;
    out.count = 0;

    if (is_tensor(input)) {
        TensorVal* tensor = null;
        Value* materialized = null;
        Value* err = tensor_expr_resolve_concrete_cpu(
            input,
            &tensor,
            &materialized,
            interp,
            "ml/confusion-matrix: tensor backing storage is missing",
            "ml/confusion-matrix: currently supports concrete CPU tensor labels"
        );
        if (err != null) return err;
        bool transfer_materialized = false;
        defer if (!transfer_materialized && materialized != null && materialized != input) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);
        if (tensor.rank != 1) return ml_visualization_invalid(interp, "ml/confusion-matrix: prediction and target tensors must be rank-1");
        if (!ml_reduction_supported_dtype(tensor.dtype)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/confusion-matrix: Tensor labels must be Float64 or Float32 class-id tensors", null);
        }
        if (!tensor_dense_row_major(tensor)) {
            return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/confusion-matrix: currently supports dense row-major tensor labels", null);
        }
        if (tensor.element_count == 0) return ml_visualization_invalid(interp, "ml/confusion-matrix: labels must not be empty");
        out.is_tensor = true;
        out.tensor = tensor;
        out.materialized = materialized;
        out.count = tensor.element_count;
        transfer_materialized = true;
        return null;
    }

    usz count = 0;
    if (!tensor_container_count(input, &count) || (!is_array(input) && !is_cons(input) && !is_nil(input))) {
        return ml_visualization_invalid(interp, "ml/confusion-matrix: predictions and targets must be rank-1 tensors, Arrays, or Lists");
    }
    if (count == 0) return ml_visualization_invalid(interp, "ml/confusion-matrix: labels must not be empty");
    out.container = input;
    out.count = count;
    return null;
}

fn Value* ml_confusion_read_label(MlConfusionInput* input, usz index, Interp* interp, usz* out) {
    if (input == null || out == null || index >= input.count) return ml_visualization_invalid(interp, "ml/confusion-matrix: label index is invalid");
    if (input.is_tensor) {
        double numeric = 0.0;
        if (!ml_reduction_read(input.tensor, index, &numeric) || !ml_confusion_class_id_from_double(numeric, out)) {
            return ml_visualization_invalid(interp, "ml/confusion-matrix: tensor labels must be non-negative integer class ids");
        }
        return null;
    }

    Value* value = tensor_container_nth(input.container, index);
    if (!ml_confusion_class_id_from_value(value, out)) {
        return ml_visualization_invalid(interp, "ml/confusion-matrix: labels must be non-negative integer class ids");
    }
    return null;
}

fn Value* ml_confusion_make_labels(Interp* interp, usz class_count) {
    Value* labels = make_array_checked(interp, class_count, "ml/confusion-matrix: failed to allocate labels");
    if (labels == null || labels.tag == ERROR) return labels;
    for (usz i = 0; i < class_count; i++) {
        Value* label = tensor_usz_metadata_value(interp, i, "ml/confusion-matrix: class id is too large");
        if (label == null || label.tag == ERROR) return label;
        labels.array_val.items[i] = label;
        labels.array_val.length++;
    }
    return labels;
}

fn Value* ml_confusion_make_matrix(Interp* interp, long* counts, usz class_count) {
    if (counts == null) return ml_visualization_oom(interp);
    Value* rows = make_array_checked(interp, class_count, "ml/confusion-matrix: failed to allocate matrix");
    if (rows == null || rows.tag == ERROR) return rows;
    for (usz row = 0; row < class_count; row++) {
        Value* row_values = make_array_checked(interp, class_count, "ml/confusion-matrix: failed to allocate matrix row");
        if (row_values == null || row_values.tag == ERROR) return row_values;
        for (usz col = 0; col < class_count; col++) {
            row_values.array_val.items[col] = make_int(interp, counts[row * class_count + col]);
            row_values.array_val.length++;
        }
        rows.array_val.items[row] = row_values;
        rows.array_val.length++;
    }
    return rows;
}

fn Value* ml_confusion_build(Value* predictions, Value* targets, Value* options, Interp* interp) {
    if (options != null && options.tag != HASHMAP) return primitive_arg_type_raise(interp, "ml/confusion-matrix", 2, "Dictionary options", options);

    MlConfusionInput pred;
    Value* err = ml_confusion_prepare_input(predictions, interp, "predictions", &pred);
    if (err != null) return err;
    defer if (pred.materialized != null && pred.materialized != predictions) boundary_cleanup_materialized_value(null, pred.materialized, interp.current_scope);

    MlConfusionInput target;
    err = ml_confusion_prepare_input(targets, interp, "targets", &target);
    if (err != null) return err;
    defer if (target.materialized != null && target.materialized != targets) boundary_cleanup_materialized_value(null, target.materialized, interp.current_scope);

    if (pred.count != target.count) return ml_visualization_invalid(interp, "ml/confusion-matrix: predictions and targets must have equal length");

    usz max_class = 0;
    for (usz i = 0; i < pred.count; i++) {
        usz p = 0;
        usz t = 0;
        if (Value* read_err = ml_confusion_read_label(&pred, i, interp, &p)) return read_err;
        if (Value* read_err = ml_confusion_read_label(&target, i, interp, &t)) return read_err;
        if (p > max_class) max_class = p;
        if (t > max_class) max_class = t;
    }

    usz class_count = max_class + 1;
    if (class_count == 0 || class_count > ML_CONFUSION_MATRIX_MAX_CLASSES) {
        return ml_visualization_invalid(interp, "ml/confusion-matrix: class id is too large");
    }
    if (class_count > usz.max / class_count) return ml_visualization_oom(interp);
    usz cell_count = class_count * class_count;
    if (cell_count > usz.max / long.sizeof) return ml_visualization_oom(interp);
    long* counts = (long*)mem::malloc(cell_count * long.sizeof);
    if (counts == null) return ml_visualization_oom(interp);
    defer mem::free(counts);
    for (usz i = 0; i < cell_count; i++) counts[i] = 0;

    long correct = 0;
    for (usz i = 0; i < pred.count; i++) {
        usz p = 0;
        usz t = 0;
        if (Value* read_err = ml_confusion_read_label(&pred, i, interp, &p)) return read_err;
        if (Value* read_err = ml_confusion_read_label(&target, i, interp, &t)) return read_err;
        counts[t * class_count + p]++;
        if (p == t) correct++;
    }

    Value* out = ml_visualization_make_dict(interp, 20);
    if (out == null || out.tag == ERROR) return out;
    if (Value* set_err = ml_visualization_dict_set_symbol(out, interp, "kind", "confusion-matrix")) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "labels", ml_confusion_make_labels(interp, class_count))) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "matrix", ml_confusion_make_matrix(interp, counts, class_count))) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "total", tensor_usz_metadata_value(interp, pred.count, "ml/confusion-matrix: label count is too large"))) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "correct", make_int(interp, correct))) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "accuracy", make_double(interp, (double)correct / (double)pred.count))) return set_err;
    if (Value* set_err = ml_visualization_dict_set_symbol(out, interp, "orientation", "target-rows-predicted-columns")) return set_err;
    if (Value* set_err = ml_visualization_apply_options(out, options, interp, "ml/confusion-matrix", "Confusion Matrix")) return set_err;
    return out;
}

fn bool ml_image_range_mode_from_options(Value* options, Interp* interp, MlImageRangeMode* out) {
    if (out == null) return false;
    *out = ML_IMAGE_RANGE_AUTO;
    Value* range = options != null ? nn_dict_get(options, interp, "range") : null;
    if (range == null) return true;
    if (nn_symbol_is(range, interp, "auto")) {
        *out = ML_IMAGE_RANGE_AUTO;
        return true;
    }
    if (nn_symbol_is(range, interp, "unit")) {
        *out = ML_IMAGE_RANGE_UNIT;
        return true;
    }
    if (nn_symbol_is(range, interp, "byte")) {
        *out = ML_IMAGE_RANGE_BYTE;
        return true;
    }
    return false;
}

fn bool ml_image_format_supported(Value* options, Interp* interp) {
    Value* format = options != null ? nn_dict_get(options, interp, "format") : null;
    if (format == null) return true;
    return nn_symbol_is(format, interp, "ppm");
}

fn bool ml_image_backend_supported(Value* options, Interp* interp) {
    Value* backend = options != null ? nn_dict_get(options, interp, "backend") : null;
    if (backend == null) return true;
    return nn_symbol_is(backend, interp, "image") || nn_symbol_is(backend, interp, "ppm");
}

fn Value* ml_image_prepare(Value* tensor_value, Value* path_value, Value* options, Interp* interp, MlImageExportSpec* out) {
    if (out == null) return ml_visualization_invalid(interp, "ml/export-image: missing export state");
    out.tensor = null;
    out.materialized = null;
    out.path = "";
    out.width = 0;
    out.height = 0;
    out.channels = 0;
    out.range = ML_IMAGE_RANGE_AUTO;

    if (!is_tensor(tensor_value)) return primitive_arg_type_raise(interp, "ml/export-image", 0, "Tensor", tensor_value);
    if (!is_string(path_value)) return primitive_arg_type_raise(interp, "ml/export-image", 1, "String path", path_value);
    if (options != null && options.tag != HASHMAP) return primitive_arg_type_raise(interp, "ml/export-image", 2, "Dictionary options", options);
    if (path_value.str_len == 0) return ml_visualization_invalid(interp, "ml/export-image: path must not be empty");
    if (!ml_image_format_supported(options, interp)) return ml_visualization_backend_unsupported(interp, "ml/export-image: only PPM export is supported");
    if (!ml_image_backend_supported(options, interp)) return ml_visualization_backend_unsupported(interp, "ml/export-image: only the image/ppm export backend is supported");
    if (!ml_image_range_mode_from_options(options, interp, &out.range)) {
        return ml_visualization_invalid(interp, "ml/export-image: range option must be 'auto, 'unit, or 'byte");
    }

    TensorVal* tensor = null;
    Value* materialized = null;
    Value* err = tensor_expr_resolve_concrete_cpu(
        tensor_value,
        &tensor,
        &materialized,
        interp,
        "ml/export-image: tensor backing storage is missing",
        "ml/export-image: currently supports concrete CPU tensor image data"
    );
    if (err != null) return err;
    bool transfer_materialized = false;
    defer if (!transfer_materialized && materialized != null && materialized != tensor_value) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);

    if (!ml_reduction_supported_dtype(tensor.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/export-image: only Float64 and Float32 tensor images are supported", null);
    }
    if (tensor.rank != 2 && tensor.rank != 3) {
        return ml_visualization_invalid(interp, "ml/export-image: expected rank-2 grayscale or rank-3 HWC tensor");
    }
    if (tensor.shape == null || tensor.shape[0] == 0 || tensor.shape[1] == 0) {
        return ml_visualization_invalid(interp, "ml/export-image: image height and width must be positive");
    }

    usz channels = tensor.rank == 2 ? (usz)1 : tensor.shape[2];
    if (channels != (usz)1 && channels != (usz)3) {
        return ml_visualization_invalid(interp, "ml/export-image: rank-3 image tensors must have 1 or 3 channels");
    }

    out.tensor = tensor;
    out.materialized = materialized;
    out.height = tensor.shape[0];
    out.width = tensor.shape[1];
    out.channels = channels;
    out.path = path_value.str_chars[:path_value.str_len];
    transfer_materialized = true;
    return null;
}

fn Value* ml_image_resolve_range(MlImageExportSpec* spec, Interp* interp, MlImageRangeMode* out_mode) {
    if (spec == null || spec.tensor == null || out_mode == null) return ml_visualization_invalid(interp, "ml/export-image: missing tensor image state");
    *out_mode = spec.range;
    double max_value = 0.0;
    for (usz i = 0; i < spec.tensor.element_count; i++) {
        double value = 0.0;
        if (!ml_reduction_read(spec.tensor, i, &value) || !conversion_double_is_finite(value)) {
            return ml_visualization_invalid(interp, "ml/export-image: image values must be finite");
        }
        if (value < 0.0) return ml_visualization_invalid(interp, "ml/export-image: image values must be non-negative");
        if (value > max_value) max_value = value;
    }
    switch (spec.range) {
        case ML_IMAGE_RANGE_UNIT:
            if (max_value > 1.0) return ml_visualization_invalid(interp, "ml/export-image: unit-range image values must be between 0 and 1");
            return null;
        case ML_IMAGE_RANGE_BYTE:
            if (max_value > 255.0) return ml_visualization_invalid(interp, "ml/export-image: byte-range image values must be between 0 and 255");
            return null;
        case ML_IMAGE_RANGE_AUTO:
        default:
            if (max_value <= 1.0) {
                *out_mode = ML_IMAGE_RANGE_UNIT;
                return null;
            }
            if (max_value <= 255.0) {
                *out_mode = ML_IMAGE_RANGE_BYTE;
                return null;
            }
            return ml_visualization_invalid(interp, "ml/export-image: auto-range image values must fit unit or byte range");
    }
}

fn int ml_image_value_to_byte(double value, MlImageRangeMode mode) {
    double scaled = mode == ML_IMAGE_RANGE_UNIT ? value * 255.0 : value;
    if (scaled < 0.0) scaled = 0.0;
    if (scaled > 255.0) scaled = 255.0;
    return (int)(scaled + 0.5);
}

fn bool ml_image_pixel_channel(MlImageExportSpec* spec, usz row, usz col, usz channel, double* out_value) {
    if (spec == null || spec.tensor == null || out_value == null) return false;
    usz logical = 0;
    if (spec.tensor.rank == 2) {
        if (!tensor_checked_mul_usz(row, spec.width, &logical)) return false;
        if (logical > usz.max - col) return false;
        logical += col;
    } else {
        usz pixel = 0;
        if (!tensor_checked_mul_usz(row, spec.width, &pixel)) return false;
        if (pixel > usz.max - col) return false;
        pixel += col;
        if (!tensor_checked_mul_usz(pixel, spec.channels, &logical)) return false;
        if (logical > usz.max - channel) return false;
        logical += channel;
    }
    return ml_reduction_read(spec.tensor, logical, out_value);
}

fn Value* ml_image_export_ppm(MlImageExportSpec* spec, Interp* interp, MlImageRangeMode resolved_range) {
    if (spec == null || spec.tensor == null) return ml_visualization_invalid(interp, "ml/export-image: missing tensor image state");
    if (try file = io::file::open((String)spec.path, "w")) {
        bool close_needed = true;
        defer if (close_needed) (void)file.close();
        char[128] line;
        if (try char[] header = io::bprintf(&line, "P3\n%d %d\n255\n", (long)spec.width, (long)spec.height)) {
            char[] remaining = header;
            while (remaining.len > 0) {
                usz? written_opt = file.write(remaining);
                if (catch err = written_opt) return raise_error_with_payload_names(interp, "io", "io/write-failed", "ml/export-image: failed to write image file", null);
                if (written_opt == 0) return raise_error_with_payload_names(interp, "io", "io/write-failed", "ml/export-image: failed to write image file", null);
                remaining = remaining[written_opt..];
            }
        } else {
            return ml_visualization_oom(interp);
        }
        for (usz row = 0; row < spec.height; row++) {
            for (usz col = 0; col < spec.width; col++) {
                double c0 = 0.0;
                double c1 = 0.0;
                double c2 = 0.0;
                if (!ml_image_pixel_channel(spec, row, col, 0, &c0)) return ml_visualization_invalid(interp, "ml/export-image: image tensor index is invalid");
                if (spec.channels == 1) {
                    c1 = c0;
                    c2 = c0;
                } else {
                    if (!ml_image_pixel_channel(spec, row, col, 1, &c1) ||
                        !ml_image_pixel_channel(spec, row, col, 2, &c2)) {
                        return ml_visualization_invalid(interp, "ml/export-image: image tensor index is invalid");
                    }
                }
                int r = ml_image_value_to_byte(c0, resolved_range);
                int g = ml_image_value_to_byte(c1, resolved_range);
                int b = ml_image_value_to_byte(c2, resolved_range);
                if (try char[] pixel = io::bprintf(&line, "%d %d %d\n", r, g, b)) {
                    char[] remaining = pixel;
                    while (remaining.len > 0) {
                        usz? written_opt = file.write(remaining);
                        if (catch err = written_opt) return raise_error_with_payload_names(interp, "io", "io/write-failed", "ml/export-image: failed to write image file", null);
                        if (written_opt == 0) return raise_error_with_payload_names(interp, "io", "io/write-failed", "ml/export-image: failed to write image file", null);
                        remaining = remaining[written_opt..];
                    }
                } else {
                    return ml_visualization_oom(interp);
                }
            }
        }
        if (catch err = file.close()) {
            close_needed = false;
            return raise_error_with_payload_names(interp, "io", "io/write-failed", "ml/export-image: failed to close image file", null);
        }
        close_needed = false;
        return null;
    }
    return raise_error_with_payload_names(interp, "io", "io/write-failed", "ml/export-image: failed to open image file for writing", null);
}

fn Value* ml_image_export(Value* tensor_value, Value* path_value, Value* options, Interp* interp) {
    MlImageExportSpec spec;
    Value* err = ml_image_prepare(tensor_value, path_value, options, interp, &spec);
    if (err != null) return err;
    defer if (spec.materialized != null && spec.materialized != tensor_value) boundary_cleanup_materialized_value(null, spec.materialized, interp.current_scope);

    MlImageRangeMode resolved_range = ML_IMAGE_RANGE_AUTO;
    err = ml_image_resolve_range(&spec, interp, &resolved_range);
    if (err != null) return err;
    err = ml_image_export_ppm(&spec, interp, resolved_range);
    if (err != null) return err;

    Value* out = ml_visualization_make_dict(interp, 12);
    if (out == null || out.tag == ERROR) return out;
    if (Value* set_err = ml_visualization_dict_set_symbol(out, interp, "kind", "image-export")) return set_err;
    if (Value* set_err = ml_visualization_dict_set_symbol(out, interp, "format", "ppm")) return set_err;
    if (Value* set_err = ml_visualization_dict_set_symbol(out, interp, "backend", "ppm")) return set_err;
    if (Value* set_err = ml_visualization_dict_set_symbol(out, interp, "range", resolved_range == ML_IMAGE_RANGE_UNIT ? "unit" : "byte")) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "path", path_value)) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "width", tensor_usz_metadata_value(interp, spec.width, "ml/export-image: image width is too large"))) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "height", tensor_usz_metadata_value(interp, spec.height, "ml/export-image: image height is too large"))) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "channels", tensor_usz_metadata_value(interp, spec.channels, "ml/export-image: image channel count is too large"))) return set_err;
    return out;
}

<*
@require interp != null
*>
fn Value* prim_ml_plot(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) return primitive_arity_raise(interp, "ml/plot", "1 or 2 arguments: data [options]", args.len);
    Value* multi_node = null;
    Value* multi_err = ml_plot_ext_try_multi_series(args[0], args.len == 2 ? args[1] : null, interp, &multi_node);
    if (multi_err != null) return multi_err;
    if (multi_node != null) return multi_node;
    MlVisualizationSeries series;
    Value* err = ml_visualization_series_from_value(args[0], interp, true, "ml/plot", &series);
    if (err != null) return err;
    return ml_plot_ext_make_graph_node(&series, args.len == 2 ? args[1] : null, interp, "ml/plot", "ml/plot", "ML Plot", series.x != null ? "xy" : "line");
}

<*
@require interp != null
*>
fn Value* prim_ml_loss_curve(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) return primitive_arity_raise(interp, "ml/loss-curve", "1 or 2 arguments: losses [options]", args.len);
    MlVisualizationSeries series;
    Value* err = ml_visualization_series_from_value(args[0], interp, false, "ml/loss-curve", &series);
    if (err != null) return err;
    return ml_visualization_make_graph_node(&series, args.len == 2 ? args[1] : null, interp, "ml/loss-curve", "ml/loss-curve", "Loss");
}

<*
@require interp != null
*>
fn Value* prim_ml_tensor_summary(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) return primitive_arity_raise(interp, "ml/tensor-summary", "1 or 2 arguments: tensor [options]", args.len);
    return ml_visualization_tensor_summary(args[0], args.len == 2 ? args[1] : null, interp);
}

<*
@require interp != null
*>
fn Value* prim_ml_confusion_matrix(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2 && args.len != 3) return primitive_arity_raise(interp, "ml/confusion-matrix", "2 or 3 arguments: predictions targets [options]", args.len);
    return ml_confusion_build(args[0], args[1], args.len == 3 ? args[2] : null, interp);
}

<*
@require interp != null
*>
fn Value* prim_ml_export_image(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 2 && args.len != 3) return primitive_arity_raise(interp, "ml/export-image", "2 or 3 arguments: tensor path [options]", args.len);
    return ml_image_export(args[0], args[1], args.len == 3 ? args[2] : null, interp);
}
