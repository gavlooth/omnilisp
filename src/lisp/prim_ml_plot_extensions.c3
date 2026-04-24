module lisp;

import std::core::mem;

struct MlPlotZoom {
    bool has_x_min;
    bool has_x_max;
    bool has_y_min;
    bool has_y_max;
    double x_min;
    double x_max;
    double y_min;
    double y_max;
}

fn Value* ml_plot_ext_option_source(Value* options, Interp* interp, char[] primitive_name) {
    if (options == null) return null;
    if (options.tag != HASHMAP) return primitive_arg_type_raise(interp, primitive_name, 1, "Dictionary options", options);
    Value* zoom = nn_dict_get(options, interp, "zoom");
    if (zoom == null) return options;
    if (zoom.tag != HASHMAP) return ml_visualization_invalid(interp, "ml plot: zoom option must be a Dictionary");
    return zoom;
}

fn Value* ml_plot_ext_read_zoom_field(
    Value* source,
    Interp* interp,
    char[] key,
    bool* has_value,
    double* out_value
) {
    if (has_value == null || out_value == null) return ml_visualization_invalid(interp, "ml plot: missing zoom state");
    *has_value = false;
    *out_value = 0.0;
    if (source == null) return null;
    Value* value = nn_dict_get(source, interp, key);
    if (value == null) return null;
    double numeric = 0.0;
    if (!ml_visualization_read_finite_number(value, &numeric)) {
        return ml_visualization_invalid(interp, "ml plot: zoom bounds must be finite real numbers");
    }
    *has_value = true;
    *out_value = numeric;
    return null;
}

fn Value* ml_plot_ext_read_zoom(Value* options, Interp* interp, char[] primitive_name, MlPlotZoom* out) {
    if (out == null) return ml_visualization_invalid(interp, "ml plot: missing zoom state");
    out.has_x_min = false;
    out.has_x_max = false;
    out.has_y_min = false;
    out.has_y_max = false;
    out.x_min = 0.0;
    out.x_max = 0.0;
    out.y_min = 0.0;
    out.y_max = 0.0;

    Value* source = ml_plot_ext_option_source(options, interp, primitive_name);
    if (source != null && source.tag == ERROR) return source;
    if (source == null) return null;
    if (Value* err = ml_plot_ext_read_zoom_field(source, interp, "x-min", &out.has_x_min, &out.x_min)) return err;
    if (Value* err = ml_plot_ext_read_zoom_field(source, interp, "x-max", &out.has_x_max, &out.x_max)) return err;
    if (Value* err = ml_plot_ext_read_zoom_field(source, interp, "y-min", &out.has_y_min, &out.y_min)) return err;
    if (Value* err = ml_plot_ext_read_zoom_field(source, interp, "y-max", &out.has_y_max, &out.y_max)) return err;
    if (out.has_x_min && out.has_x_max && out.x_min > out.x_max) {
        return ml_visualization_invalid(interp, "ml plot: x-min must be less than or equal to x-max");
    }
    if (out.has_y_min && out.has_y_max && out.y_min > out.y_max) {
        return ml_visualization_invalid(interp, "ml plot: y-min must be less than or equal to y-max");
    }
    return null;
}

fn bool ml_plot_ext_zoom_has_any(MlPlotZoom* zoom) @inline {
    return zoom != null && (zoom.has_x_min || zoom.has_x_max || zoom.has_y_min || zoom.has_y_max);
}

fn bool ml_plot_ext_zoom_has_x(MlPlotZoom* zoom) @inline {
    return zoom != null && (zoom.has_x_min || zoom.has_x_max);
}

fn Value* ml_plot_ext_apply_zoom_props(Value* props, Interp* interp, MlPlotZoom* zoom) {
    if (!ml_plot_ext_zoom_has_any(zoom)) return null;
    Value* out = ml_visualization_make_dict(interp, 8);
    if (out == null || out.tag == ERROR) return out;
    if (Value* err = ml_visualization_dict_set_symbol(out, interp, "mode", "data-window")) return err;
    if (Value* err = ml_visualization_dict_set(out, interp, "interactive", make_symbol(interp, interp.sym_false))) return err;
    if (zoom.has_x_min) {
        if (Value* err = ml_visualization_dict_set(out, interp, "x-min", make_double(interp, zoom.x_min))) return err;
    }
    if (zoom.has_x_max) {
        if (Value* err = ml_visualization_dict_set(out, interp, "x-max", make_double(interp, zoom.x_max))) return err;
    }
    if (zoom.has_y_min) {
        if (Value* err = ml_visualization_dict_set(out, interp, "y-min", make_double(interp, zoom.y_min))) return err;
    }
    if (zoom.has_y_max) {
        if (Value* err = ml_visualization_dict_set(out, interp, "y-max", make_double(interp, zoom.y_max))) return err;
    }
    return ml_visualization_dict_set(props, interp, "zoom", out);
}

fn Value* ml_plot_ext_window_series(MlVisualizationSeries* series, Value* options, Interp* interp, char[] primitive_name, MlVisualizationSeries* out, MlPlotZoom* zoom) {
    if (series == null || out == null || zoom == null) return ml_visualization_invalid(interp, "ml plot: missing series state");
    *out = *series;
    Value* err = ml_plot_ext_read_zoom(options, interp, primitive_name, zoom);
    if (err != null) return err;
    if (!ml_plot_ext_zoom_has_x(zoom)) return null;

    Value* y = make_array_checked(interp, series.count, "ml plot: failed to allocate zoomed y series");
    if (y == null || y.tag == ERROR) return y;
    Value* x = series.x != null ? make_array_checked(interp, series.count, "ml plot: failed to allocate zoomed x series") : null;
    if (x != null && x.tag == ERROR) return x;

    for (usz i = 0; i < series.count; i++) {
        double x_value = (double)i;
        if (series.x != null && !ml_visualization_read_finite_number(series.x.array_val.items[i], &x_value)) {
            return ml_visualization_invalid(interp, "ml plot: x values must be finite real numbers");
        }
        if (zoom.has_x_min && x_value < zoom.x_min) continue;
        if (zoom.has_x_max && x_value > zoom.x_max) continue;
        double y_value = 0.0;
        if (!ml_visualization_read_finite_number(series.y.array_val.items[i], &y_value)) {
            return ml_visualization_invalid(interp, "ml plot: y values must be finite real numbers");
        }
        if (Value* push_y = ml_visualization_push_double(y, interp, y_value)) return push_y;
        if (x != null) {
            if (Value* push_x = ml_visualization_push_double(x, interp, x_value)) return push_x;
        }
    }
    if (y.array_val.length == 0) return ml_visualization_invalid(interp, "ml plot: zoom window contains no samples");
    out.y = y;
    out.x = x;
    out.count = y.array_val.length;
    return null;
}

fn Value* ml_plot_ext_make_points(MlVisualizationSeries* series, Interp* interp) {
    if (series == null || series.x == null || series.y == null) return ml_visualization_invalid(interp, "ml plot: x/y points are required");
    Value* points = make_array_checked(interp, series.count, "ml plot: failed to allocate points");
    if (points == null || points.tag == ERROR) return points;
    for (usz i = 0; i < series.count; i++) {
        Value* point = make_array_checked(interp, 2, "ml plot: failed to allocate point");
        if (point == null || point.tag == ERROR) return point;
        double x = 0.0;
        double y = 0.0;
        if (!ml_visualization_read_finite_number(series.x.array_val.items[i], &x) ||
            !ml_visualization_read_finite_number(series.y.array_val.items[i], &y)) {
            return ml_visualization_invalid(interp, "ml plot: points must contain finite real numbers");
        }
        if (Value* err = ml_visualization_push_double(point, interp, x)) return err;
        if (Value* err = ml_visualization_push_double(point, interp, y)) return err;
        points.array_val.items[points.array_val.length] = point;
        points.array_val.length++;
    }
    return points;
}

fn Value* ml_plot_ext_make_graph_node(
    MlVisualizationSeries* series,
    Value* options,
    Interp* interp,
    char[] primitive_name,
    char[] visualization_name,
    char[] default_title,
    char[] plot_kind
) {
    MlVisualizationSeries visible;
    MlPlotZoom zoom;
    Value* err = ml_plot_ext_window_series(series, options, interp, primitive_name, &visible, &zoom);
    if (err != null) return err;
    Value* node = ml_visualization_make_graph_node(&visible, options, interp, primitive_name, visualization_name, default_title);
    if (node == null || node.tag == ERROR) return node;
    Value* props = nn_dict_get(node, interp, "props");
    if (props == null) return ml_visualization_invalid(interp, "ml plot: graph props are missing");
    if (Value* set_err = ml_visualization_dict_set_symbol(props, interp, "plot-kind", plot_kind)) return set_err;
    if (Value* set_err = ml_plot_ext_apply_zoom_props(props, interp, &zoom)) return set_err;
    return node;
}

fn bool ml_plot_ext_option_plot_kind_is(Value* options, Interp* interp, char[] name) {
    if (options == null || options.tag != HASHMAP) return false;
    Value* value = nn_dict_get(options, interp, "plot-kind");
    return value != null && nn_symbol_is(value, interp, name);
}

fn Value* ml_plot_ext_try_multi_series(Value* input, Value* options, Interp* interp, Value** out_node) {
    if (out_node == null) return ml_visualization_invalid(interp, "ml/plot: missing output state");
    *out_node = null;
    if (options != null && options.tag != HASHMAP) return primitive_arg_type_raise(interp, "ml/plot", 1, "Dictionary options", options);
    if (!is_array(input) && !is_cons(input)) return null;

    usz count = 0;
    if (!tensor_container_count(input, &count) || count < 2) return null;
    Value* first = tensor_container_nth(input, 0);
    if (!is_array(first) && !is_cons(first)) return null;
    usz first_count = 0;
    if (!tensor_container_count(first, &first_count) || first_count == 0) return null;
    if (first_count == 2 && !ml_plot_ext_option_plot_kind_is(options, interp, "multi-series")) {
        if (ml_plot_ext_option_plot_kind_is(options, interp, "xy")) return null;
        return ml_visualization_invalid(interp, "ml/plot: ambiguous two-wide nested input requires plot-kind 'xy or 'multi-series");
    }

    Value* series_list = make_array_checked(interp, count, "ml/plot: failed to allocate series-list");
    if (series_list == null || series_list.tag == ERROR) return series_list;
    Value* primary = null;
    for (usz i = 0; i < count; i++) {
        Value* item = tensor_container_nth(input, i);
        if (!is_array(item) && !is_cons(item)) return ml_visualization_invalid(interp, "ml/plot: multi-series input must contain numeric series");
        MlVisualizationSeries series;
        Value* err = ml_visualization_series_from_container(item, interp, false, "ml/plot", &series);
        if (err != null) return err;
        MlVisualizationSeries visible;
        MlPlotZoom series_zoom;
        err = ml_plot_ext_window_series(&series, options, interp, "ml/plot", &visible, &series_zoom);
        if (err != null) return err;
        if (primary == null) primary = visible.y;
        series_list.array_val.items[series_list.array_val.length] = visible.y;
        series_list.array_val.length++;
    }

    Value* props = ml_visualization_make_dict(interp, 16);
    if (props == null || props.tag == ERROR) return props;
    if (Value* err = ml_visualization_dict_set(props, interp, "series", primary)) return err;
    if (Value* err = ml_visualization_dict_set(props, interp, "series-list", series_list)) return err;
    if (Value* err = ml_visualization_dict_set_symbol(props, interp, "visualization", "ml/plot")) return err;
    if (Value* err = ml_visualization_dict_set_symbol(props, interp, "plot-kind", "multi-series")) return err;
    if (Value* err = ml_visualization_apply_options(props, options, interp, "ml/plot", "ML Plot")) return err;
    MlPlotZoom zoom;
    if (Value* err = ml_plot_ext_read_zoom(options, interp, "ml/plot", &zoom)) return err;
    if (Value* err = ml_plot_ext_apply_zoom_props(props, interp, &zoom)) return err;

    Value* legend = options != null ? nn_dict_get(options, interp, "legend") : null;
    if (legend != null) {
        if (!is_array(legend) && !is_cons(legend)) return ml_visualization_invalid(interp, "ml/plot: legend option must be an Array or List");
        if (Value* err = ml_visualization_dict_set(props, interp, "legend", legend)) return err;
    }

    Value* node = ml_visualization_make_dict(interp, 8);
    if (node == null || node.tag == ERROR) return node;
    if (Value* err = ml_visualization_dict_set_symbol(node, interp, "kind", "graph")) return err;
    if (Value* err = ml_visualization_dict_set(node, interp, "props", props)) return err;
    if (Value* err = ml_visualization_dict_set(node, interp, "children", make_nil(interp))) return err;
    *out_node = node;
    return null;
}

fn Value* ml_plot_ext_graph_from_pairs(Value* input, Value* options, Interp* interp, char[] primitive_name, char[] default_title, char[] plot_kind) {
    MlVisualizationSeries series;
    Value* err = ml_visualization_series_from_value(input, interp, true, primitive_name, &series);
    if (err != null) return err;
    if (series.x == null) return ml_visualization_invalid(interp, "ml plot: expected x/y pairs or rank-2 [N 2] tensor");
    Value* node = ml_plot_ext_make_graph_node(&series, options, interp, primitive_name, primitive_name, default_title, plot_kind);
    if (node == null || node.tag == ERROR) return node;
    Value* props = nn_dict_get(node, interp, "props");
    if (props == null) return ml_visualization_invalid(interp, "ml plot: graph props are missing");
    MlVisualizationSeries visible;
    MlPlotZoom zoom;
    err = ml_plot_ext_window_series(&series, options, interp, primitive_name, &visible, &zoom);
    if (err != null) return err;
    if (Value* set_err = ml_visualization_dict_set(props, interp, "points", ml_plot_ext_make_points(&visible, interp))) return set_err;
    return node;
}

<*
@require interp != null
*>
fn Value* prim_ml_scatter(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) return primitive_arity_raise(interp, "ml/scatter", "1 or 2 arguments: points [options]", args.len);
    Value* options = args.len == 2 ? args[1] : null;
    Value* node = ml_plot_ext_graph_from_pairs(args[0], options, interp, "ml/scatter", "Scatter", "scatter");
    if (node == null || node.tag == ERROR) return node;
    Value* props = nn_dict_get(node, interp, "props");
    if (props == null) return ml_visualization_invalid(interp, "ml/scatter: graph props are missing");
    Value* marker = options != null ? nn_dict_get(options, interp, "marker") : null;
    if (marker == null) {
        if (Value* err = ml_visualization_dict_set_symbol(props, interp, "marker", "point")) return err;
    } else {
        if (marker.tag != SYMBOL && marker.tag != STRING) return ml_visualization_invalid(interp, "ml/scatter: marker option must be a Symbol or String");
        if (Value* err = ml_visualization_dict_set(props, interp, "marker", marker)) return err;
    }
    return node;
}

<*
@require interp != null
*>
fn Value* prim_ml_roc_curve(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) return primitive_arity_raise(interp, "ml/roc-curve", "1 or 2 arguments: points [options]", args.len);
    Value* node = ml_plot_ext_graph_from_pairs(args[0], args.len == 2 ? args[1] : null, interp, "ml/roc-curve", "ROC Curve", "roc-curve");
    if (node == null || node.tag == ERROR) return node;
    Value* props = nn_dict_get(node, interp, "props");
    if (props == null) return ml_visualization_invalid(interp, "ml/roc-curve: graph props are missing");
    if (Value* err = ml_visualization_dict_set_symbol(props, interp, "x-label", "false-positive-rate")) return err;
    if (Value* err = ml_visualization_dict_set_symbol(props, interp, "y-label", "true-positive-rate")) return err;
    return node;
}

<*
@require interp != null
*>
fn Value* prim_ml_pr_curve(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) return primitive_arity_raise(interp, "ml/pr-curve", "1 or 2 arguments: points [options]", args.len);
    Value* node = ml_plot_ext_graph_from_pairs(args[0], args.len == 2 ? args[1] : null, interp, "ml/pr-curve", "Precision-Recall Curve", "pr-curve");
    if (node == null || node.tag == ERROR) return node;
    Value* props = nn_dict_get(node, interp, "props");
    if (props == null) return ml_visualization_invalid(interp, "ml/pr-curve: graph props are missing");
    if (Value* err = ml_visualization_dict_set_symbol(props, interp, "x-label", "recall")) return err;
    if (Value* err = ml_visualization_dict_set_symbol(props, interp, "y-label", "precision")) return err;
    return node;
}

<*
@require interp != null
*>
fn Value* prim_ml_bar_chart(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) return primitive_arity_raise(interp, "ml/bar-chart", "1 or 2 arguments: values [options]", args.len);
    MlVisualizationSeries series;
    Value* err = ml_visualization_series_from_value(args[0], interp, false, "ml/bar-chart", &series);
    if (err != null) return err;
    Value* node = ml_plot_ext_make_graph_node(&series, args.len == 2 ? args[1] : null, interp, "ml/bar-chart", "ml/bar-chart", "Bar Chart", "bar-chart");
    if (node == null || node.tag == ERROR) return node;
    Value* props = nn_dict_get(node, interp, "props");
    if (props == null) return ml_visualization_invalid(interp, "ml/bar-chart: graph props are missing");
    Value* labels = args.len == 2 ? nn_dict_get(args[1], interp, "labels") : null;
    if (labels != null) {
        if (!is_array(labels) && !is_cons(labels)) return ml_visualization_invalid(interp, "ml/bar-chart: labels option must be an Array or List");
        if (Value* set_err = ml_visualization_dict_set(props, interp, "labels", labels)) return set_err;
    }
    return node;
}

fn Value* ml_plot_ext_histogram_bins(Value* options, Interp* interp, int* out_bins) {
    if (out_bins == null) return ml_visualization_invalid(interp, "ml/histogram: missing bin state");
    *out_bins = 10;
    Value* bins = options != null ? nn_dict_get(options, interp, "bins") : null;
    if (bins == null) return null;
    if (bins.tag != INT || bins.int_val <= 0 || bins.int_val > 4096) {
        return ml_visualization_invalid(interp, "ml/histogram: bins option must be a positive Integer no larger than 4096");
    }
    *out_bins = (int)bins.int_val;
    return null;
}

<*
@require interp != null
*>
fn Value* prim_ml_histogram(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) return primitive_arity_raise(interp, "ml/histogram", "1 or 2 arguments: values [options]", args.len);
    Value* options = args.len == 2 ? args[1] : null;
    if (options != null && options.tag != HASHMAP) return primitive_arg_type_raise(interp, "ml/histogram", 1, "Dictionary options", options);
    MlVisualizationSeries samples;
    Value* err = ml_visualization_series_from_value(args[0], interp, false, "ml/histogram", &samples);
    if (err != null) return err;
    int bin_count = 10;
    err = ml_plot_ext_histogram_bins(options, interp, &bin_count);
    if (err != null) return err;

    double min_value = 0.0;
    double max_value = 0.0;
    for (usz i = 0; i < samples.count; i++) {
        double value = 0.0;
        if (!ml_visualization_read_finite_number(samples.y.array_val.items[i], &value)) return ml_visualization_invalid(interp, "ml/histogram: samples must be finite real numbers");
        if (i == 0 || value < min_value) min_value = value;
        if (i == 0 || value > max_value) max_value = value;
    }
    if (min_value == max_value) {
        min_value -= 0.5;
        max_value += 0.5;
    }

    double* counts = (double*)mem::malloc((usz)bin_count * double.sizeof);
    if (counts == null) return ml_visualization_oom(interp);
    defer mem::free(counts);
    for (int i = 0; i < bin_count; i++) counts[i] = 0.0;
    double width = (max_value - min_value) / (double)bin_count;
    if (!conversion_double_is_finite(width) || width <= 0.0) return ml_visualization_invalid(interp, "ml/histogram: invalid bin range");
    for (usz i = 0; i < samples.count; i++) {
        double value = 0.0;
        if (!ml_visualization_read_finite_number(samples.y.array_val.items[i], &value)) return ml_visualization_invalid(interp, "ml/histogram: samples must be finite real numbers");
        int bin = value == max_value ? bin_count - 1 : (int)((value - min_value) / width);
        if (bin < 0) bin = 0;
        if (bin >= bin_count) bin = bin_count - 1;
        counts[bin] += 1.0;
    }

    Value* series = make_array_checked(interp, (usz)bin_count, "ml/histogram: failed to allocate counts");
    if (series == null || series.tag == ERROR) return series;
    Value* edges = make_array_checked(interp, (usz)bin_count + 1, "ml/histogram: failed to allocate bin edges");
    if (edges == null || edges.tag == ERROR) return edges;
    for (int i = 0; i < bin_count; i++) {
        if (Value* push = ml_visualization_push_double(series, interp, counts[i])) return push;
    }
    for (int i = 0; i <= bin_count; i++) {
        if (Value* push = ml_visualization_push_double(edges, interp, min_value + width * (double)i)) return push;
    }

    MlVisualizationSeries hist;
    hist.y = series;
    hist.x = null;
    hist.count = (usz)bin_count;
    Value* node = ml_plot_ext_make_graph_node(&hist, options, interp, "ml/histogram", "ml/histogram", "Histogram", "histogram");
    if (node == null || node.tag == ERROR) return node;
    Value* props = nn_dict_get(node, interp, "props");
    if (props == null) return ml_visualization_invalid(interp, "ml/histogram: graph props are missing");
    if (Value* set_err = ml_visualization_dict_set(props, interp, "bin-edges", edges)) return set_err;
    if (Value* set_err = ml_visualization_dict_set(props, interp, "sample-count", tensor_usz_metadata_value(interp, samples.count, "ml/histogram: sample count is too large"))) return set_err;
    return node;
}

fn Value* ml_plot_ext_heatmap_from_tensor(Value* input, Interp* interp, Value** matrix_out, usz* height_out, usz* width_out, double* min_out, double* max_out) {
    TensorVal* tensor = null;
    Value* materialized = null;
    Value* err = tensor_expr_resolve_concrete_cpu(
        input,
        &tensor,
        &materialized,
        interp,
        "ml/heatmap: tensor backing storage is missing",
        "ml/heatmap: currently supports concrete CPU tensor data"
    );
    if (err != null) return err;
    defer if (materialized != null && materialized != input) boundary_cleanup_materialized_value(null, materialized, interp.current_scope);
    if (!ml_reduction_supported_dtype(tensor.dtype)) {
        return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/heatmap: only Float64 and Float32 tensor matrices are supported", null);
    }
    if (!tensor_dense_row_major(tensor)) return raise_error_with_payload_names(interp, "tensor", "tensor/backend-unsupported", "ml/heatmap: currently supports dense row-major tensors", null);
    if (tensor.rank != 2 || tensor.shape[0] == 0 || tensor.shape[1] == 0) return ml_visualization_invalid(interp, "ml/heatmap: expected non-empty rank-2 matrix data");
    *height_out = tensor.shape[0];
    *width_out = tensor.shape[1];
    Value* rows = make_array_checked(interp, *height_out, "ml/heatmap: failed to allocate matrix rows");
    if (rows == null || rows.tag == ERROR) return rows;
    for (usz row = 0; row < *height_out; row++) {
        Value* row_values = make_array_checked(interp, *width_out, "ml/heatmap: failed to allocate matrix row");
        if (row_values == null || row_values.tag == ERROR) return row_values;
        for (usz col = 0; col < *width_out; col++) {
            double value = 0.0;
            if (!ml_reduction_read(tensor, row * *width_out + col, &value) || !conversion_double_is_finite(value)) {
                return ml_visualization_invalid(interp, "ml/heatmap: values must be finite real numbers");
            }
            if (row == 0 && col == 0 || value < *min_out) *min_out = value;
            if (row == 0 && col == 0 || value > *max_out) *max_out = value;
            if (Value* push = ml_visualization_push_double(row_values, interp, value)) return push;
        }
        rows.array_val.items[rows.array_val.length] = row_values;
        rows.array_val.length++;
    }
    *matrix_out = rows;
    return null;
}

fn Value* ml_plot_ext_heatmap_from_container(Value* input, Interp* interp, Value** matrix_out, usz* height_out, usz* width_out, double* min_out, double* max_out) {
    usz height = 0;
    if (!tensor_container_count(input, &height) || (!is_array(input) && !is_cons(input)) || height == 0) {
        return ml_visualization_invalid(interp, "ml/heatmap: expected non-empty rank-2 matrix data");
    }
    Value* rows = make_array_checked(interp, height, "ml/heatmap: failed to allocate matrix rows");
    if (rows == null || rows.tag == ERROR) return rows;
    usz width = 0;
    for (usz row = 0; row < height; row++) {
        Value* row_input = tensor_container_nth(input, row);
        usz row_width = 0;
        if (!tensor_container_count(row_input, &row_width) || (!is_array(row_input) && !is_cons(row_input)) || row_width == 0) {
            return ml_visualization_invalid(interp, "ml/heatmap: matrix rows must be non-empty numeric Arrays or Lists");
        }
        if (row == 0) width = row_width;
        if (row_width != width) return ml_visualization_invalid(interp, "ml/heatmap: matrix rows must have equal length");
        Value* row_values = make_array_checked(interp, width, "ml/heatmap: failed to allocate matrix row");
        if (row_values == null || row_values.tag == ERROR) return row_values;
        for (usz col = 0; col < width; col++) {
            double value = 0.0;
            if (!ml_visualization_read_finite_number(tensor_container_nth(row_input, col), &value)) {
                return ml_visualization_invalid(interp, "ml/heatmap: values must be finite real numbers");
            }
            if (row == 0 && col == 0 || value < *min_out) *min_out = value;
            if (row == 0 && col == 0 || value > *max_out) *max_out = value;
            if (Value* push = ml_visualization_push_double(row_values, interp, value)) return push;
        }
        rows.array_val.items[rows.array_val.length] = row_values;
        rows.array_val.length++;
    }
    *height_out = height;
    *width_out = width;
    *matrix_out = rows;
    return null;
}

<*
@require interp != null
*>
fn Value* prim_ml_heatmap(Value*[] args, Env* env, Interp* interp) {
    if (args.len != 1 && args.len != 2) return primitive_arity_raise(interp, "ml/heatmap", "1 or 2 arguments: matrix [options]", args.len);
    Value* options = args.len == 2 ? args[1] : null;
    if (options != null && options.tag != HASHMAP) return primitive_arg_type_raise(interp, "ml/heatmap", 1, "Dictionary options", options);
    Value* matrix = null;
    usz height = 0;
    usz width = 0;
    double min_value = 0.0;
    double max_value = 0.0;
    Value* err = is_tensor(args[0])
        ? ml_plot_ext_heatmap_from_tensor(args[0], interp, &matrix, &height, &width, &min_value, &max_value)
        : ml_plot_ext_heatmap_from_container(args[0], interp, &matrix, &height, &width, &min_value, &max_value);
    if (err != null) return err;

    Value* out = ml_visualization_make_dict(interp, 16);
    if (out == null || out.tag == ERROR) return out;
    if (Value* set_err = ml_visualization_dict_set_symbol(out, interp, "kind", "heatmap")) return set_err;
    if (Value* set_err = ml_visualization_dict_set_symbol(out, interp, "visualization", "ml/heatmap")) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "matrix", matrix)) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "height", tensor_usz_metadata_value(interp, height, "ml/heatmap: height is too large"))) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "width", tensor_usz_metadata_value(interp, width, "ml/heatmap: width is too large"))) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "min", make_double(interp, min_value))) return set_err;
    if (Value* set_err = ml_visualization_dict_set(out, interp, "max", make_double(interp, max_value))) return set_err;
    if (Value* set_err = ml_visualization_apply_options(out, options, interp, "ml/heatmap", "Heatmap")) return set_err;
    return out;
}
