module lisp;

import std::core::mem;

struct UiFtxuiPlotData {
    double* values;
    usz value_count;
    usz* series_lengths;
    usz series_count;
    double* x_values;
    usz x_count;
    usz heatmap_rows;
    usz heatmap_cols;
}

fn void ui_ftxui_plot_free_data(UiFtxuiPlotData* data) {
    if (data == null) return;
    if (data.values != null) mem::free(data.values);
    if (data.series_lengths != null) mem::free(data.series_lengths);
    if (data.x_values != null) mem::free(data.x_values);
    *data = {};
}

fn Value* ui_ftxui_plot_track_data(UiFtxuiRunState* state, UiFtxuiPlotData* data, Interp* interp) {
    if (data == null) return null;
    if (data.values != null) {
        Value* err = ui_ftxui_track_heap_or_oom(state, data.values, interp);
        if (err != null) {
            data.values = null;
            ui_ftxui_plot_free_data(data);
            return err;
        }
        data.values = null;
    }
    if (data.series_lengths != null) {
        Value* err = ui_ftxui_track_heap_or_oom(state, data.series_lengths, interp);
        if (err != null) {
            data.series_lengths = null;
            ui_ftxui_plot_free_data(data);
            return err;
        }
        data.series_lengths = null;
    }
    if (data.x_values != null) {
        Value* err = ui_ftxui_track_heap_or_oom(state, data.x_values, interp);
        if (err != null) {
            data.x_values = null;
            ui_ftxui_plot_free_data(data);
            return err;
        }
        data.x_values = null;
    }
    return null;
}

fn Value* ui_ftxui_plot_container_count(Value* seq, usz* out_count, Interp* interp, char[] message) {
    if (out_count == null) return ftxui_raise(interp, "ui/invalid-argument", message);
    *out_count = 0;
    if ((!is_array(seq) && !is_cons(seq)) || !tensor_container_count(seq, out_count) || *out_count == 0) {
        return ftxui_raise(interp, "ui/invalid-argument", message);
    }
    return null;
}

fn Value* ui_ftxui_plot_copy_numbers(Value* seq, double* out_values, usz offset, usz count, Interp* interp, char[] message) {
    if (out_values == null) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui: missing plot buffer");
    for (usz i = 0; i < count; i++) {
        Value* item = tensor_container_nth(seq, i);
        double value = 0.0;
        if (!try_numeric_to_double(item, &value)) return ftxui_raise(interp, "ui/invalid-argument", message);
        if (!conversion_double_is_finite(value)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: plot values must be finite");
        out_values[offset + i] = value;
    }
    return null;
}

fn Value* ui_ftxui_plot_collect_single_series(Value* seq, UiFtxuiPlotData* out, Interp* interp, char[] message) {
    if (out == null) return ftxui_raise(interp, "ui/invalid-argument", message);
    *out = {};
    usz count = 0;
    Value* err = ui_ftxui_plot_container_count(seq, &count, interp, message);
    if (err != null) return err;
    if (count > usz.max / double.sizeof) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: plot series too large");
    double* values = (double*)mem::malloc(double.sizeof * count);
    if (values == null) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui: out of memory allocating plot series");
    err = ui_ftxui_plot_copy_numbers(seq, values, 0, count, interp, message);
    if (err != null) {
        mem::free(values);
        return err;
    }
    out.values = values;
    out.value_count = count;
    return null;
}

fn Value* ui_ftxui_plot_collect_series_list(Value* seq, UiFtxuiPlotData* out, Interp* interp, char[] message) {
    if (out == null) return ftxui_raise(interp, "ui/invalid-argument", message);
    *out = {};
    usz series_count = 0;
    Value* err = ui_ftxui_plot_container_count(seq, &series_count, interp, message);
    if (err != null) return err;
    if (series_count > usz.max / usz.sizeof) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: too many plot series");
    usz* lengths = (usz*)mem::malloc(usz.sizeof * series_count);
    if (lengths == null) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui: out of memory allocating plot series lengths");

    usz total = 0;
    for (usz i = 0; i < series_count; i++) {
        Value* item = tensor_container_nth(seq, i);
        usz count = 0;
        err = ui_ftxui_plot_container_count(item, &count, interp, message);
        if (err != null) {
            mem::free(lengths);
            return err;
        }
        if (total > usz.max - count) {
            mem::free(lengths);
            return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: plot series too large");
        }
        lengths[i] = count;
        total += count;
    }
    if (total == 0 || total > usz.max / double.sizeof) {
        mem::free(lengths);
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: plot series too large");
    }
    double* values = (double*)mem::malloc(double.sizeof * total);
    if (values == null) {
        mem::free(lengths);
        return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui: out of memory allocating plot series");
    }
    usz offset = 0;
    for (usz i = 0; i < series_count; i++) {
        Value* item = tensor_container_nth(seq, i);
        err = ui_ftxui_plot_copy_numbers(item, values, offset, lengths[i], interp, message);
        if (err != null) {
            mem::free(values);
            mem::free(lengths);
            return err;
        }
        offset += lengths[i];
    }
    out.values = values;
    out.value_count = total;
    out.series_lengths = lengths;
    out.series_count = series_count;
    return null;
}

fn Value* ui_ftxui_plot_collect_heatmap(Value* matrix, UiFtxuiPlotData* out, Interp* interp) {
    if (out == null) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: missing heatmap output");
    *out = {};
    usz rows = 0;
    Value* err = ui_ftxui_plot_container_count(matrix, &rows, interp, "ui-ftxui: heatmap matrix must be a non-empty Array/List of numeric rows");
    if (err != null) return err;
    usz cols = 0;
    for (usz row = 0; row < rows; row++) {
        Value* row_value = tensor_container_nth(matrix, row);
        usz row_cols = 0;
        err = ui_ftxui_plot_container_count(row_value, &row_cols, interp, "ui-ftxui: heatmap rows must be non-empty numeric Arrays/Lists");
        if (err != null) return err;
        if (row == 0) cols = row_cols;
        if (row_cols != cols) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: heatmap rows must have equal length");
    }
    if (rows > usz.max / cols || rows * cols > usz.max / double.sizeof) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: heatmap matrix too large");
    }
    usz total = rows * cols;
    double* values = (double*)mem::malloc(double.sizeof * total);
    if (values == null) return ftxui_raise(interp, "ui/out-of-memory", "ui-ftxui: out of memory allocating heatmap matrix");
    usz offset = 0;
    for (usz row = 0; row < rows; row++) {
        Value* row_value = tensor_container_nth(matrix, row);
        err = ui_ftxui_plot_copy_numbers(row_value, values, offset, cols, interp, "ui-ftxui: heatmap values must be finite numbers");
        if (err != null) {
            mem::free(values);
            return err;
        }
        offset += cols;
    }
    out.values = values;
    out.value_count = total;
    out.heatmap_rows = rows;
    out.heatmap_cols = cols;
    return null;
}

fn Value* ui_ftxui_plot_dimension(Value* props, char[] key, int default_value, Interp* interp, int* out_value) {
    if (out_value == null) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: missing plot dimension");
    *out_value = default_value;
    Value* value = ui_ftxui_dict_get(props, key, interp);
    if (value == null) return null;
    if (value.tag != INT || value.int_val <= 0 || value.int_val > (long)int.max) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: plot width/height must be positive Integers");
    }
    *out_value = (int)value.int_val;
    return null;
}

fn int ui_ftxui_plot_kind_from_symbol(Value* plot_kind, Interp* interp) {
    char[] name = ui_ftxui_kind_name(plot_kind, interp);
    if (ui_ftxui_str_eq(name, "xy") || ui_ftxui_str_eq(name, "roc-curve") || ui_ftxui_str_eq(name, "pr-curve")) return OMNI_FTXUI_PLOT_XY;
    if (ui_ftxui_str_eq(name, "multi-series")) return OMNI_FTXUI_PLOT_MULTI_SERIES;
    if (ui_ftxui_str_eq(name, "scatter")) return OMNI_FTXUI_PLOT_SCATTER;
    if (ui_ftxui_str_eq(name, "bar-chart")) return OMNI_FTXUI_PLOT_BAR;
    if (ui_ftxui_str_eq(name, "histogram")) return OMNI_FTXUI_PLOT_HISTOGRAM;
    if (ui_ftxui_str_eq(name, "heatmap")) return OMNI_FTXUI_PLOT_HEATMAP;
    return OMNI_FTXUI_PLOT_LINE;
}

fn int ui_ftxui_plot_marker_from_props(Value* props, Interp* interp) {
    Value* marker = ui_ftxui_dict_get(props, "marker", interp);
    char[] name = ui_ftxui_kind_name(marker, interp);
    if (ui_ftxui_str_eq(name, "cross")) return OMNI_FTXUI_PLOT_MARKER_CROSS;
    if (ui_ftxui_str_eq(name, "plus")) return OMNI_FTXUI_PLOT_MARKER_PLUS;
    return OMNI_FTXUI_PLOT_MARKER_POINT;
}

fn Value* ui_ftxui_plot_read_y_bound(Value* zoom, Interp* interp, char[] key, int* has_value, double* out_value) {
    if (has_value == null || out_value == null) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: missing plot zoom state");
    *has_value = 0;
    *out_value = 0.0;
    if (zoom == null) return null;
    Value* value = ui_ftxui_dict_get(zoom, key, interp);
    if (value == null) return null;
    double numeric = 0.0;
    if (!try_numeric_to_double(value, &numeric)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: plot y zoom bounds must be finite numbers");
    if (!conversion_double_is_finite(numeric)) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: plot y zoom bounds must be finite numbers");
    *has_value = 1;
    *out_value = numeric;
    return null;
}

fn Value* ui_ftxui_plot_apply_y_zoom(Value* props, OmniFtxuiPlotOptions* options, Interp* interp) {
    if (options == null) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: missing plot options");
    Value* zoom = ui_ftxui_dict_get(props, "zoom", interp);
    if (zoom == null) return null;
    if (zoom.tag != HASHMAP) return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: plot zoom must be a Dictionary");
    if (Value* err = ui_ftxui_plot_read_y_bound(zoom, interp, "y-min", &options.has_y_min, &options.y_min)) return err;
    if (Value* err = ui_ftxui_plot_read_y_bound(zoom, interp, "y-max", &options.has_y_max, &options.y_max)) return err;
    if (options.has_y_min != 0 && options.has_y_max != 0 && options.y_min > options.y_max) {
        return ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: plot y-min must be less than or equal to y-max");
    }
    return null;
}

fn Value* ui_ftxui_plot_create_component(
    UiFtxuiRunState* state,
    OmniFtxuiPlotOptions* options,
    UiFtxuiPlotData* data,
    OmniFtxuiComponent** out_component,
    Interp* interp
) {
    Value* track_err = ui_ftxui_plot_track_data(state, data, interp);
    if (track_err != null) return track_err;

    OmniFtxuiElement* element = null;
    OmniFtxuiStatus status = omni_ftxui_element_plot(state.context, options, &element);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "element_plot", status, interp);
    defer if (element != null) omni_ftxui_element_destroy(element);

    OmniFtxuiComponent* component = null;
    status = omni_ftxui_component_from_element(state.context, element, &component);
    if (status != OMNI_FTXUI_STATUS_OK) return ui_ftxui_raise_status(state, "component_from_element", status, interp);

    track_err = ui_ftxui_track_component_or_oom(state, component, interp);
    if (track_err != null) return track_err;
    *out_component = component;
    return null;
}

fn Value* ui_ftxui_lower_typed_graph(
    UiFtxuiRunState* state,
    Value* props,
    OmniFtxuiComponent** out_component,
    Interp* interp
) {
    int plot_kind = ui_ftxui_plot_kind_from_symbol(ui_ftxui_dict_get(props, "plot-kind", interp), interp);
    UiFtxuiPlotData data = {};
    Value* err = null;
    if (plot_kind == OMNI_FTXUI_PLOT_MULTI_SERIES) {
        err = ui_ftxui_plot_collect_series_list(
            ui_ftxui_dict_get(props, "series-list", interp),
            &data,
            interp,
            "ui-ftxui: graph 'series-list' must contain numeric series"
        );
    } else {
        err = ui_ftxui_plot_collect_single_series(
            ui_ftxui_dict_get(props, "series", interp),
            &data,
            interp,
            "ui-ftxui: graph 'series' must be a proper list or array of numbers"
        );
        if (err == null && (plot_kind == OMNI_FTXUI_PLOT_XY || plot_kind == OMNI_FTXUI_PLOT_SCATTER)) {
            double* x_values = null;
            usz x_count = 0;
            err = ui_ftxui_collect_number_sequence(
                ui_ftxui_dict_get(props, "x", interp),
                &x_values,
                &x_count,
                interp,
                "ui-ftxui: graph 'x' must be a proper list or array of numbers"
            );
            if (err == null && x_count != data.value_count) {
                mem::free(x_values);
                err = ftxui_raise(interp, "ui/invalid-argument", "ui-ftxui: graph x and y series must have equal length");
            }
            if (err == null) {
                data.x_values = x_values;
                data.x_count = x_count;
            }
        }
    }
    if (err != null) {
        ui_ftxui_plot_free_data(&data);
        return err;
    }

    int width = 120;
    int height = 40;
    err = ui_ftxui_plot_dimension(props, "width", width, interp, &width);
    if (err == null) err = ui_ftxui_plot_dimension(props, "height", height, interp, &height);
    if (err != null) {
        ui_ftxui_plot_free_data(&data);
        return err;
    }

    OmniFtxuiPlotOptions options = {};
    options.size = OmniFtxuiPlotOptions.sizeof;
    options.abi_version = OMNI_FTXUI_ABI_VERSION;
    options.kind = plot_kind;
    options.width = width;
    options.height = height;
    options.values = data.values;
    options.value_count = data.value_count;
    options.series_lengths = data.series_lengths;
    options.series_count = data.series_count;
    options.x_values = data.x_values;
    options.x_count = data.x_count;
    options.marker = ui_ftxui_plot_marker_from_props(props, interp);
    err = ui_ftxui_plot_apply_y_zoom(props, &options, interp);
    if (err != null) {
        ui_ftxui_plot_free_data(&data);
        return err;
    }
    return ui_ftxui_plot_create_component(state, &options, &data, out_component, interp);
}

fn Value* ui_ftxui_lower_heatmap_node(
    UiFtxuiRunState* state,
    Value* node,
    OmniFtxuiComponent** out_component,
    Interp* interp
) {
    UiFtxuiPlotData data = {};
    Value* err = ui_ftxui_plot_collect_heatmap(ui_ftxui_dict_get(node, "matrix", interp), &data, interp);
    if (err != null) return err;
    int width = 120;
    int height = 40;
    err = ui_ftxui_plot_dimension(node, "width", width, interp, &width);
    if (err == null) err = ui_ftxui_plot_dimension(node, "height", height, interp, &height);
    if (err != null) {
        ui_ftxui_plot_free_data(&data);
        return err;
    }

    OmniFtxuiPlotOptions options = {};
    options.size = OmniFtxuiPlotOptions.sizeof;
    options.abi_version = OMNI_FTXUI_ABI_VERSION;
    options.kind = OMNI_FTXUI_PLOT_HEATMAP;
    options.width = width;
    options.height = height;
    options.values = data.values;
    options.value_count = data.value_count;
    options.heatmap_rows = data.heatmap_rows;
    options.heatmap_cols = data.heatmap_cols;
    options.marker = OMNI_FTXUI_PLOT_MARKER_POINT;
    return ui_ftxui_plot_create_component(state, &options, &data, out_component, interp);
}
