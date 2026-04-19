omni_ftxui_status omni_ftxui_element_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_element_create_options* options,
    omni_ftxui_element_t* const* children,
    size_t child_count,
    omni_ftxui_element_t** out_element
) {
    if (options == nullptr || out_element == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: null argument");
    }
    if (!validate_sized_abi(options->size, options->abi_version, sizeof(omni_ftxui_element_create_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: invalid option ABI");
    }
    if (child_count > 0 && children == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: nonzero child_count with null children");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)children;
    (void)child_count;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "element_create: FTXUI backend unavailable");
#else
    return status_try(context, "element_create: out of memory", "element_create: exception", [&]() -> omni_ftxui_status {
    ftxui::Element result;
    const char* text = options->text ? options->text : "";
    const ftxui::Elements elems = copy_children(children, child_count);
    switch (options->kind) {
        case OMNI_FTXUI_ELEMENT_EMPTY: result = ftxui::emptyElement(); break;
        case OMNI_FTXUI_ELEMENT_TEXT: result = ftxui::text(text); break;
        case OMNI_FTXUI_ELEMENT_VTEXT: result = ftxui::vtext(text); break;
        case OMNI_FTXUI_ELEMENT_PARAGRAPH: result = ftxui::paragraph(text); break;
        case OMNI_FTXUI_ELEMENT_PARAGRAPH_LEFT: result = ftxui::paragraphAlignLeft(text); break;
        case OMNI_FTXUI_ELEMENT_PARAGRAPH_CENTER: result = ftxui::paragraphAlignCenter(text); break;
        case OMNI_FTXUI_ELEMENT_PARAGRAPH_RIGHT: result = ftxui::paragraphAlignRight(text); break;
        case OMNI_FTXUI_ELEMENT_PARAGRAPH_JUSTIFY: result = ftxui::paragraphAlignJustify(text); break;
        case OMNI_FTXUI_ELEMENT_SEPARATOR: result = ftxui::separator(); break;
        case OMNI_FTXUI_ELEMENT_SEPARATOR_LIGHT: result = ftxui::separatorLight(); break;
        case OMNI_FTXUI_ELEMENT_SEPARATOR_HEAVY: result = ftxui::separatorHeavy(); break;
        case OMNI_FTXUI_ELEMENT_SEPARATOR_DOUBLE: result = ftxui::separatorDouble(); break;
        case OMNI_FTXUI_ELEMENT_GAUGE: result = ftxui::gauge(options->gauge_progress); break;
        case OMNI_FTXUI_ELEMENT_GAUGE_LEFT: result = ftxui::gaugeLeft(options->gauge_progress); break;
        case OMNI_FTXUI_ELEMENT_GAUGE_RIGHT: result = ftxui::gaugeRight(options->gauge_progress); break;
        case OMNI_FTXUI_ELEMENT_GAUGE_UP: result = ftxui::gaugeUp(options->gauge_progress); break;
        case OMNI_FTXUI_ELEMENT_GAUGE_DOWN: result = ftxui::gaugeDown(options->gauge_progress); break;
        case OMNI_FTXUI_ELEMENT_HBOX: result = ftxui::hbox(elems); break;
        case OMNI_FTXUI_ELEMENT_VBOX: result = ftxui::vbox(elems); break;
        case OMNI_FTXUI_ELEMENT_DBOX: result = ftxui::dbox(elems); break;
        case OMNI_FTXUI_ELEMENT_HFLOW: result = ftxui::hflow(elems); break;
        case OMNI_FTXUI_ELEMENT_VFLOW: result = ftxui::vflow(elems); break;
        case OMNI_FTXUI_ELEMENT_FLEXBOX: result = ftxui::flexbox(elems); break;
        case OMNI_FTXUI_ELEMENT_GRIDBOX: {
            std::vector<ftxui::Elements> lines;
            lines.push_back(elems);
            result = ftxui::gridbox(std::move(lines));
            break;
        }
        case OMNI_FTXUI_ELEMENT_WINDOW:
            if (elems.size() < 2) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: window requires title and body children");
            }
            result = ftxui::window(elems[0], elems[1]);
            break;
        case OMNI_FTXUI_ELEMENT_FILLER: result = ftxui::filler(); break;
        case OMNI_FTXUI_ELEMENT_SPINNER:
            result = ftxui::spinner(options->spinner_charset, options->spinner_frame);
            break;
        case OMNI_FTXUI_ELEMENT_CANVAS: {
            if (options->canvas_width <= 0 || options->canvas_height <= 0) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: canvas requires positive width/height");
            }
            ftxui::Canvas canvas(options->canvas_width, options->canvas_height);
            result = ftxui::canvas(canvas);
            break;
        }
        case OMNI_FTXUI_ELEMENT_TABLE: {
            if (options->table_rows == 0 || options->table_cols == 0) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: table requires positive rows/cols");
            }
            if (options->table_cols > std::numeric_limits<size_t>::max() / options->table_rows) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: table rows*cols overflow");
            }
            size_t table_cell_count = options->table_rows * options->table_cols;
            if (table_cell_count != elems.size()) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: table rows*cols must match child_count");
            }
            std::vector<std::vector<ftxui::Element>> rows;
            rows.reserve(options->table_rows);
            size_t idx = 0;
            for (size_t r = 0; r < options->table_rows; ++r) {
                std::vector<ftxui::Element> row;
                row.reserve(options->table_cols);
                for (size_t c = 0; c < options->table_cols; ++c) row.push_back(elems[idx++]);
                rows.push_back(std::move(row));
            }
            ftxui::Table table(std::move(rows));
            result = table.Render();
            break;
        }
        case OMNI_FTXUI_ELEMENT_GRAPH:
            if (options->graph_cb == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: graph requires graph_cb");
            }
            result = ftxui::graph([context, user_data = options->graph_user_data, graph_cb = options->graph_cb](int width, int height) {
                return call_graph_callback(context, user_data, graph_cb, width, height);
            });
            break;
        default:
            return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_create: unknown element kind");
    }
    auto* out = make_element(std::move(result));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "element_create: allocation failed");
    *out_element = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_element_graph_from_series(
    omni_ftxui_context_t* context,
    const double* values,
    size_t value_count,
    omni_ftxui_element_t** out_element
) {
    if (values == nullptr || value_count == 0 || out_element == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_graph_from_series: invalid argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)values;
    (void)value_count;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "element_graph_from_series: FTXUI backend unavailable");
#else
    return status_try(context, "element_graph_from_series: out of memory", "element_graph_from_series: exception", [&]() -> omni_ftxui_status {
    auto series = std::make_shared<std::vector<double>>(values, values + value_count);
    auto* out = make_element(ftxui::graph([series](int width, int height) {
        return graph_values_from_series(series, width, height);
    }));
    if (out == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "element_graph_from_series: allocation failed");
    }
    *out_element = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

void omni_ftxui_element_destroy(omni_ftxui_element_t* element) {
    delete element;
}

omni_ftxui_status omni_ftxui_element_apply_decorator(
    omni_ftxui_context_t* context,
    omni_ftxui_element_t* element,
    const omni_ftxui_decorator_options* options
) {
    if (element == nullptr || options == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_apply_decorator: null argument");
    }
    if (!validate_sized_abi(options->size, options->abi_version, sizeof(omni_ftxui_decorator_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_apply_decorator: invalid option ABI");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)options;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "element_apply_decorator: FTXUI backend unavailable");
#else
    return status_try(context, "element_apply_decorator: out of memory", "element_apply_decorator: exception", [&]() -> omni_ftxui_status {
    switch (options->kind) {
        case OMNI_FTXUI_DECORATOR_BORDER: element->element |= ftxui::border; break;
        case OMNI_FTXUI_DECORATOR_BORDER_LIGHT: element->element |= ftxui::borderLight; break;
        case OMNI_FTXUI_DECORATOR_BORDER_HEAVY: element->element |= ftxui::borderHeavy; break;
        case OMNI_FTXUI_DECORATOR_BORDER_DOUBLE: element->element |= ftxui::borderDouble; break;
        case OMNI_FTXUI_DECORATOR_BORDER_ROUNDED: element->element |= ftxui::borderRounded; break;
        case OMNI_FTXUI_DECORATOR_BORDER_EMPTY: element->element |= ftxui::borderEmpty; break;
        case OMNI_FTXUI_DECORATOR_FRAME: element->element |= ftxui::frame; break;
        case OMNI_FTXUI_DECORATOR_XFRAME: element->element |= ftxui::xframe; break;
        case OMNI_FTXUI_DECORATOR_YFRAME: element->element |= ftxui::yframe; break;
        case OMNI_FTXUI_DECORATOR_CENTER: element->element |= ftxui::center; break;
        case OMNI_FTXUI_DECORATOR_HCENTER: element->element |= ftxui::hcenter; break;
        case OMNI_FTXUI_DECORATOR_VCENTER: element->element |= ftxui::vcenter; break;
        case OMNI_FTXUI_DECORATOR_ALIGN_RIGHT: element->element |= ftxui::align_right; break;
        case OMNI_FTXUI_DECORATOR_BOLD: element->element |= ftxui::bold; break;
        case OMNI_FTXUI_DECORATOR_ITALIC: element->element |= ftxui::italic; break;
        case OMNI_FTXUI_DECORATOR_DIM: element->element |= ftxui::dim; break;
        case OMNI_FTXUI_DECORATOR_INVERTED: element->element |= ftxui::inverted; break;
        case OMNI_FTXUI_DECORATOR_UNDERLINED: element->element |= ftxui::underlined; break;
        case OMNI_FTXUI_DECORATOR_UNDERLINED_DOUBLE: element->element |= ftxui::underlinedDouble; break;
        case OMNI_FTXUI_DECORATOR_STRIKETHROUGH: element->element |= ftxui::strikethrough; break;
        case OMNI_FTXUI_DECORATOR_BLINK: element->element |= ftxui::blink; break;
        case OMNI_FTXUI_DECORATOR_COLOR: element->element |= ftxui::color(map_color(options->color)); break;
        case OMNI_FTXUI_DECORATOR_BGCOLOR: element->element |= ftxui::bgcolor(map_color(options->color)); break;
        case OMNI_FTXUI_DECORATOR_FLEX: element->element |= ftxui::flex; break;
        case OMNI_FTXUI_DECORATOR_FLEX_GROW: element->element |= ftxui::flex_grow; break;
        case OMNI_FTXUI_DECORATOR_FLEX_SHRINK: element->element |= ftxui::flex_shrink; break;
        case OMNI_FTXUI_DECORATOR_XFLEX: element->element |= ftxui::xflex; break;
        case OMNI_FTXUI_DECORATOR_XFLEX_GROW: element->element |= ftxui::xflex_grow; break;
        case OMNI_FTXUI_DECORATOR_XFLEX_SHRINK: element->element |= ftxui::xflex_shrink; break;
        case OMNI_FTXUI_DECORATOR_YFLEX: element->element |= ftxui::yflex; break;
        case OMNI_FTXUI_DECORATOR_YFLEX_GROW: element->element |= ftxui::yflex_grow; break;
        case OMNI_FTXUI_DECORATOR_YFLEX_SHRINK: element->element |= ftxui::yflex_shrink; break;
        case OMNI_FTXUI_DECORATOR_NOTFLEX: element->element |= ftxui::notflex; break;
        case OMNI_FTXUI_DECORATOR_FOCUS: element->element |= ftxui::focus; break;
        case OMNI_FTXUI_DECORATOR_SELECT: element->element |= ftxui::select; break;
        case OMNI_FTXUI_DECORATOR_AUTOMERGE: element->element |= ftxui::automerge; break;
        case OMNI_FTXUI_DECORATOR_VSCROLL_INDICATOR: element->element |= ftxui::vscroll_indicator; break;
        case OMNI_FTXUI_DECORATOR_HSCROLL_INDICATOR: element->element |= ftxui::hscroll_indicator; break;
        case OMNI_FTXUI_DECORATOR_CLEAR_UNDER: element->element |= ftxui::clear_under; break;
        case OMNI_FTXUI_DECORATOR_SIZE:
            element->element |= ftxui::size(
                map_width_or_height(options->width_or_height),
                map_constraint(options->constraint),
                options->size_value
            );
            break;
        case OMNI_FTXUI_DECORATOR_HYPERLINK:
            if (options->link == nullptr) {
                return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_apply_decorator: hyperlink requires link");
            }
            element->element |= ftxui::hyperlink(options->link);
            break;
        case OMNI_FTXUI_DECORATOR_SELECTION_STYLE_RESET:
            element->element = ftxui::selectionStyleReset(element->element);
            break;
        case OMNI_FTXUI_DECORATOR_SELECTION_COLOR:
            element->element |= ftxui::selectionColor(map_color(options->color));
            break;
        case OMNI_FTXUI_DECORATOR_SELECTION_BACKGROUND_COLOR:
            element->element |= ftxui::selectionBackgroundColor(map_color(options->color));
            break;
        case OMNI_FTXUI_DECORATOR_SELECTION_FOREGROUND_COLOR:
            element->element |= ftxui::selectionForegroundColor(map_color(options->color));
            break;
        default:
            return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "element_apply_decorator: unknown decorator");
    }
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}
