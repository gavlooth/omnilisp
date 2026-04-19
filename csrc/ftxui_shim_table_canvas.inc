omni_ftxui_status omni_ftxui_table_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_table_create_options* options,
    omni_ftxui_table_t** out_table
) {
    if (options == nullptr || out_table == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_create: null argument");
    }
    if (!validate_sized_abi(options->size, options->abi_version, sizeof(omni_ftxui_table_create_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_create: invalid option ABI");
    }
    if (options->rows == 0 || options->cols == 0 || options->cells == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_create: invalid table dimensions");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_create: FTXUI backend unavailable");
#else
    return status_try(context, "table_create: out of memory", "table_create: exception", [&]() -> omni_ftxui_status {
    auto* table = new (std::nothrow) omni_ftxui_table_t();
    if (table == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "table_create: allocation failed");

    table->data.resize(options->rows);
    size_t idx = 0;
    for (size_t r = 0; r < options->rows; ++r) {
        table->data[r].reserve(options->cols);
        for (size_t c = 0; c < options->cols; ++c) {
            table->data[r].push_back(options->cells[idx] ? options->cells[idx] : "");
            ++idx;
        }
    }
    table->table = std::make_unique<ftxui::Table>(table->data);
    *out_table = table;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

void omni_ftxui_table_destroy(omni_ftxui_table_t* table) {
    delete table;
}

omni_ftxui_status omni_ftxui_table_select_all(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    omni_ftxui_border_style border
) {
    if (table == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_all: null table");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)border;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_select_all: FTXUI backend unavailable");
#else
    return status_try(context, "table_select_all: out of memory", "table_select_all: exception", [&]() -> omni_ftxui_status {
    table->table->SelectAll().Border(map_border_style(border));
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_table_select_row(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    size_t row,
    omni_ftxui_border_style border
) {
    if (table == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_row: null table");
    if (row > static_cast<size_t>(std::numeric_limits<int>::max())) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_row: row out of int range");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)row;
    (void)border;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_select_row: FTXUI backend unavailable");
#else
    return status_try(context, "table_select_row: out of memory", "table_select_row: exception", [&]() -> omni_ftxui_status {
    table->table->SelectRow(static_cast<int>(row)).Border(map_border_style(border));
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_table_select_column(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    size_t column,
    omni_ftxui_border_style border
) {
    if (table == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_column: null table");
    if (column > static_cast<size_t>(std::numeric_limits<int>::max())) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_column: column out of int range");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)column;
    (void)border;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_select_column: FTXUI backend unavailable");
#else
    return status_try(context, "table_select_column: out of memory", "table_select_column: exception", [&]() -> omni_ftxui_status {
    table->table->SelectColumn(static_cast<int>(column)).Border(map_border_style(border));
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_table_select_cell(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    size_t row,
    size_t column,
    omni_ftxui_border_style border
) {
    if (table == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_cell: null table");
    if (row > static_cast<size_t>(std::numeric_limits<int>::max()) ||
        column > static_cast<size_t>(std::numeric_limits<int>::max())) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_select_cell: row/column out of int range");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)row;
    (void)column;
    (void)border;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_select_cell: FTXUI backend unavailable");
#else
    return status_try(context, "table_select_cell: out of memory", "table_select_cell: exception", [&]() -> omni_ftxui_status {
    table->table->SelectCell(static_cast<int>(row), static_cast<int>(column)).Border(map_border_style(border));
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_table_render(
    omni_ftxui_context_t* context,
    omni_ftxui_table_t* table,
    omni_ftxui_element_t** out_element
) {
    if (table == nullptr || out_element == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "table_render: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "table_render: FTXUI backend unavailable");
#else
    return status_try(context, "table_render: out of memory", "table_render: exception", [&]() -> omni_ftxui_status {
    auto* out = make_element(table->table->Render());
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "table_render: allocation failed");
    *out_element = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_canvas_create(
    omni_ftxui_context_t* context,
    const omni_ftxui_canvas_create_options* options,
    omni_ftxui_canvas_t** out_canvas
) {
    if (options == nullptr || out_canvas == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_create: null argument");
    }
    if (!validate_sized_abi(options->size, options->abi_version, sizeof(omni_ftxui_canvas_create_options))) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_create: invalid option ABI");
    }
    if (options->width <= 0 || options->height <= 0) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_create: invalid dimensions");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_create: FTXUI backend unavailable");
#else
    return status_try(context, "canvas_create: out of memory", "canvas_create: exception", [&]() -> omni_ftxui_status {
    auto* out = new (std::nothrow) omni_ftxui_canvas_t(options->width, options->height);
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "canvas_create: allocation failed");
    *out_canvas = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

void omni_ftxui_canvas_destroy(omni_ftxui_canvas_t* canvas) {
    delete canvas;
}

omni_ftxui_status omni_ftxui_canvas_draw_text(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x,
    int32_t y,
    const char* text
) {
    if (canvas == nullptr || text == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_draw_text: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    (void)x;
    (void)y;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_draw_text: FTXUI backend unavailable");
#else
    return status_try(context, "canvas_draw_text: out of memory", "canvas_draw_text: exception", [&]() -> omni_ftxui_status {
    canvas->canvas->DrawText(x, y, text);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_canvas_draw_point_on(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x,
    int32_t y
) {
    if (canvas == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_draw_point_on: null canvas");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)x;
    (void)y;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_draw_point_on: FTXUI backend unavailable");
#else
    return status_try(context, "canvas_draw_point_on: out of memory", "canvas_draw_point_on: exception", [&]() -> omni_ftxui_status {
    canvas->canvas->DrawPointOn(x, y);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_canvas_draw_point_off(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x,
    int32_t y
) {
    if (canvas == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_draw_point_off: null canvas");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)x;
    (void)y;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_draw_point_off: FTXUI backend unavailable");
#else
    return status_try(context, "canvas_draw_point_off: out of memory", "canvas_draw_point_off: exception", [&]() -> omni_ftxui_status {
    canvas->canvas->DrawPointOff(x, y);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_canvas_draw_line(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    int32_t x1,
    int32_t y1,
    int32_t x2,
    int32_t y2
) {
    if (canvas == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_draw_line: null canvas");
#if !OMNI_FTXUI_HAS_BACKEND
    (void)x1;
    (void)y1;
    (void)x2;
    (void)y2;
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_draw_line: FTXUI backend unavailable");
#else
    return status_try(context, "canvas_draw_line: out of memory", "canvas_draw_line: exception", [&]() -> omni_ftxui_status {
    canvas->canvas->DrawPointLine(x1, y1, x2, y2);
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}

omni_ftxui_status omni_ftxui_canvas_render(
    omni_ftxui_context_t* context,
    omni_ftxui_canvas_t* canvas,
    omni_ftxui_element_t** out_element
) {
    if (canvas == nullptr || out_element == nullptr) {
        return ctx_fail(context, OMNI_FTXUI_STATUS_INVALID_ARGUMENT, "canvas_render: null argument");
    }
#if !OMNI_FTXUI_HAS_BACKEND
    return ctx_fail(context, OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE, "canvas_render: FTXUI backend unavailable");
#else
    return status_try(context, "canvas_render: out of memory", "canvas_render: exception", [&]() -> omni_ftxui_status {
    auto* out = make_element(ftxui::canvas(*canvas->canvas));
    if (out == nullptr) return ctx_fail(context, OMNI_FTXUI_STATUS_OUT_OF_MEMORY, "canvas_render: allocation failed");
    *out_element = out;
    ctx_clear(context);
    return OMNI_FTXUI_STATUS_OK;
    });
#endif
}
