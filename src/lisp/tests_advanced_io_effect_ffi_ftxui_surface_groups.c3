module lisp;

import std::io;

fn void ftxui_record_result(ZString name, bool ok, int* pass, int* fail) {
    if (ok) {
        if (!test_quiet_enabled()) {
            io::printfn("[PASS] %s", name);
        }
        (*pass)++;
    } else {
        io::printfn("[FAIL] %s", name);
        (*fail)++;
    }
}

fn OmniFtxuiDimension ftxui_test_fit_dimension() {
    return {
        .size = OmniFtxuiDimension.sizeof,
        .abi_version = OMNI_FTXUI_ABI_VERSION,
        .kind = OMNI_FTXUI_DIMENSION_FIT,
        .value = 0
    };
}

fn OmniFtxuiEventSpec ftxui_test_escape_event_spec() {
    return {
        .size = OmniFtxuiEventSpec.sizeof,
        .abi_version = OMNI_FTXUI_ABI_VERSION,
        .kind = OMNI_FTXUI_EVENT_ESCAPE,
        .text = null
    };
}

fn OmniFtxuiEventSpec ftxui_test_return_event_spec() {
    return {
        .size = OmniFtxuiEventSpec.sizeof,
        .abi_version = OMNI_FTXUI_ABI_VERSION,
        .kind = OMNI_FTXUI_EVENT_RETURN,
        .text = null
    };
}

fn int ftxui_test_event_callback(void* user_data, OmniFtxuiContext* context, OmniFtxuiEventHandle* event) {
    (void)context;
    (void)event;
    if (user_data != null) {
        int* count = (int*)user_data;
        (*count)++;
    }
    return 1;
}

fn void ftxui_test_action_callback(void* user_data) {
    if (user_data != null) {
        int* count = (int*)user_data;
        (*count)++;
    }
}

fn void run_advanced_ftxui_context_screen_lifecycle_test(int* pass, int* fail) {
    OmniFtxuiContext* context = null;
    OmniFtxuiStatus status = omni_ftxui_context_create(&context);
    ftxui_record_result("ftxui context create", status == OMNI_FTXUI_STATUS_OK && context != null, pass, fail);
    if (context == null) return;
    defer omni_ftxui_context_destroy(context);

    OmniFtxuiEventHandle* bad_event = null;
    status = omni_ftxui_event_create(context, null, &bad_event);
    ftxui_record_result(
        "ftxui context captures invalid argument status",
        status == OMNI_FTXUI_STATUS_INVALID_ARGUMENT &&
            omni_ftxui_context_last_error_code(context) == OMNI_FTXUI_STATUS_INVALID_ARGUMENT,
        pass,
        fail
    );
    omni_ftxui_context_clear_error(context);
    ftxui_record_result(
        "ftxui context clear error",
        omni_ftxui_context_last_error_code(context) == OMNI_FTXUI_STATUS_OK,
        pass,
        fail
    );

    OmniFtxuiScreenCreateOptions options = {};
    options.size = OmniFtxuiScreenCreateOptions.sizeof;
    options.abi_version = OMNI_FTXUI_ABI_VERSION;
    options.mode = OMNI_FTXUI_SCREEN_TERMINAL_OUTPUT;
    options.track_mouse = false;
    options.handle_piped_input = false;
    options.width = ftxui_test_fit_dimension();
    options.height = ftxui_test_fit_dimension();

    OmniFtxuiScreenCreateOptions piped_options = options;
    piped_options.handle_piped_input = true;
    OmniFtxuiScreen* piped_screen = null;
    status = omni_ftxui_screen_create(context, &piped_options, &piped_screen);
    if (omni_ftxui_backend_available() != 0) {
        ftxui_record_result(
            "ftxui screen create rejects piped input",
            status == OMNI_FTXUI_STATUS_NOT_SUPPORTED && piped_screen == null,
            pass,
            fail
        );
    }
    if (piped_screen != null) omni_ftxui_screen_destroy(piped_screen);

    OmniFtxuiScreen* screen = null;
    status = omni_ftxui_screen_create(context, &options, &screen);
    if (omni_ftxui_backend_available() == 0) {
        ftxui_record_result(
            "ftxui screen create fails closed without backend",
            status == OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE && screen == null,
            pass,
            fail
        );
        return;
    }
    ftxui_record_result("ftxui screen create", status == OMNI_FTXUI_STATUS_OK && screen != null, pass, fail);
    if (screen == null) return;
    defer omni_ftxui_screen_destroy(screen);

    status = omni_ftxui_screen_set_track_mouse(context, screen, true);
    ftxui_record_result("ftxui screen set track mouse", status == OMNI_FTXUI_STATUS_OK, pass, fail);
    status = omni_ftxui_screen_set_handle_piped_input(context, screen, false);
    ftxui_record_result("ftxui screen accepts disabled piped input", status == OMNI_FTXUI_STATUS_OK, pass, fail);
    status = omni_ftxui_screen_set_handle_piped_input(context, screen, true);
    ftxui_record_result("ftxui screen rejects enabled piped input", status == OMNI_FTXUI_STATUS_NOT_SUPPORTED, pass, fail);
    status = omni_ftxui_screen_request_animation_frame(context, screen);
    ftxui_record_result("ftxui screen request animation frame", status == OMNI_FTXUI_STATUS_OK, pass, fail);

    OmniFtxuiComponentCreateOptions component_options = {};
    component_options.size = OmniFtxuiComponentCreateOptions.sizeof;
    component_options.abi_version = OMNI_FTXUI_ABI_VERSION;
    component_options.kind = OMNI_FTXUI_COMPONENT_BUTTON;
    component_options.label = "message";

    OmniFtxuiComponent* message_button = null;
    status = omni_ftxui_component_create(context, &component_options, null, 0, &message_button);
    ftxui_record_result("ftxui message button component create", status == OMNI_FTXUI_STATUS_OK && message_button != null, pass, fail);
    if (message_button != null) {
        defer omni_ftxui_component_destroy(message_button);

        OmniFtxuiComponent* message_wrapped = null;
        status = omni_ftxui_component_wrap_message_action(context, screen, message_button, "increment", &message_wrapped);
        ftxui_record_result("ftxui message action wrapper create", status == OMNI_FTXUI_STATUS_OK && message_wrapped != null, pass, fail);
        if (message_wrapped != null) {
            defer omni_ftxui_component_destroy(message_wrapped);
            OmniFtxuiEventSpec return_spec = ftxui_test_return_event_spec();
            OmniFtxuiEventHandle* return_event = null;
            status = omni_ftxui_event_create(context, &return_spec, &return_event);
            ftxui_record_result("ftxui message action return event create", status == OMNI_FTXUI_STATUS_OK && return_event != null, pass, fail);
            if (return_event != null) {
                defer omni_ftxui_event_destroy(return_event);
                bool handled = false;
                status = omni_ftxui_component_handle_event(context, message_wrapped, return_event, &handled);
                ftxui_record_result(
                    "ftxui message action handles activation",
                    status == OMNI_FTXUI_STATUS_OK && handled,
                    pass,
                    fail
                );
            }
        }
    }

    OmniFtxuiEventSpec event_spec = ftxui_test_escape_event_spec();
    OmniFtxuiEventHandle* event = null;
    status = omni_ftxui_event_create(context, &event_spec, &event);
    ftxui_record_result("ftxui event create", status == OMNI_FTXUI_STATUS_OK && event != null, pass, fail);
    if (event != null) {
        defer omni_ftxui_event_destroy(event);
        status = omni_ftxui_screen_post_event(context, screen, event);
        ftxui_record_result("ftxui screen post event", status == OMNI_FTXUI_STATUS_OK, pass, fail);
    }

    status = omni_ftxui_screen_exit(context, screen);
    ftxui_record_result("ftxui screen exit", status == OMNI_FTXUI_STATUS_OK, pass, fail);
}

fn void run_advanced_ftxui_table_canvas_conversion_test(int* pass, int* fail) {
    OmniFtxuiContext* context = null;
    OmniFtxuiStatus status = omni_ftxui_context_create(&context);
    ftxui_record_result("ftxui table/canvas context create", status == OMNI_FTXUI_STATUS_OK && context != null, pass, fail);
    if (context == null) return;
    defer omni_ftxui_context_destroy(context);

    char*[4] cells = { null, null, null, null };
    OmniFtxuiTableCreateOptions table_options = {};
    table_options.size = OmniFtxuiTableCreateOptions.sizeof;
    table_options.abi_version = OMNI_FTXUI_ABI_VERSION;
    table_options.rows = 2;
    table_options.cols = 2;
    table_options.cells = &cells[0];

    OmniFtxuiTable* table = null;
    status = omni_ftxui_table_create(context, &table_options, &table);
    if (omni_ftxui_backend_available() == 0) {
        ftxui_record_result(
            "ftxui table create fails closed without backend",
            status == OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE && table == null,
            pass,
            fail
        );
    } else {
        ftxui_record_result("ftxui table create", status == OMNI_FTXUI_STATUS_OK && table != null, pass, fail);
    }

    if (table != null) {
        defer omni_ftxui_table_destroy(table);
        status = omni_ftxui_table_select_all(context, table, OMNI_FTXUI_BORDER_LIGHT);
        ftxui_record_result("ftxui table select all", status == OMNI_FTXUI_STATUS_OK, pass, fail);
        status = omni_ftxui_table_select_row(context, table, 1, OMNI_FTXUI_BORDER_HEAVY);
        ftxui_record_result("ftxui table select row", status == OMNI_FTXUI_STATUS_OK, pass, fail);
        status = omni_ftxui_table_select_column(context, table, 1, OMNI_FTXUI_BORDER_ROUNDED);
        ftxui_record_result("ftxui table select column", status == OMNI_FTXUI_STATUS_OK, pass, fail);
        status = omni_ftxui_table_select_cell(context, table, 1, 1, OMNI_FTXUI_BORDER_DOUBLE);
        ftxui_record_result("ftxui table select cell", status == OMNI_FTXUI_STATUS_OK, pass, fail);

        OmniFtxuiElement* table_element = null;
        status = omni_ftxui_table_render(context, table, &table_element);
        ftxui_record_result("ftxui table render element", status == OMNI_FTXUI_STATUS_OK && table_element != null, pass, fail);
        if (table_element != null) omni_ftxui_element_destroy(table_element);
    }

    OmniFtxuiCanvasCreateOptions canvas_options = {};
    canvas_options.size = OmniFtxuiCanvasCreateOptions.sizeof;
    canvas_options.abi_version = OMNI_FTXUI_ABI_VERSION;
    canvas_options.width = 8;
    canvas_options.height = 4;

    OmniFtxuiCanvas* canvas = null;
    status = omni_ftxui_canvas_create(context, &canvas_options, &canvas);
    if (omni_ftxui_backend_available() == 0) {
        ftxui_record_result(
            "ftxui canvas create fails closed without backend",
            status == OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE && canvas == null,
            pass,
            fail
        );
        return;
    }

    ftxui_record_result("ftxui canvas create", status == OMNI_FTXUI_STATUS_OK && canvas != null, pass, fail);
    if (canvas == null) return;
    defer omni_ftxui_canvas_destroy(canvas);

    status = omni_ftxui_canvas_draw_point_on(context, canvas, 1, 1);
    ftxui_record_result("ftxui canvas draw point on", status == OMNI_FTXUI_STATUS_OK, pass, fail);
    status = omni_ftxui_canvas_draw_point_off(context, canvas, 1, 1);
    ftxui_record_result("ftxui canvas draw point off", status == OMNI_FTXUI_STATUS_OK, pass, fail);
    status = omni_ftxui_canvas_draw_line(context, canvas, 0, 0, 7, 3);
    ftxui_record_result("ftxui canvas draw line", status == OMNI_FTXUI_STATUS_OK, pass, fail);

    OmniFtxuiElement* canvas_element = null;
    status = omni_ftxui_canvas_render(context, canvas, &canvas_element);
    ftxui_record_result("ftxui canvas render element", status == OMNI_FTXUI_STATUS_OK && canvas_element != null, pass, fail);
    if (canvas_element != null) omni_ftxui_element_destroy(canvas_element);

    double[6] plot_values = { 1.0, 2.0, 3.0, 3.0, 2.0, 1.0 };
    usz[2] plot_lengths = { 3, 3 };
    OmniFtxuiPlotOptions plot_options = {};
    plot_options.size = OmniFtxuiPlotOptions.sizeof;
    plot_options.abi_version = OMNI_FTXUI_ABI_VERSION;
    plot_options.kind = OMNI_FTXUI_PLOT_MULTI_SERIES;
    plot_options.width = 24;
    plot_options.height = 12;
    plot_options.values = &plot_values[0];
    plot_options.value_count = 6;
    plot_options.series_lengths = &plot_lengths[0];
    plot_options.series_count = 2;
    plot_options.marker = OMNI_FTXUI_PLOT_MARKER_POINT;
    OmniFtxuiElement* plot_element = null;
    status = omni_ftxui_element_plot(context, &plot_options, &plot_element);
    ftxui_record_result("ftxui plot render multi-series element", status == OMNI_FTXUI_STATUS_OK && plot_element != null, pass, fail);
    if (plot_element != null) omni_ftxui_element_destroy(plot_element);

    double[4] heat_values = { 1.0, 2.0, 3.0, 4.0 };
    OmniFtxuiPlotOptions heat_options = {};
    heat_options.size = OmniFtxuiPlotOptions.sizeof;
    heat_options.abi_version = OMNI_FTXUI_ABI_VERSION;
    heat_options.kind = OMNI_FTXUI_PLOT_HEATMAP;
    heat_options.width = 16;
    heat_options.height = 8;
    heat_options.values = &heat_values[0];
    heat_options.value_count = 4;
    heat_options.heatmap_rows = 2;
    heat_options.heatmap_cols = 2;
    OmniFtxuiElement* heat_element = null;
    status = omni_ftxui_element_plot(context, &heat_options, &heat_element);
    ftxui_record_result("ftxui plot render heatmap element", status == OMNI_FTXUI_STATUS_OK && heat_element != null, pass, fail);
    if (heat_element != null) omni_ftxui_element_destroy(heat_element);
}

fn void run_advanced_ftxui_component_callback_delivery_test(int* pass, int* fail) {
    OmniFtxuiContext* context = null;
    OmniFtxuiStatus status = omni_ftxui_context_create(&context);
    ftxui_record_result("ftxui callback context create", status == OMNI_FTXUI_STATUS_OK && context != null, pass, fail);
    if (context == null) return;
    defer omni_ftxui_context_destroy(context);

    OmniFtxuiComponentCreateOptions component_options = {};
    component_options.size = OmniFtxuiComponentCreateOptions.sizeof;
    component_options.abi_version = OMNI_FTXUI_ABI_VERSION;
    component_options.kind = OMNI_FTXUI_COMPONENT_BUTTON;

    OmniFtxuiComponent* base = null;
    status = omni_ftxui_component_create(context, &component_options, null, 0, &base);
    if (omni_ftxui_backend_available() == 0) {
        ftxui_record_result(
            "ftxui component create fails closed without backend",
            status == OMNI_FTXUI_STATUS_BACKEND_UNAVAILABLE && base == null,
            pass,
            fail
        );
        return;
    }

    ftxui_record_result("ftxui component create", status == OMNI_FTXUI_STATUS_OK && base != null, pass, fail);
    if (base == null) return;
    defer omni_ftxui_component_destroy(base);

    int callback_count = 0;
    OmniFtxuiCallbackOptions callbacks = {};
    callbacks.size = OmniFtxuiCallbackOptions.sizeof;
    callbacks.abi_version = OMNI_FTXUI_ABI_VERSION;
    callbacks.user_data = (void*)&callback_count;
    callbacks.event_cb = &ftxui_test_event_callback;

    OmniFtxuiComponent* wrapped = null;
    status = omni_ftxui_component_wrap_event_handler(context, base, &callbacks, &wrapped);
    ftxui_record_result("ftxui component wrap event handler", status == OMNI_FTXUI_STATUS_OK && wrapped != null, pass, fail);
    if (wrapped == null) return;
    defer omni_ftxui_component_destroy(wrapped);

    OmniFtxuiEventSpec event_spec = ftxui_test_escape_event_spec();
    OmniFtxuiEventHandle* event = null;
    status = omni_ftxui_event_create(context, &event_spec, &event);
    ftxui_record_result("ftxui callback event create", status == OMNI_FTXUI_STATUS_OK && event != null, pass, fail);
    if (event == null) return;
    defer omni_ftxui_event_destroy(event);

    bool handled = false;
    status = omni_ftxui_component_handle_event(context, wrapped, event, &handled);
    ftxui_record_result(
        "ftxui component handle event delivers callback",
        status == OMNI_FTXUI_STATUS_OK && handled && callback_count == 1,
        pass,
        fail
    );

    OmniFtxuiCallbackOptions missing_action_callbacks = {};
    missing_action_callbacks.size = OmniFtxuiCallbackOptions.sizeof;
    missing_action_callbacks.abi_version = OMNI_FTXUI_ABI_VERSION;
    OmniFtxuiComponent* bad_action_wrapper = null;
    status = omni_ftxui_component_wrap_action(context, base, &missing_action_callbacks, &bad_action_wrapper);
    ftxui_record_result(
        "ftxui component wrap action rejects missing callback",
        status == OMNI_FTXUI_STATUS_INVALID_ARGUMENT && bad_action_wrapper == null,
        pass,
        fail
    );

    int action_count = 0;
    OmniFtxuiCallbackOptions action_callbacks = {};
    action_callbacks.size = OmniFtxuiCallbackOptions.sizeof;
    action_callbacks.abi_version = OMNI_FTXUI_ABI_VERSION;
    action_callbacks.user_data = (void*)&action_count;
    action_callbacks.action_cb = &ftxui_test_action_callback;

    OmniFtxuiComponent* action_wrapped = null;
    status = omni_ftxui_component_wrap_action(context, base, &action_callbacks, &action_wrapped);
    ftxui_record_result("ftxui component wrap action", status == OMNI_FTXUI_STATUS_OK && action_wrapped != null, pass, fail);
    if (action_wrapped == null) return;
    defer omni_ftxui_component_destroy(action_wrapped);

    OmniFtxuiEventSpec return_spec = ftxui_test_return_event_spec();
    OmniFtxuiEventHandle* return_event = null;
    status = omni_ftxui_event_create(context, &return_spec, &return_event);
    ftxui_record_result("ftxui action event create", status == OMNI_FTXUI_STATUS_OK && return_event != null, pass, fail);
    if (return_event == null) return;
    defer omni_ftxui_event_destroy(return_event);

    handled = false;
    status = omni_ftxui_component_handle_event(context, action_wrapped, return_event, &handled);
    ftxui_record_result(
        "ftxui component handle event delivers action callback",
        status == OMNI_FTXUI_STATUS_OK && handled && action_count == 1,
        pass,
        fail
    );
}

fn void run_advanced_ftxui_event_read_result_clear_guard_test(int* pass, int* fail) {
    OmniFtxuiEventReadResult event = {};
    event.size = OmniFtxuiEventReadResult.sizeof;
    event.abi_version = OMNI_FTXUI_ABI_VERSION;
    event.initialized = 0;
    event.text = (char*)1;
    event.text_len = 4;
    event.has_event = true;
    event.dropped_events = 7;

    omni_ftxui_event_read_result_clear(&event);

    ftxui_record_result(
        "ftxui event read result clear ignores unowned text",
        event.size == OmniFtxuiEventReadResult.sizeof &&
            event.abi_version == OMNI_FTXUI_ABI_VERSION &&
            event.initialized != 0 &&
            event.text == null &&
            event.text_len == 0 &&
            !event.has_event &&
            event.dropped_events == 0,
        pass,
        fail
    );
}
