module lisp;

import std::io;
import std::core::mem;

fn void ftxui_record_result(ZString name, bool ok, int* pass, int* fail) {
    if (ok) {
        io::printfn("[PASS] %s", name);
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

fn void run_advanced_ffi_system_surface_tests(Interp* interp, int* pass, int* fail) {
    // === FFI TESTS (define [ffi lib] / define [ffi λ]) ===
    // Library declaration
    setup(interp, "(define [ffi lib] libc \"libc.so.6\")");
    test_tag(interp, "ffi lib handle", "libc", FFI_HANDLE, pass, fail);
    test_truthy(interp, "foreign-describe uses array capabilities metadata",
        "(let (m (foreign-describe libc)) "
        "  (and (= (ref m 'type) 'ForeignHandle) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'library) "
        "       (= (ref m 'name) \"libc.so.6\") "
        "       (array? (ref m 'capabilities))"
        "       (= (length (ref m 'capabilities)) 3)"
        "       (= (ref (ref m 'capabilities) 0) 'load)"
        "       (= (ref (ref m 'capabilities) 1) 'resolve)"
        "       (= (ref (ref m 'capabilities) 2) 'reflect)))",
        pass, fail);
    test_error_contains(interp, "foreign-release rejects non-releasable library handle",
        "(foreign-release libc)",
        "handle is not releasable",
        pass, fail);
    test_error_contains(interp, "ffi lib rejects empty path",
        "(define [ffi lib] ffi-empty-path \"\")",
        "path must be a non-empty string shorter than 256 bytes",
        pass, fail);
    {
        bool saved_force = g_env_force_barrier_promote_failure;
        g_env_force_barrier_promote_failure = true;
        test_error_contains(interp, "ffi lib registration failure releases constructed wrapper",
            "(define [ffi lib] ffi-lib-fail \"libc.so.6\")",
            "ffi lib: out of memory",
            pass, fail);
        g_env_force_barrier_promote_failure = saved_force;
    }
    setup(interp, "(define [ffi lib] libc-recover \"libc.so.6\")");
    test_tag(interp, "ffi lib registration still works after failed wrapper release",
        "libc-recover", FFI_HANDLE, pass, fail);

    // Shared dlopen registry: two handles for the same path share the
    // same underlying dlopen handle.
    setup(interp, "(define [ffi lib] libc-shared-a \"libc.so.6\")");
    setup(interp, "(define [ffi lib] libc-shared-b \"libc.so.6\")");
    {
        Value* a_val = interp.global_env.lookup(interp.symbols.intern("libc-shared-a"));
        Value* b_val = interp.global_env.lookup(interp.symbols.intern("libc-shared-b"));
        if (a_val != null && a_val.tag == FFI_HANDLE &&
            b_val != null && b_val.tag == FFI_HANDLE &&
            a_val.ffi_val != null && b_val.ffi_val != null &&
            a_val.ffi_val.lib_handle == b_val.ffi_val.lib_handle) {
            io::printn("[PASS] shared dlopen registry");
            (*pass)++;
        } else {
            io::printn("[FAIL] shared dlopen registry: libc-shared-a and libc-shared-b do not share handle");
            (*fail)++;
        }
    }
    {
        char[256] too_long_path;
        for (usz i = 0; i < too_long_path.len; i++) too_long_path[i] = 'a';
        void* rejected = ffi_dlopen_registry_acquire(too_long_path[0:too_long_path.len]);
        if (rejected == null) {
            io::printn("[PASS] shared dlopen registry rejects overlong non-NUL path");
            (*pass)++;
        } else {
            ffi_dlopen_registry_release(too_long_path[0:too_long_path.len]);
            io::printn("[FAIL] shared dlopen registry rejects overlong non-NUL path");
            (*fail)++;
        }
    }

    // Function bindings — integer functions
    setup(interp, "(define [ffi λ libc] (strlen (^String s)) ^Integer)");
    test_eq(interp, "ffi λ strlen", "(strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi λ strlen empty", "(strlen \"\")", 0, pass, fail);
    test_truthy(interp, "foreign-describe FFI function signature metadata",
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (length (ref m 'parameters)) 1) "
        "       (= (ref (ref (ref m 'parameters) 0) 'type) 'String) "
        "       (= (ref (ref (ref m 'parameters) 0) 'abi-type) 'string) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (= (ref (ref m 'returns) 'abi-type) 'int) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect)))",
        pass, fail);
    test_truthy(interp, "foreign-describe FFI function signature metadata after call",
        "(block "
        "(strlen \"hello\") "
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (length (ref m 'parameters)) 1) "
        "       (= (ref (ref (ref m 'parameters) 0) 'type) 'String) "
        "       (= (ref (ref (ref m 'parameters) 0) 'abi-type) 'string) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (= (ref (ref m 'returns) 'abi-type) 'int) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect))))",
        pass, fail);

    setup(interp,
        "(define [ffi module] libc-grouped \"libc.so.6\" "
        "  (strlen (^String s)) ^Integer "
        "  (abs (^Integer n)) ^Integer "
        "  (fopen (^String path) (^String mode)) ^{'name File 'ownership owned 'finalizer fclose} "
        "  (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)");
    test_eq(interp, "ffi module grouped strlen", "(strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi module grouped abs", "(abs -7)", 7, pass, fail);
    test_error_contains(interp, "ffi module grouped fclose rejects nil for non-null handle",
        "(fclose nil)",
        "expected non-null ForeignHandle",
        pass, fail);
    test_truthy(interp, "ffi module grouped foreign-describe callable metadata",
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect)))",
        pass, fail);
    test_truthy(interp, "ffi module grouped ForeignHandle return metadata",
        "(let (r (ref (foreign-describe fopen) 'returns)) "
        "  (and (= (ref r 'type) 'ForeignHandle) "
        "       (= (ref r 'name) 'File) "
        "       (= (ref r 'ownership) 'owned) "
        "       (= (ref r 'finalizer) 'fclose)))",
        pass, fail);

    setup(interp, "(define [ffi λ libc] (abs (^Integer n)) ^Integer)");
    test_eq(interp, "ffi λ abs", "(abs -42)", 42, pass, fail);
    test_eq(interp, "ffi λ abs zero", "(abs 0)", 0, pass, fail);

    setup(interp, "(define [ffi λ libc] (atoi (^String s)) ^Integer)");
    test_eq(interp, "ffi λ atoi", "(atoi \"12345\")", 12345, pass, fail);
    test_eq(interp, "ffi λ atoi neg", "(atoi \"-99\")", -99, pass, fail);

    // Zero-arg function
    setup(interp, "(define [ffi λ libc] (getpid) ^Integer)");
    test_gt(interp, "ffi λ getpid", "(getpid)", 0, pass, fail);

    // Math library — Float64 functions
    setup(interp, "(define [ffi lib] libm \"libm.so.6\")");
    setup(interp, "(define [ffi λ libm] (sqrt (^Float64 x)) ^Float64)");
    test_eq_double(interp, "ffi λ sqrt", "(sqrt 4.0)", 2.0, pass, fail);
    test_eq_double(interp, "ffi λ sqrt 9", "(sqrt 9.0)", 3.0, pass, fail);

    setup(interp, "(define [ffi λ libm] (sinf (^Float32 x)) ^Float32)");
    test_truthy(interp, "ffi λ sinf returns Float32 payload",
        "(let (r (sinf (Float32 1.5707964))) (and (= (type-of r) 'Float32) (> r (Float32 0.999)) (< r (Float32 1.001))))",
        pass, fail);

    setup(interp, "(define [ffi λ libm] (pow (^Float64 x) (^Float64 y)) ^Float64)");
    test_eq_double(interp, "ffi λ pow", "(pow 2.0 10.0)", 1024.0, pass, fail);

    // Mixed int/Float64 (libffi enables this — old FFI couldn't)
    setup(interp, "(define [ffi λ libm] (ldexp (^Float64 x) (^Integer exp)) ^Float64)");
    test_eq_double(interp, "ffi λ ldexp", "(ldexp 1.0 10)", 1024.0, pass, fail);

    setup(interp, "(define [ffi λ libc] (strtol (^String s) (^ForeignHandle endptr) (^Integer base)) ^Integer)");
    test_eq(interp, "ffi λ strtol mixed scalar foreign handle", "(strtol \"12345\" nil 10)", 12345, pass, fail);

    setup(interp, "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^String)");
    test_str_val(interp, "ffi λ string return copies C string", "(strchr \"hello\" 101)", "ello", pass, fail);
    test_nil(interp, "ffi λ string return null maps to nil", "(strchr \"hello\" 120)", pass, fail);

    // Void return (canonical ForeignHandle annotation)
    setup(interp, "(define [ffi λ libc] (free (^ForeignHandle p)))");
    test_truthy(interp, "ffi λ free null (^ForeignHandle)", "(= (type-of (free nil)) 'Void)", pass, fail);
    test_error_contains(interp, "ffi λ free rejects integer address (^ForeignHandle)",
        "(free 0)",
        "argument 1 expected ForeignHandle, got Integer", pass, fail);

    // Error cases
    test_error_contains(interp, "ffi λ strlen rejects non-string arg",
        "(strlen 123)",
        "argument 1 expected String, got Integer", pass, fail);
    test_error_contains(interp, "ffi λ abs rejects boolean arg",
        "(abs false)",
        "argument 1 expected Integer, got Boolean", pass, fail);
    test_error_contains(interp, "ffi λ sqrt rejects boolean arg",
        "(sqrt false)",
        "argument 1 expected Float64, got Boolean", pass, fail);
    test_error_contains(interp, "ffi λ free rejects string arg",
        "(free \"hello\")",
        "argument 1 expected ForeignHandle, got String", pass, fail);
    test_error_contains(interp, "ffi λ exact arity rejects extra args",
        "(getpid 1)",
        "arity mismatch: expected 0 argument(s), got 1", pass, fail);
    test_error_contains(interp, "ffi lib bad",
        "(define [ffi lib] bad-lib \"nonexistent_xyz.so\")",
        "dlopen failed for", pass, fail);
    test_error_contains(interp, "ffi lib bad preserves path cause",
        "(define [ffi lib] bad-lib2 \"nonexistent_xyz.so\")",
        "nonexistent_xyz.so", pass, fail);
    setup(interp, "(define [ffi λ libc] (definitely_missing_ffi_symbol (^Integer n)) ^Integer)");
    test_error_contains(interp, "ffi λ missing symbol cause",
        "(definitely_missing_ffi_symbol 1)",
        "dlsym failed for", pass, fail);
    test_error_contains(interp, "ffi λ missing symbol preserves loader cause",
        "(definitely_missing_ffi_symbol 1)",
        "undefined symbol", pass, fail);
    test_error_contains(interp, "ffi λ unsupported parameter annotation",
        "(block (define [ffi lib] bad-ffi-lib \"libc.so.6\") (define [ffi λ bad-ffi-lib] (strlen (^Symbol s)) ^Integer))",
        "unsupported parameter type annotation '^Symbol'", pass, fail);
    test_error_contains(interp, "ffi λ legacy Pointer annotation is rejected",
        "(block (define [ffi lib] bad-ffi-lib3 \"libc.so.6\") (define [ffi λ bad-ffi-lib3] (free (^Pointer p))))",
        "unsupported parameter type annotation '^Pointer'", pass, fail);
    test_error_contains(interp, "ffi λ unsupported return annotation",
        "(block (define [ffi lib] bad-ffi-lib2 \"libc.so.6\") (define [ffi λ bad-ffi-lib2] (strlen (^String s)) ^Symbol))",
        "unsupported return type annotation '^Symbol'", pass, fail);
    setup(interp, "(define [ffi λ libc] (malloc (^Integer size)) ^ForeignHandle)");
    setup(interp, "(define [ffi λ libc variadic] (snprintf (^ForeignHandle buf) (^Integer size) (^String fmt)) ^Integer)");
    test_gt(interp, "ffi λ variadic snprintf",
        "(let (buf (malloc 64)) (snprintf buf 64 \"hello %s %d\" \"world\" 42))",
        0, pass, fail);
    test_gt(interp, "ffi-async-call variadic snprintf",
        "(let (buf (malloc 64)) (ffi-async-call snprintf buf 64 \"hello %s %d\" \"world\" 42))",
        0, pass, fail);
    test_error_contains(interp, "ffi λ Boolean requires real booleans",
        "(block (define [ffi λ libc] (abs (^Boolean flag)) ^Integer) (abs 1))",
        "argument 1 expected Boolean, got Integer", pass, fail);
    test_error_contains(interp, "ffi module grouped rejects missing return annotation",
        "(define [ffi module] bad-grouped \"libc.so.6\" (strlen (^String s)))",
        "expected return type annotation after [ffi module] function signature", pass, fail);

    if (omni_ffi_call_null_arg_vectors_guard_for_tests() == -1) {
        io::printn("[PASS] ffi native call rejects null arg vectors");
        (*pass)++;
    } else {
        io::printn("[FAIL] ffi native call rejects null arg vectors");
        (*fail)++;
    }
    if (omni_ffi_call_var_null_arg_vectors_guard_for_tests() == -1) {
        io::printn("[PASS] ffi native variadic call rejects null arg vectors");
        (*pass)++;
    } else {
        io::printn("[FAIL] ffi native variadic call rejects null arg vectors");
        (*fail)++;
    }
    if (omni_ffi_closure_alloc_failure_clears_outputs_for_tests() == 0) {
        io::printn("[PASS] ffi native closure failure clears outputs");
        (*pass)++;
    } else {
        io::printn("[FAIL] ffi native closure failure clears outputs");
        (*fail)++;
    }

    // === FFI CALLBACK TESTS ===
    setup(interp, "(define cb (ffi-callback (lambda (a b) (- a b)) ['Integer 'Integer] 'Integer))");
    test_tag(interp, "ffi-callback list form creates ForeignHandle", "cb", FFI_HANDLE, pass, fail);

    setup(interp, "(define cb2 (ffi-callback (lambda (a b) (+ a b)) 'Integer 'Integer 'Integer))");
    test_tag(interp, "ffi-callback variadic form creates ForeignHandle", "cb2", FFI_HANDLE, pass, fail);

    test_error_contains(interp, "ffi-callback rejects unsupported parameter type symbol",
        "(ffi-callback (lambda (x) x) ['UnknownType] 'Integer)",
        "unsupported parameter type", pass, fail);

    test_error_contains(interp, "ffi-callback rejects unsupported return type symbol",
        "(ffi-callback (lambda (x) x) ['Integer] 'UnknownType)",
        "unsupported return type", pass, fail);

    test_error_contains(interp, "ffi-callback rejects non-callable first arg",
        "(ffi-callback 123 ['Integer] 'Integer)",
        "first arg must be a callable", pass, fail);

    // End-to-end: invoke the callback through C test helper
    {
        EvalResult ri = run("(ffi-callback (lambda (a b) (- a b)) ['Integer 'Integer] 'Integer)", interp);
        if (ri.error.has_error || ri.value == null || ri.value.tag != FFI_HANDLE || ri.value.ffi_val == null) {
            io::printfn("[FAIL] ffi-callback end-to-end: failed to create callback");
            (*fail)++;
        } else {
            void* code_ptr = ri.value.ffi_val.lib_handle;
            int result = omni_ffi_test_callback_int_int(code_ptr, 10, 3);
            if (result == 7) {
                io::printfn("[PASS] ffi-callback end-to-end invocation");
                (*pass)++;
            } else {
                io::printfn("[FAIL] ffi-callback end-to-end invocation: expected 7, got %d", result);
                (*fail)++;
            }
        }
    }

    // Regression: foreign-release on ffi-callback must not segfault
    {
        EvalResult ri = run("(let (cb (ffi-callback (lambda (a b) (- a b)) ['Integer 'Integer] 'Integer)) (foreign-release cb))", interp);
        if (ri.error.has_error) {
            io::printfn("[FAIL] ffi-callback foreign-release: got error: %s", ri.error.message);
            (*fail)++;
        } else {
            io::printfn("[PASS] ffi-callback foreign-release");
            (*pass)++;
        }
    }

    // === ASYNC FFI CALL TESTS ===
    test_eq(interp, "ffi-async-call strlen",
        "(ffi-async-call strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi-async-call strlen empty",
        "(ffi-async-call strlen \"\")", 0, pass, fail);
    test_eq(interp, "ffi-async-call atoi neg",
        "(ffi-async-call atoi \"-42\")", -42, pass, fail);
    test_eq_double(interp, "ffi-async-call sqrt",
        "(ffi-async-call sqrt 4.0)", 2.0, pass, fail);
    test_truthy(interp, "ffi-async-call sinf Float32 payload",
        "(let (r (ffi-async-call sinf (Float32 1.5707964))) (and (> r 0.999) (< r 1.001)))",
        pass, fail);
    test_eq_double(interp, "ffi-async-call pow",
        "(ffi-async-call pow 2.0 10.0)", 1024.0, pass, fail);
    test_eq(interp, "ffi-async-call atoi",
        "(ffi-async-call atoi \"9876\")", 9876, pass, fail);
    test_str_val(interp, "ffi-async-call string return copies C string",
        "(ffi-async-call strchr \"hello\" 101)", "ello", pass, fail);
    test_nil(interp, "ffi-async-call string return null maps to nil",
        "(ffi-async-call strchr \"hello\" 120)", pass, fail);
    test_error_contains(interp, "ffi-async-call rejects pointer-like return",
        "(ffi-async-call malloc 16)",
        "pointer-like return types are not supported", pass, fail);
    test_error_contains(interp, "ffi-async-call rejects non-bound first arg",
        "(ffi-async-call 123 456)",
        "expected an FFI-bound function", pass, fail);
    test_error_contains(interp, "ffi-async-call rejects string first arg",
        "(ffi-async-call \"hello\" 1)",
        "expected an FFI-bound function", pass, fail);
    test_error_contains(interp, "ffi-async-call arity mismatch propagates",
        "(ffi-async-call strlen \"hello\" \"extra\")",
        "ffi: expected 1 args, got 2", pass, fail);
    test_error_contains(interp, "ffi-async-call type mismatch propagates",
        "(ffi-async-call strlen 123)",
        "expected String, got Integer", pass, fail);

    // === SYSTEM PRIMITIVES TESTS ===
    // random — returns Float64 in [0,1)
    test_truthy(interp, "random range", "(let (r (random)) (and (>= r 0.0) (< r 1.0)))", pass, fail);
    test_truthy(interp, "random is Float64", "(float64? (random))", pass, fail);
    // random-int — returns int in [0,n)
    test_truthy(interp, "random-int range", "(let (r (random-int 10)) (and (>= r 0) (< r 10)))", pass, fail);
    test_truthy(interp, "random-int is int", "(int? (random-int 100))", pass, fail);
    // time — returns epoch seconds (should be > 2020-01-01)
    test_gt(interp, "time epoch", "(time)", 1577836800, pass, fail);
    // time-ms — returns epoch milliseconds (> time * 1000)
    test_gt(interp, "time-ms epoch", "(time-ms)", 1577836800000, pass, fail);
    // getenv — HOME should exist, nonexistent returns nil
    test_str(interp, "getenv HOME", "(getenv \"HOME\")", pass, fail);
    test_nil(interp, "getenv missing", "(getenv \"OMNI_NONEXISTENT_VAR_XYZ\")", pass, fail);
    // shell — captures stdout
    test_str_val(interp, "shell echo", "(shell \"echo hello\")", "hello", pass, fail);
    // sleep — returns void (tiny sleep to not slow tests)
    test_tag_interp(interp, "sleep tiny", "(sleep 0.001)", VOID, pass, fail);

    run_advanced_uv_timer_callback_register_invoke_test(interp, pass, fail);
    run_advanced_uv_timer_callback_unhandle_idempotence_test(interp, pass, fail);
    run_advanced_uv_timer_callback_scope_release_test(interp, pass, fail);
    run_advanced_uv_timer_callback_wrapper_failure_scope_release_test(interp, pass, fail);
    run_advanced_uv_timer_callback_finalizer_scope_release_test(interp, pass, fail);
    run_advanced_uv_timer_callback_handle_isolation_test(interp, pass, fail);
    run_advanced_ftxui_context_screen_lifecycle_test(pass, fail);
    run_advanced_ftxui_table_canvas_conversion_test(pass, fail);
    run_advanced_ftxui_component_callback_delivery_test(pass, fail);
}
