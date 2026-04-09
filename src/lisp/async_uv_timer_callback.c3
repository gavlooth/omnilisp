module lisp;

import std::io;

// (__raw-uv-timer-callback-handle callback) -> uv-timer-callback-handle
fn Value* prim_uv_timer_callback_handle(Value*[] args, Env* env, Interp* interp) {
    Value* callback_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        callback_v = car(args[0]);
    } else if (args.len >= 1) {
        callback_v = args[0];
    } else {
        return io_raise(
            interp,
            "io/uv-timer-callback-handle-expected-callback",
            "uv-timer-callback-handle: expected (uv-timer-callback-handle callback)"
        );
    }
    return make_uv_timer_callback_handle(interp, callback_v);
}

// (__raw-uv-timer-callback-unhandle handle) -> void (idempotent)
fn Value* prim_uv_timer_callback_unhandle(Value*[] args, Env* env, Interp* interp) {
    Value* handle_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        handle_v = car(args[0]);
    } else if (args.len >= 1) {
        handle_v = args[0];
    } else {
        return io_raise(
            interp,
            "io/uv-timer-callback-unhandle-expected-handle",
            "uv-timer-callback-unhandle: expected (uv-timer-callback-unhandle handle)"
        );
    }

    UvTimerCallbackHandle* handle = get_uv_timer_callback_handle(handle_v);
    if (handle == null) {
        return io_raise(
            interp,
            "io/uv-timer-callback-unhandle-invalid-handle",
            "uv-timer-callback-unhandle: invalid callback handle"
        );
    }

    uv_timer_callback_handle_detach_runtime_state(handle);
    return make_void(interp);
}

// (__raw-uv-timer-callback-invoke handle) -> void
fn Value* prim_uv_timer_callback_invoke(Value*[] args, Env* env, Interp* interp) {
    Value* handle_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        handle_v = car(args[0]);
    } else if (args.len >= 1) {
        handle_v = args[0];
    } else {
        return io_raise(
            interp,
            "io/uv-timer-callback-invoke-expected-handle",
            "uv-timer-callback-invoke: expected (uv-timer-callback-invoke handle)"
        );
    }

    UvTimerCallbackHandle* handle = get_uv_timer_callback_handle(handle_v);
    if (handle == null) {
        return io_raise(
            interp,
            "io/uv-timer-callback-invoke-invalid-handle",
            "uv-timer-callback-invoke: invalid callback handle"
        );
    }
    if (!handle.open || handle.shim_handle == null) {
        return io_raise(
            interp,
            "io/uv-timer-callback-invoke-stale-handle",
            "uv-timer-callback-invoke: stale callback handle"
        );
    }

    int invoke_status = omni_uv_timer_callback_invoke(handle.shim_handle);
    if (invoke_status != 0) {
        char[128] err_buf;
        char[] err_msg = "uv-timer-callback-invoke: stale callback handle";
        if (try char[] formatted = io::bprintf(
            &err_buf,
            "uv-timer-callback-invoke: stale callback handle (%d)",
            invoke_status
        )) {
            err_msg = formatted;
        }
        return io_raise(interp, "io/uv-timer-callback-invoke-stale-handle", err_msg);
    }
    return make_void(interp);
}

// (__raw-uv-timer-callback-dispatch-count handle) -> Integer
fn Value* prim_uv_timer_callback_dispatch_count(Value*[] args, Env* env, Interp* interp) {
    Value* handle_v = null;
    if (args.len == 1 && is_cons(args[0])) {
        handle_v = car(args[0]);
    } else if (args.len >= 1) {
        handle_v = args[0];
    } else {
        return io_raise(
            interp,
            "io/uv-timer-callback-dispatch-count-expected-handle",
            "uv-timer-callback-dispatch-count: expected (uv-timer-callback-dispatch-count handle)"
        );
    }

    UvTimerCallbackHandle* handle = get_uv_timer_callback_handle(handle_v);
    if (handle == null) {
        return io_raise(
            interp,
            "io/uv-timer-callback-dispatch-count-invalid-handle",
            "uv-timer-callback-dispatch-count: invalid callback handle"
        );
    }

    int shim_count = handle.shim_handle != null
        ? omni_uv_timer_callback_debug_invoke_count(handle.shim_handle)
        : (int)handle.dispatch_count;
    if (shim_count < 0) shim_count = 0;
    return make_int(interp, (long)shim_count);
}
