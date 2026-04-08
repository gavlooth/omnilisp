module lisp;

import std::core::mem;
import std::io;
import main;

struct UvTimerCallbackHandle {
    void* shim_handle;
    Value* callback;
    main::ScopeRegion* callback_owner_scope;
    Interp* owner_interp;
    usz dispatch_count;
    bool open;
}

fn bool uv_timer_callback_callable(Value* v) @inline {
    if (v == null) return false;
    ValueTag t = v.tag;
    return t == CLOSURE || t == PRIMITIVE || t == PARTIAL_PRIM || t == METHOD_TABLE;
}

fn main::ScopeRegion* resolve_uv_timer_callback_owner_scope(Value* callback_v, Interp* interp) {
    if (callback_v == null || interp == null) return null;
    main::ScopeRegion* scope = interp.current_scope;
    while (scope != null) {
        if (main::is_in_scope((void*)callback_v, scope)) return scope;
        scope = scope.parent;
    }
    if (interp.root_scope != null && main::is_in_scope((void*)callback_v, interp.root_scope)) {
        return interp.root_scope;
    }
    return null;
}

fn void release_uv_timer_callback_owner_scope(UvTimerCallbackHandle* handle) {
    if (handle == null) return;
    if (handle.callback_owner_scope != null) {
        main::scope_release(handle.callback_owner_scope);
        handle.callback_owner_scope = null;
    }
    handle.callback = null;
}

fn void close_uv_timer_callback_shim(UvTimerCallbackHandle* handle) {
    if (handle == null || handle.shim_handle == null) return;
    (void)omni_uv_timer_callback_unregister(handle.shim_handle);
    omni_uv_timer_callback_free(handle.shim_handle);
    handle.shim_handle = null;
}

fn void uv_timer_callback_handle_detach_runtime_state(UvTimerCallbackHandle* handle) {
    if (handle == null) return;
    close_uv_timer_callback_shim(handle);
    release_uv_timer_callback_owner_scope(handle);
    handle.owner_interp = null;
    handle.open = false;
}

fn void uv_timer_callback_handle_finalizer(void* raw_handle) {
    UvTimerCallbackHandle* handle = (UvTimerCallbackHandle*)raw_handle;
    if (handle == null) return;
    uv_timer_callback_handle_detach_runtime_state(handle);
}

fn bool uv_timer_callback_handle_name_equals(Value* v) @inline {
    if (v == null || v.tag != FFI_HANDLE || v.ffi_val == null) return false;
    char[] expected = "uv-timer-callback-handle";
    if (v.ffi_val.name_len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (v.ffi_val.lib_name[i] != expected[i]) return false;
    }
    return true;
}

fn UvTimerCallbackHandle* get_uv_timer_callback_handle(Value* v) {
    if (!uv_timer_callback_handle_name_equals(v)) return null;
    return (UvTimerCallbackHandle*)v.ffi_val.lib_handle;
}

fn void uv_timer_callback_shim_trampoline(void* user_data) {
    UvTimerCallbackHandle* handle = (UvTimerCallbackHandle*)user_data;
    if (handle == null || !handle.open || handle.callback == null || handle.owner_interp == null) return;
    if (!uv_timer_callback_callable(handle.callback)) return;

    Interp* interp = handle.owner_interp;
    Value* tick = make_int(interp, (long)(handle.dispatch_count + 1));
    Value* result = jit_apply_value(handle.callback, tick, interp);
    if (result != null && result.tag == ERROR) return;
    handle.dispatch_count++;
}

fn Value* make_uv_timer_callback_handle(Interp* interp, Value* callback_v) {
    if (!uv_timer_callback_callable(callback_v)) {
        return io_raise(
            interp,
            "io/uv-timer-callback-handle-callback-not-callable",
            "uv-timer-callback-handle: callback must be a procedure"
        );
    }

    main::ScopeRegion* callback_owner_scope = resolve_uv_timer_callback_owner_scope(callback_v, interp);
    if (callback_owner_scope == null) {
        return io_raise(
            interp,
            "io/uv-timer-callback-handle-callback-lifetime-invalid",
            "uv-timer-callback-handle: callback lifetime is not owned by the active interpreter scope chain"
        );
    }

    UvTimerCallbackHandle* handle = (UvTimerCallbackHandle*)mem::malloc(UvTimerCallbackHandle.sizeof);
    if (handle == null) {
        return io_raise(
            interp,
            "io/uv-timer-callback-handle-out-of-memory",
            "uv-timer-callback-handle: out of memory"
        );
    }

    *handle = {};
    main::scope_retain(callback_owner_scope);
    handle.callback = callback_v;
    handle.callback_owner_scope = callback_owner_scope;
    handle.owner_interp = interp;
    handle.open = true;

    int register_status = omni_uv_timer_callback_register(
        &uv_timer_callback_shim_trampoline,
        handle,
        &handle.shim_handle
    );
    if (register_status != 0 || handle.shim_handle == null) {
        uv_timer_callback_handle_detach_runtime_state(handle);
        mem::free(handle);
        char[128] err_buf;
        char[] err_msg = "uv-timer-callback-handle: failed to register callback shim";
        if (try char[] formatted = io::bprintf(
            &err_buf,
            "uv-timer-callback-handle: failed to register callback shim (%d)",
            register_status
        )) {
            err_msg = formatted;
        }
        return io_raise(interp, "io/uv-timer-callback-handle-register-failed", err_msg);
    }

    Value* value = make_ffi_handle_ex(
        interp,
        (void*)handle,
        "uv-timer-callback-handle",
        &uv_timer_callback_handle_finalizer,
        true,
        false
    );
    if (value == null) {
        uv_timer_callback_handle_detach_runtime_state(handle);
        mem::free(handle);
        return io_raise(
            interp,
            "io/uv-timer-callback-handle-out-of-memory",
            "uv-timer-callback-handle: out of memory"
        );
    }
    return value;
}

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
