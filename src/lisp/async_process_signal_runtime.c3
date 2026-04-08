module lisp;

import std::core::mem;
import main;

struct ProcessHandle {
    void* uv_process;
    bool open;
}

struct SignalHandle {
    void* uv_signal_watch;
    Value* callback;
    main::ScopeRegion* callback_owner_scope;
    Interp* owner_interp;
    SignalHandle* registry_prev;
    SignalHandle* registry_next;
    int signum;
    bool open;
}

fn void process_handle_finalizer(void* handle) {
    ProcessHandle* ph = (ProcessHandle*)handle;
    if (ph == null) return;
    if (ph.open && ph.uv_process != null) {
        omni_uv_process_close(ph.uv_process);
        ph.uv_process = null;
        ph.open = false;
    }
}

fn bool signal_callback_callable(Value* v) @inline {
    if (v == null) return false;
    ValueTag t = v.tag;
    return t == CLOSURE || t == PRIMITIVE || t == PARTIAL_PRIM || t == METHOD_TABLE;
}

fn main::ScopeRegion* resolve_signal_callback_owner_scope(Value* callback_v, Interp* interp) {
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

fn void release_signal_callback_owner_scope(SignalHandle* sh) {
    if (sh == null) return;
    if (sh.callback_owner_scope != null) {
        main::scope_release(sh.callback_owner_scope);
        sh.callback_owner_scope = null;
    }
    sh.callback = null;
}

fn bool register_signal_handle(Interp* interp, SignalHandle* sh) {
    if (interp == null || sh == null) return false;
    SignalHandle* head = (SignalHandle*)interp.signal_handles_head;
    sh.owner_interp = interp;
    sh.registry_prev = null;
    sh.registry_next = head;
    if (head != null) {
        head.registry_prev = sh;
    }
    interp.signal_handles_head = (void*)sh;
    return true;
}

fn void unregister_signal_handle(SignalHandle* sh) {
    if (sh == null) return;
    Interp* owner = sh.owner_interp;
    if (owner == null) {
        sh.registry_prev = null;
        sh.registry_next = null;
        return;
    }

    if (sh.registry_prev != null) {
        sh.registry_prev.registry_next = sh.registry_next;
    } else {
        owner.signal_handles_head = (void*)sh.registry_next;
    }
    if (sh.registry_next != null) {
        sh.registry_next.registry_prev = sh.registry_prev;
    }

    sh.registry_prev = null;
    sh.registry_next = null;
    sh.owner_interp = null;
}

fn void close_signal_handle(SignalHandle* sh) {
    if (sh == null || !sh.open || sh.uv_signal_watch == null) return;
    (void)omni_uv_signal_stop(sh.uv_signal_watch);
    sh.uv_signal_watch = null;
    sh.open = false;
}

fn void signal_handle_detach_runtime_state(SignalHandle* sh) {
    if (sh == null) return;
    unregister_signal_handle(sh);
    close_signal_handle(sh);
    release_signal_callback_owner_scope(sh);
}

fn void signal_handle_finalizer(void* handle) {
    SignalHandle* sh = (SignalHandle*)handle;
    if (sh == null) return;
    signal_handle_detach_runtime_state(sh);
}

fn bool ffi_handle_name_equals(Value* v, char[] expected) @inline {
    if (v == null || v.tag != FFI_HANDLE || v.ffi_val == null) return false;
    if (v.ffi_val.name_len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (v.ffi_val.lib_name[i] != expected[i]) return false;
    }
    return true;
}

fn ProcessHandle* get_process_handle(Value* v) {
    if (!ffi_handle_name_equals(v, "process-handle")) return null;
    return (ProcessHandle*)v.ffi_val.lib_handle;
}

fn Value* make_process_handle(Interp* interp, void* uv_process) {
    ProcessHandle* ph = (ProcessHandle*)mem::malloc(ProcessHandle.sizeof);
    if (ph == null) {
        if (uv_process != null) omni_uv_process_close(uv_process);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    ph.uv_process = uv_process;
    ph.open = true;
    Value* v = make_ffi_handle_ex(interp, (void*)ph, "process-handle",
        &process_handle_finalizer, true, false);
    if (v == null) {
        mem::free(ph);
        if (uv_process != null) omni_uv_process_close(uv_process);
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    return v;
}

fn SignalHandle* get_signal_handle(Value* v) {
    if (!ffi_handle_name_equals(v, "signal-handle")) return null;
    return (SignalHandle*)v.ffi_val.lib_handle;
}

fn Value* make_signal_handle(Interp* interp, void* uv_signal_watch, int signum, Value* callback_v) {
    if (uv_signal_watch == null) {
        return io_raise(interp, "io/signal-handle-failed", "signal-handle: failed to create signal watcher");
    }

    SignalHandle* sh = (SignalHandle*)mem::malloc(SignalHandle.sizeof);
    if (sh == null) {
        (void)omni_uv_signal_stop(uv_signal_watch);
        return io_raise(interp, "io/signal-handle-out-of-memory", "signal-handle: out of memory");
    }

    main::ScopeRegion* callback_owner_scope = resolve_signal_callback_owner_scope(callback_v, interp);
    if (callback_owner_scope == null) {
        (void)omni_uv_signal_stop(uv_signal_watch);
        mem::free(sh);
        return io_raise(
            interp,
            "io/signal-handle-callback-lifetime-invalid",
            "signal-handle: callback lifetime is not owned by the active interpreter scope chain"
        );
    }

    main::scope_retain(callback_owner_scope);
    sh.uv_signal_watch = uv_signal_watch;
    sh.callback = callback_v;
    sh.callback_owner_scope = callback_owner_scope;
    sh.owner_interp = null;
    sh.registry_prev = null;
    sh.registry_next = null;
    sh.signum = signum;
    sh.open = true;

    if (!register_signal_handle(interp, sh)) {
        signal_handle_detach_runtime_state(sh);
        mem::free(sh);
        return io_raise(interp, "io/signal-handle-failed", "signal-handle: failed to register signal handle");
    }

    Value* v = make_ffi_handle_ex(interp, (void*)sh, "signal-handle",
        &signal_handle_finalizer, true, false);
    if (v == null) {
        signal_handle_detach_runtime_state(sh);
        mem::free(sh);
        return io_raise(interp, "io/signal-handle-out-of-memory", "signal-handle: out of memory");
    }
    return v;
}
