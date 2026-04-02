module lisp;

import std::core::mem;
import std::io;
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

usz g_signal_debug_dispatch_count = 0;

fn void signal_debug_reset_dispatch_count() {
    g_signal_debug_dispatch_count = 0;
}

fn usz signal_debug_dispatch_count() {
    return g_signal_debug_dispatch_count;
}

fn void tcp_handle_finalizer(void* handle) {
    TcpHandle* th = (TcpHandle*)handle;
    if (th == null) return;
    if (th.uv_listener_handle != null) {
        omni_uv_tcp_close_and_free(th.uv_listener_handle);
        th.uv_listener_handle = null;
    }
    if (th.connected && th.fd >= 0) {
        c_close_fd(th.fd);
        th.connected = false;
        th.fd = -1;
    }
}

fn Value* make_tcp_stream_handle(Interp* interp, int fd, TcpTransport transport = TCP_TRANSPORT_INET) {
    TcpHandle* th = (TcpHandle*)mem::malloc(TcpHandle.sizeof);
    if (th == null) return io_raise(interp, "io/tcp-connect-out-of-memory", "tcp-connect: out of memory");
    th.fd = fd;
    th.connected = true;
    th.mode = TCP_STREAM;
    th.transport = transport;
    th.uv_listener_handle = null;
    Value* v = make_ffi_handle_ex(interp, (void*)th, "tcp-handle",
        &tcp_handle_finalizer, true, false);
    if (v == null) {
        if (th.fd >= 0) c_close_fd(th.fd);
        mem::free(th);
        return io_raise(interp, "io/tcp-connect-out-of-memory", "tcp-connect: out of memory");
    }
    return v;
}

fn TcpHandle* get_tcp_handle(Value* v) {
    if (v == null || v.tag != FFI_HANDLE) return null;
    if (v.ffi_val == null) return null;
    return (TcpHandle*)v.ffi_val.lib_handle;
}

fn void udp_handle_finalizer(void* handle) {
    UdpHandle* uh = (UdpHandle*)handle;
    if (uh == null) return;
    if (uh.open && uh.fd >= 0) {
        c_close_fd(uh.fd);
        uh.open = false;
        uh.fd = -1;
    }
}

fn Value* make_udp_handle(Interp* interp, int fd) {
    UdpHandle* uh = (UdpHandle*)mem::malloc(UdpHandle.sizeof);
    if (uh == null) {
        c_close_fd(fd);
        return io_raise(interp, "io/udp-socket-out-of-memory", "udp-socket: out of memory");
    }
    uh.fd = fd;
    uh.open = true;
    Value* v = make_ffi_handle_ex(interp, (void*)uh, "udp-handle",
        &udp_handle_finalizer, true, false);
    if (v == null) {
        mem::free(uh);
        c_close_fd(fd);
        return io_raise(interp, "io/udp-socket-out-of-memory", "udp-socket: out of memory");
    }
    return v;
}

fn UdpHandle* get_udp_handle(Value* v) {
    if (v == null || v.tag != FFI_HANDLE) return null;
    if (v.ffi_val == null) return null;
    return (UdpHandle*)v.ffi_val.lib_handle;
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

fn void dispatch_signal_callback(SignalHandle* sh, int signum, Interp* interp) {
    if (sh == null || !sh.open || sh.callback == null) return;
    if (!signal_callback_callable(sh.callback)) return;

    g_signal_debug_dispatch_count++;
    Value* arg = make_int(interp, (long)signum);
    Value* result = jit_apply_value(sh.callback, arg, interp);
    if (result != null && result.tag == ERROR) {
        // Keep signal delivery non-fatal: callback failures should not crash runtime loops.
        return;
    }
}

fn void drain_signal_handle_pending(SignalHandle* sh, Interp* interp) {
    if (sh == null || !sh.open || sh.uv_signal_watch == null) return;
    int pending = omni_uv_signal_poll_pending(sh.uv_signal_watch);
    if (pending <= 0) return;

    for (int i = 0; i < pending; i++) {
        dispatch_signal_callback(sh, sh.signum, interp);
    }
}

fn void dispatch_pending_signal_callbacks(Interp* interp) {
    if (interp == null) return;
    SignalHandle* sh = (SignalHandle*)interp.signal_handles_head;
    while (sh != null) {
        SignalHandle* next = sh.registry_next;
        drain_signal_handle_pending(sh, interp);
        sh = next;
    }
}

fn void close_fs_like_handle(Value* v, Interp* interp) {
    if (!ffi_handle_name_equals(v, "fs-handle")) return;
    Value*[1] close_args;
    close_args[0] = v;
    (void)prim_fs_close(close_args[..], null, interp);
}

fn char* copy_to_heap_c_string(char[] src) {
    usz cap = src.len + 1;
    char* out = (char*)mem::malloc(cap);
    if (out == null) return null;
    for (usz i = 0; i < src.len; i++) out[i] = src[i];
    out[src.len] = 0;
    return out;
}

fn char* copy_value_to_heap_c_string(Value* v, Interp* interp) {
    if (v == null) return null;
    if (is_string(v)) return copy_to_heap_c_string(v.str_chars[:v.str_len]);
    if (is_symbol(v)) return copy_to_heap_c_string(interp.symbols.get_name(v.sym_val));
    return null;
}

fn void free_heap_c_string_items(char** items, usz len) {
    if (items == null) return;
    for (usz i = 0; i < len; i++) {
        if (items[i] != null) mem::free(items[i]);
    }
    mem::free(items);
}

fn Value* value_to_process_handle_arg(Value* v, Interp* interp) {
    if (v == null) return null;
    if (ffi_handle_name_equals(v, "process-handle")) return v;
    if (v.tag == HASHMAP && v.hashmap_val != null) {
        Value* handle_key = make_symbol(interp, interp.symbols.intern("handle"));
        return hashmap_get(v.hashmap_val, handle_key);
    }
    return null;
}

fn Value* collect_string_sequence(
    Value* seq,
    char*** out_items,
    usz* out_len,
    Interp* interp,
    char[] code,
    char[] message
) {
    *out_items = null;
    *out_len = 0;
    if (seq == null || is_nil(seq)) return null;

    usz count = 0;
    if (seq.tag == ARRAY && seq.array_val != null) {
        count = seq.array_val.length;
    } else if (is_cons(seq)) {
        Value* cur = seq;
        while (is_cons(cur)) {
            count++;
            cur = cdr(cur);
        }
        if (!is_nil(cur)) return io_raise(interp, code, message);
    } else {
        return io_raise(interp, code, message);
    }

    if (count == 0) return null;

    char** items = (char**)mem::malloc(char*.sizeof * count);
    if (items == null) {
        return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
    }
    for (usz i = 0; i < count; i++) items[i] = null;

    if (seq.tag == ARRAY && seq.array_val != null) {
        for (usz i = 0; i < count; i++) {
            Value* it = seq.array_val.items[i];
            if (!(is_string(it) || is_symbol(it))) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, code, message);
            }
            char* copied = copy_value_to_heap_c_string(it, interp);
            if (copied == null) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
            }
            items[i] = copied;
        }
    } else {
        Value* cur = seq;
        usz i = 0;
        while (is_cons(cur) && i < count) {
            Value* it = car(cur);
            if (!(is_string(it) || is_symbol(it))) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, code, message);
            }
            char* copied = copy_value_to_heap_c_string(it, interp);
            if (copied == null) {
                free_heap_c_string_items(items, count);
                return io_raise(interp, "io/process-spawn-out-of-memory", "process-spawn: out of memory");
            }
            items[i] = copied;
            i++;
            cur = cdr(cur);
        }
    }

    *out_items = items;
    *out_len = count;
    return null;
}
