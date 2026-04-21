module lisp;

import std::io;

fn void* jit_current_thread_token() @inline {
    return (void*)&g_jit_thread_marker;
}

fn void jit_thread_violation(char[] op) @inline {
    io::eprintfn("jit runtime thread-affinity violation in %s", op);
    unreachable("jit runtime used from non-owner thread");
}

fn void jit_require_owner_thread(char[] op) @inline {
    void* cur = jit_current_thread_token();
    if (runtime_backend_owner_thread_token_is_unset()) {
        runtime_backend_set_owner_thread_token(cur);
        return;
    }
    if (!runtime_backend_owner_thread_token_matches(cur)) {
        jit_thread_violation(op);
    }
}

fn isz jit_attached_interp_index(Interp* interp) {
    return runtime_attached_interp_index(interp);
}

fn bool jit_interp_is_attached(Interp* interp) {
    return runtime_interp_is_attached(interp);
}

fn bool jit_attached_interp_serial(Interp* interp, ulong* serial_out) {
    return runtime_attached_interp_serial(interp, serial_out);
}

fn bool jit_is_attached_serial(ulong attach_serial) {
    if (attach_serial == 0) return false;
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (runtime_backend_attached_interp_at(i) == null) continue;
        if (runtime_backend_attached_interp_serial_at(i) == attach_serial) return true;
    }
    return false;
}

fn bool jit_interp_has_active_exec(Interp* interp) {
    return runtime_interp_has_active_exec(interp);
}

fn bool jit_interp_on_current_stack(Interp* interp) {
    return runtime_interp_on_current_stack(interp);
}

fn usz runtime_backend_interp_refcount() @inline {
    return g_jit_interp_refcount;
}

fn void runtime_backend_increment_interp_refcount() @inline {
    g_jit_interp_refcount++;
}

fn void runtime_backend_decrement_interp_refcount() @inline {
    g_jit_interp_refcount--;
}

fn usz runtime_backend_exec_depth() @inline {
    return g_jit_exec_depth;
}

fn void runtime_backend_set_exec_depth(usz depth) @inline {
    g_jit_exec_depth = depth;
}

fn void runtime_backend_increment_exec_depth() @inline {
    g_jit_exec_depth++;
}

fn void runtime_backend_decrement_exec_depth() @inline {
    g_jit_exec_depth--;
}

fn usz runtime_backend_suspended_guard_count() @inline {
    return g_jit_suspended_guard_count;
}

fn void runtime_backend_increment_suspended_guard_count() @inline {
    g_jit_suspended_guard_count++;
}

fn void runtime_backend_decrement_suspended_guard_count() @inline {
    g_jit_suspended_guard_count--;
}

fn bool runtime_backend_is_initialized() @inline {
    return g_jit_initialized;
}

fn void runtime_backend_mark_initialized() @inline {
    g_jit_initialized = true;
}

fn void runtime_backend_clear_initialized() @inline {
    g_jit_initialized = false;
}

fn void runtime_backend_clear_owner_thread_token() @inline {
    g_jit_owner_thread_token = null;
}

fn bool runtime_backend_owner_thread_token_is_unset() @inline {
    return g_jit_owner_thread_token == null;
}

fn void runtime_backend_set_owner_thread_token(void* token) @inline {
    g_jit_owner_thread_token = token;
}

fn bool runtime_backend_owner_thread_token_matches(void* token) @inline {
    return g_jit_owner_thread_token == token;
}

fn ulong runtime_backend_next_compile_nonce() @inline {
    jit_require_owner_thread("runtime_backend_next_compile_nonce");
    g_jit_compile_nonce_counter++;
    if (g_jit_compile_nonce_counter == 0) g_jit_compile_nonce_counter = 1;
    return g_jit_compile_nonce_counter;
}
