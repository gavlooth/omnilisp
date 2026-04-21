module lisp;

struct JitAttachedInterp {
    Interp* interp;
    ulong attach_serial;
    usz active_exec_depth;
}

const usz JIT_MAX_ATTACHED_INTERPS = 1024;
JitAttachedInterp[JIT_MAX_ATTACHED_INTERPS] g_jit_attached_interps;
ulong g_jit_attach_serial_counter = 0;

fn Interp* runtime_backend_attached_interp_at(usz idx) @inline {
    return g_jit_attached_interps[idx].interp;
}

fn ulong runtime_backend_attached_interp_serial_at(usz idx) @inline {
    return g_jit_attached_interps[idx].attach_serial;
}

fn usz runtime_backend_attached_interp_exec_depth_at(usz idx) @inline {
    return g_jit_attached_interps[idx].active_exec_depth;
}

fn void runtime_backend_set_attached_interp_exec_depth_at(usz idx, usz depth) @inline {
    g_jit_attached_interps[idx].active_exec_depth = depth;
}

fn void runtime_backend_increment_attached_interp_exec_depth_at(usz idx) @inline {
    g_jit_attached_interps[idx].active_exec_depth++;
}

fn void runtime_backend_decrement_attached_interp_exec_depth_at(usz idx) @inline {
    g_jit_attached_interps[idx].active_exec_depth--;
}

fn void runtime_backend_store_attached_interp(usz idx, Interp* interp, ulong attach_serial, usz active_exec_depth) @inline {
    g_jit_attached_interps[idx].interp = interp;
    g_jit_attached_interps[idx].attach_serial = attach_serial;
    g_jit_attached_interps[idx].active_exec_depth = active_exec_depth;
}

fn void runtime_backend_clear_attached_interp(usz idx) @inline {
    runtime_backend_store_attached_interp(idx, null, 0, 0);
}

fn ulong runtime_backend_next_attach_serial() @inline {
    g_jit_attach_serial_counter++;
    if (g_jit_attach_serial_counter == 0) g_jit_attach_serial_counter = 1;
    return g_jit_attach_serial_counter;
}

fn void jit_register_attached_interp(Interp* interp) {
    runtime_register_attached_interp(interp);
}

fn void jit_unregister_attached_interp(Interp* interp) {
    runtime_unregister_attached_interp(interp);
}
