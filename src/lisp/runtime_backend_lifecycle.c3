module lisp;

import main;

fn bool runtime_handle_state_is_live(void* state) @inline {
    return handle_effect_state_is_live((HandleEffectState*)state);
}

fn void runtime_handle_state_release(void* state) @inline {
    handle_effect_state_release((HandleEffectState*)state);
}

fn uint runtime_handle_state_continuation_refcount(void* state) @inline {
    return ((HandleEffectState*)state).continuation_refcount;
}

fn void runtime_handle_state_invalidate_continuation(void* state, bool drop_retained_ref) @inline {
    HandleEffectState* live_state = (HandleEffectState*)state;
    live_state.frame_active = false;
    if (drop_retained_ref && live_state.continuation_refcount > 0) {
        live_state.continuation_refcount--;
    }
}

fn isz runtime_attached_interp_index(Interp* interp) {
    if (interp == null) return -1;
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (runtime_backend_attached_interp_at(i) == interp) return (isz)i;
    }
    return -1;
}

fn bool runtime_interp_is_attached(Interp* interp) {
    return runtime_attached_interp_index(interp) >= 0;
}

fn bool runtime_interp_has_active_exec(Interp* interp) {
    isz idx = runtime_attached_interp_index(interp);
    return idx >= 0 && runtime_backend_attached_interp_exec_depth_at((usz)idx) > 0;
}

fn bool runtime_interp_on_current_stack(Interp* interp) {
    if (interp == null) return false;
    main::StackCtx* current = main::g_current_stack_ctx;
    if (current == null) return false;
    return current.owner_pool == &interp.stack_ctx_pool;
}

fn bool runtime_attached_interp_serial(Interp* interp, ulong* serial_out) {
    if (serial_out == null) return false;
    isz idx = runtime_attached_interp_index(interp);
    if (idx < 0) return false;
    *serial_out = runtime_backend_attached_interp_serial_at((usz)idx);
    return true;
}

fn usz runtime_interp_exec_depth(Interp* interp) @inline {
    isz idx = runtime_attached_interp_index(interp);
    return idx >= 0 ? runtime_backend_attached_interp_exec_depth_at((usz)idx) : 0;
}

fn void runtime_set_interp_exec_depth(Interp* interp, usz depth) {
    isz idx = runtime_attached_interp_index(interp);
    if (idx >= 0) runtime_backend_set_attached_interp_exec_depth_at((usz)idx, depth);
}

fn void runtime_exec_enter(Interp* interp) @inline {
    jit_require_owner_thread("runtime_exec_enter");
    runtime_backend_increment_exec_depth();
    isz idx = runtime_attached_interp_index(interp);
    assert(idx >= 0, "runtime_exec_enter: interp is not attached");
    runtime_backend_increment_attached_interp_exec_depth_at((usz)idx);
}

fn void runtime_exec_leave(Interp* interp) @inline {
    jit_require_owner_thread("runtime_exec_leave");
    assert(runtime_backend_exec_depth() > 0, "runtime_exec_leave: jit exec depth underflow");
    runtime_backend_decrement_exec_depth();
    isz idx = runtime_attached_interp_index(interp);
    assert(idx >= 0, "runtime_exec_leave: interp is not attached");
    assert(runtime_backend_attached_interp_exec_depth_at((usz)idx) > 0,
        "runtime_exec_leave: interp exec depth underflow");
    runtime_backend_decrement_attached_interp_exec_depth_at((usz)idx);
}

fn bool runtime_backend_gc_needed() @inline {
    return g_jit_gc_needed;
}

fn void runtime_backend_mark_gc_needed() @inline {
    g_jit_gc_needed = true;
}

fn void runtime_backend_clear_gc_needed() @inline {
    g_jit_gc_needed = false;
}

fn bool runtime_backend_pool_warned() @inline {
    return g_jit_pool_warned;
}

fn void runtime_backend_mark_pool_warned() @inline {
    g_jit_pool_warned = true;
}

fn void runtime_backend_clear_pool_warned() @inline {
    g_jit_pool_warned = false;
}

fn void runtime_register_attached_interp(Interp* interp) {
    assert(interp != null, "runtime_register_attached_interp: null interp");
    if (runtime_interp_is_attached(interp)) return;
    ulong serial = runtime_backend_next_attach_serial();
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (runtime_backend_attached_interp_at(i) == null) {
            runtime_backend_store_attached_interp(i, interp, serial, 0);
            return;
        }
    }
    unreachable("runtime attached interp table full");
}

fn void runtime_unregister_attached_interp(Interp* interp) {
    assert(interp != null, "runtime_unregister_attached_interp: null interp");
    for (usz i = 0; i < JIT_MAX_ATTACHED_INTERPS; i++) {
        if (runtime_backend_attached_interp_at(i) == interp) {
            runtime_backend_clear_attached_interp(i);
            return;
        }
    }
}

fn void runtime_attach_interp(Interp* interp) {
    jit_require_owner_thread("runtime_attach_interp");
    runtime_register_attached_interp(interp);
    runtime_backend_increment_interp_refcount();
}

fn void runtime_detach_interp(Interp* interp) {
    jit_require_owner_thread("runtime_detach_interp");
    ulong serial = 0;
    bool has_serial = runtime_attached_interp_serial(interp, &serial);
    if (runtime_interp_on_current_stack(interp)) {
        assert(!runtime_interp_has_active_exec(interp), "runtime_detach_interp: interp has active jit frames on current stack");
    }
    assert(runtime_backend_interp_refcount() > 0, "jit interp refcount underflow");
    runtime_backend_decrement_interp_refcount();
    runtime_unregister_attached_interp(interp);
    if (has_serial) {
        runtime_backend_mark_gc_needed();
    }
    if (runtime_backend_interp_refcount() == 0 && !runtime_backend_is_initialized()) {
        runtime_backend_clear_owner_thread_token();
        jit_retired_code_reset();
    }
}

fn void runtime_global_shutdown() @inline {
    jit_global_shutdown();
}

fn void runtime_gc_between_top_level_runs() @inline {
    jit_gc();
}
