module lisp;

import std::io;
import std::collections::list;
import std::core::mem;
import main;

fn void runtime_backend_schedule_gc_for_pool_pressure() {
    if (runtime_backend_gc_needed()) return;
    runtime_backend_mark_gc_needed();
    $if DEBUG_BUILD:
        io::eprintfn("[debug] JIT pool at %d/%d (%.0f%%), GC scheduled",
            g_jit_state_count, JIT_STATE_POOL_SIZE,
            (double)g_jit_state_count / (double)JIT_STATE_POOL_SIZE * 100.0);
    $endif
}

fn void runtime_backend_warn_spill_alloc_failed() {
    if (runtime_backend_pool_warned()) return;
    io::printfn("WARNING: JIT state pool full (%d states), spill allocation failed; JIT compile will fail for this expression", JIT_STATE_POOL_SIZE);
    runtime_backend_mark_pool_warned();
}

fn void runtime_backend_warn_spill_tracking() {
    if (runtime_backend_pool_warned()) return;
    io::printfn(
        "WARNING: JIT state pool full (%d states), tracking overflow in spill list (count=%d)",
        JIT_STATE_POOL_SIZE,
        g_jit_spill_count
    );
    runtime_backend_mark_pool_warned();
}

fn bool jit_track_compiled_state(void* s, void* code_ptr, Interp* owner_interp, ulong* compile_nonce_out) {
    jit_require_owner_thread("jit_track_compiled_state");
    ulong owner_serial = 0;
    if (compile_nonce_out != null) *compile_nonce_out = 0;
    if (owner_interp == null || compile_nonce_out == null) return false;
    if (!jit_attached_interp_serial(owner_interp, &owner_serial)) return false;
    g_jit_compile_nonce_counter++;
    if (g_jit_compile_nonce_counter == 0) g_jit_compile_nonce_counter = 1;
    ulong compile_nonce = g_jit_compile_nonce_counter;
    if (g_jit_state_count < JIT_STATE_POOL_SIZE) {
        g_jit_states[g_jit_state_count].state = s;
        g_jit_states[g_jit_state_count].code = code_ptr;
        g_jit_states[g_jit_state_count].owner_interp = owner_interp;
        g_jit_states[g_jit_state_count].owner_attach_serial = owner_serial;
        g_jit_states[g_jit_state_count].compile_nonce = compile_nonce;
        g_jit_state_count++;
        *compile_nonce_out = compile_nonce;
        // Signal GC needed when approaching capacity.
        if (g_jit_state_count >= JIT_STATE_GC_THRESHOLD) {
            runtime_backend_schedule_gc_for_pool_pressure();
        }
        return true;
    }

    JitStateSpillNode* spill = (JitStateSpillNode*)mem::malloc(JitStateSpillNode.sizeof);
    if (spill == null) {
        runtime_backend_warn_spill_alloc_failed();
        return false;
    }

    spill.state = s;
    spill.code = code_ptr;
    spill.owner_interp = owner_interp;
    spill.owner_attach_serial = owner_serial;
    spill.compile_nonce = compile_nonce;
    spill.next = g_jit_spill_states;
    g_jit_spill_states = spill;
    g_jit_spill_count++;
    runtime_backend_mark_gc_needed();
    *compile_nonce_out = compile_nonce;

    runtime_backend_warn_spill_tracking();
    return true;
}

/**
 * Compile an expression to a native function.
 *
 * Returns a null handle if compilation fails.
 * The compiled function signature is: Value* fn(Interp*)
 */
fn JitCompiledFn jit_compile(Expr* expr, Interp* interp) {
    jit_require_owner_thread("jit_compile");
    jit_global_init();
    if (interp == null) return jit_compiled_fn_null();

    void* s = jit_new_state();
    if (s == null) return jit_compiled_fn_null();

    // Function prolog: Value* fn(Interp* interp)
    _jit_prolog(s);
    void* arg = _jit_arg(s, CODE_ARG_L);
    _jit_getarg_l(s, JIT_V0, arg);  // V0 = interp (callee-saved)

    // Compile the expression, result goes to R0.
    JitLocals locals = {};
    locals.interp_stack_offset = _jit_allocai(s, 8);
    _jit_new_node_www(s, CODE_STXI_L, (long)locals.interp_stack_offset, (long)JIT_FP, (long)JIT_V0);
    if (catch err = jit_compile_expr(s, expr, interp, &locals, true)) {
        _jit_destroy_state(s);
        return jit_compiled_fn_null();
    }

    // Return R0
    _jit_retr(s, JIT_R0, CODE_RETR_L);

    // Emit machine code
    void* code = _jit_emit(s);

    // C10: If emit failed, destroy the state immediately and bail out
    if (code == null) {
        _jit_destroy_state(s);
        return jit_compiled_fn_null();
    }

    _jit_clear_state(s);

    // Track state for cleanup (code buffer lives inside it).
    // C9: Cannot destroy_state here — code buffer lives inside the state.
    // States are destroyed by jit_gc() between top-level evaluations.
    ulong compile_nonce = 0;
    if (!jit_track_compiled_state(s, code, interp, &compile_nonce)) {
        // Tracking failed (spill OOM edge). Destroy state so code buffer is not leaked.
        _jit_destroy_state(s);
        return jit_compiled_fn_null();
    }

    return { .code_fn = (JitFn)code, .compile_nonce = compile_nonce };
}
