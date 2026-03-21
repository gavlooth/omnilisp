module lisp;

// JIT state cleanup tracking (C9: increased from 64 to 256, then to 4096).
// Larger headroom avoids overflow warnings during long nested JIT chains where
// safe-point GC cannot run until execution unwinds.
const usz JIT_STATE_POOL_SIZE = 16384;
const usz JIT_STATE_GC_THRESHOLD = JIT_STATE_POOL_SIZE * 3 / 4;  // GC when pool reaches 75%

struct JitTrackedState {
    void* state;
    void* code;
    Interp* owner_interp;
    ulong owner_attach_serial;
    ulong compile_nonce;
}

JitTrackedState[JIT_STATE_POOL_SIZE] g_jit_states;
usz g_jit_state_count = 0;

struct JitStateSpillNode {
    void* state;
    void* code;
    Interp* owner_interp;
    ulong owner_attach_serial;
    ulong compile_nonce;
    JitStateSpillNode* next;
}

JitStateSpillNode* g_jit_spill_states = null;
usz g_jit_spill_count = 0;

fn void* runtime_backend_tracked_state_state_at(usz idx) @inline {
    return g_jit_states[idx].state;
}

fn void* runtime_backend_tracked_state_code_at(usz idx) @inline {
    return g_jit_states[idx].code;
}

fn Interp* runtime_backend_tracked_state_owner_interp_at(usz idx) @inline {
    return g_jit_states[idx].owner_interp;
}

fn ulong runtime_backend_tracked_state_owner_attach_serial_at(usz idx) @inline {
    return g_jit_states[idx].owner_attach_serial;
}

fn ulong runtime_backend_tracked_state_compile_nonce_at(usz idx) @inline {
    return g_jit_states[idx].compile_nonce;
}

fn void runtime_backend_clear_tracked_state_slot(usz idx) @inline {
    g_jit_states[idx].state = null;
    g_jit_states[idx].code = null;
    g_jit_states[idx].owner_interp = null;
    g_jit_states[idx].owner_attach_serial = 0;
    g_jit_states[idx].compile_nonce = 0;
}

fn usz runtime_backend_tracked_state_count() @inline {
    return g_jit_state_count;
}

fn void runtime_backend_set_tracked_state_count(usz count) @inline {
    g_jit_state_count = count;
}

fn JitStateSpillNode* runtime_backend_spill_state_head() @inline {
    return g_jit_spill_states;
}

fn void runtime_backend_set_spill_state_head(JitStateSpillNode* node) @inline {
    g_jit_spill_states = node;
}

fn usz runtime_backend_spill_state_count() @inline {
    return g_jit_spill_count;
}

fn void runtime_backend_set_spill_state_count(usz count) @inline {
    g_jit_spill_count = count;
}

fn JitStateSpillNode* runtime_backend_spill_state_next(JitStateSpillNode* node) @inline {
    return node != null ? node.next : null;
}

fn void* runtime_backend_spill_state_state(JitStateSpillNode* node) @inline {
    return node != null ? node.state : null;
}

fn void* runtime_backend_spill_state_code(JitStateSpillNode* node) @inline {
    return node != null ? node.code : null;
}

fn Interp* runtime_backend_spill_state_owner_interp(JitStateSpillNode* node) @inline {
    return node != null ? node.owner_interp : null;
}

fn ulong runtime_backend_spill_state_owner_attach_serial(JitStateSpillNode* node) @inline {
    return node != null ? node.owner_attach_serial : 0;
}

fn ulong runtime_backend_spill_state_compile_nonce(JitStateSpillNode* node) @inline {
    return node != null ? node.compile_nonce : 0;
}
