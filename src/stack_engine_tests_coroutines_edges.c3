module main;

// Test that FPU state is preserved across context switches
fn void test_entry_fpu(void* arg) {
    TestState* s = (TestState*)arg;
    // The FPU state should be default values on entry
    // Just set a marker to confirm we ran
    s.value = 99;
    stack_ctx_suspend();
    s.value = 100;
}

fn bool test_fpu_preservation() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;
    stack_ctx_init(c, &test_entry_fpu, &state);

    // Save current FPU state
    uint mxcsr_before;
    uint x87cw_before;
    fpu_save(&mxcsr_before, &x87cw_before);

    StackContext main_ctx;
    stack_ctx_switch_to(c, &main_ctx);  // runs until suspend
    bool ok = (state.value == 99) && (c.status == CTX_SUSPENDED);

    // Verify parent FPU state preserved after switch
    uint mxcsr_after;
    uint x87cw_after;
    fpu_save(&mxcsr_after, &x87cw_after);
    ok = ok && (mxcsr_before == mxcsr_after);
    ok = ok && (x87cw_before == x87cw_after);

    // Resume and complete
    stack_ctx_resume(c, &main_ctx);
    ok = ok && (state.value == 100) && (c.status == CTX_COMPLETED);

    // FPU still preserved
    fpu_save(&mxcsr_after, &x87cw_after);
    ok = ok && (mxcsr_before == mxcsr_after);
    ok = ok && (x87cw_before == x87cw_after);

    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

// Clone a suspended context and verify multi-shot behavior:
// cloned execution completes independently while the source remains resumable.
fn bool test_stack_ctx_clone_multishot() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* source = stack_ctx_create(&pool);
    if (source == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;
    state.step = 0;
    stack_ctx_init(source, &test_entry_generator, &state);

    StackContext main_ctx;
    stack_ctx_switch_to(source, &main_ctx);  // yield #1
    if (source.status != CTX_SUSPENDED || state.step != 1) {
        stack_ctx_destroy(source, &pool);
        stack_pool_shutdown(&pool);
        return false;
    }

    StackCtx* clone = stack_ctx_clone(source, &pool);
    if (clone == null || clone.status != CTX_SUSPENDED) {
        stack_ctx_destroy(source, &pool);
        if (clone != null) stack_ctx_destroy(clone, &pool);
        stack_pool_shutdown(&pool);
        return false;
    }

    // Drive clone to completion first.
    stack_ctx_resume(clone, &main_ctx);  // yield #2
    bool ok = (clone.status == CTX_SUSPENDED);
    stack_ctx_resume(clone, &main_ctx);  // complete
    ok = ok && (clone.status == CTX_COMPLETED);

    // Source must remain suspended and independently resumable.
    ok = ok && (source.status == CTX_SUSPENDED);
    stack_ctx_resume(source, &main_ctx);
    ok = ok && (source.status == CTX_SUSPENDED);
    stack_ctx_resume(source, &main_ctx);
    ok = ok && (source.status == CTX_COMPLETED);

    stack_ctx_destroy(clone, &pool);
    stack_ctx_destroy(source, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}
