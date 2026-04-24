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

fn bool test_stack_ctx_clone_signed_delta_negative() {
    long negative_delta = 0;
    bool negative_ok = stack_clone_signed_address_delta((usz)0x3000, (usz)0x5000, &negative_delta);
    ulong relocated = stack_clone_relocate_ptr_if_needed(
        (ulong)0x5100,
        (usz)0x4000,
        (usz)0x6000,
        negative_delta
    );
    bool untouched = stack_clone_relocate_ptr_if_needed(
        (ulong)0x7000,
        (usz)0x4000,
        (usz)0x6000,
        negative_delta
    ) == (ulong)0x7000;

    long positive_delta = 0;
    bool positive_ok = stack_clone_signed_address_delta((usz)0x7000, (usz)0x5000, &positive_delta);
    long overflow_delta = 0;
    bool overflow_positive_rejected = !stack_clone_signed_address_delta((usz)long.max + (usz)1, (usz)0, &overflow_delta);
    bool overflow_negative_rejected = !stack_clone_signed_address_delta((usz)0, (usz)long.max + (usz)1, &overflow_delta);

    return negative_ok &&
           negative_delta == -8192 &&
           relocated == (ulong)0x3100 &&
           untouched &&
           positive_ok &&
           positive_delta == 8192 &&
           overflow_positive_rejected &&
           overflow_negative_rejected;
}

fn bool test_stack_ctx_clone_residual_source_pointer_policy() {
    ulong[4] words;
    words[0] = (ulong)0x4100;
    words[1] = (ulong)0x7000;
    words[2] = ((ulong)0xab << 56) | (ulong)0x4200;
    words[3] = (ulong)0x5000;

    StackCloneResidualSourceWords residual = stack_clone_classify_residual_source_stack_words(
        (usz)&words[0],
        (usz)4 * ulong.sizeof,
        (usz)0x4000,
        (usz)0x5000
    );
    bool raw_fails_closed =
        residual.raw_words == 1 &&
        residual.low56_only_words == 1 &&
        stack_clone_residual_source_stack_words_fail_closed(residual);

    words[0] = (ulong)0x4100;
    words[1] = (ulong)0x7000;
    words[2] = (ulong)0x9000;
    words[3] = (ulong)0x5000;
    usz relocated = stack_clone_relocate_active_stack_words(
        (usz)&words[0],
        (usz)4 * ulong.sizeof,
        (usz)0x4000,
        (usz)0x5000,
        0x3000
    );
    residual = stack_clone_classify_residual_source_stack_words(
        (usz)&words[0],
        (usz)4 * ulong.sizeof,
        (usz)0x4000,
        (usz)0x5000
    );
    bool relocation_clears_raw =
        relocated == 1 &&
        words[0] == (ulong)0x7100 &&
        residual.raw_words == 0 &&
        residual.low56_only_words == 0 &&
        !stack_clone_residual_source_stack_words_fail_closed(residual);

    words[0] = ((ulong)0xab << 56) | (ulong)0x4200;
    words[1] = (ulong)0x7000;
    words[2] = (ulong)0x9000;
    words[3] = (ulong)0x5000;
    residual = stack_clone_classify_residual_source_stack_words(
        (usz)&words[0],
        (usz)4 * ulong.sizeof,
        (usz)0x4000,
        (usz)0x5000
    );
    bool low56_false_positive_tolerated =
        residual.raw_words == 0 &&
        residual.low56_only_words == 1 &&
        !stack_clone_residual_source_stack_words_fail_closed(residual);

    return raw_fails_closed &&
           relocation_clears_raw &&
           low56_false_positive_tolerated;
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
