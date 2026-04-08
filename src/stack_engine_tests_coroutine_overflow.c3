module main;

// Stack overflow entry - recurses until guard page hit.
fn void test_entry_overflow(void* arg) {
    TestState* s = (TestState*)arg;
    test_overflow_recurse(s, 0);
}

fn int test_overflow_recurse(TestState* s, int depth) {
    s.value = depth;
    // 256-byte frame ensures we overflow the 64KB stack quickly (~250 frames).
    char[256] padding;
    padding[0] = (char)depth;
    return test_overflow_recurse(s, depth + 1) + (int)padding[0];
}

fn bool test_stack_overflow_recovery() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;

    stack_ctx_init(c, &test_entry_overflow, &state);

    StackContext main_ctx;
    stack_ctx_switch_to(c, &main_ctx);

    // Should recover gracefully: context marked DEAD, not crashed.
    bool ok = (c.status == CTX_DEAD);
    // Should have recursed some before overflow.
    ok = ok && (state.value > 10);

    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}
