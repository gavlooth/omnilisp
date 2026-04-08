module main;

fn void test_entry_simple(void* arg) {
    TestState* s = (TestState*)arg;
    s.value = 42;
}

fn void test_entry_100(void* arg) {
    TestState* s = (TestState*)arg;
    s.value = 100;
}

fn bool test_basic_coro() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;

    stack_ctx_init(c, &test_entry_simple, &state);

    // Switch into the context — it runs test_entry_simple, sets value=42,
    // returns, trampoline marks COMPLETED and switches back
    StackContext main_ctx;
    stack_ctx_switch_to(c, &main_ctx);

    bool ok = (c.status == CTX_COMPLETED) && (state.value == 42);

    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

fn bool test_stack_ctx_second() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;

    stack_ctx_init(c, &test_entry_100, &state);

    StackContext main_ctx;
    stack_ctx_switch_to(c, &main_ctx);

    bool ok = (c.status == CTX_COMPLETED) && (state.value == 100);

    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}
