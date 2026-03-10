module main;

// Generator-style entry: yields 3 values then completes
fn void test_entry_generator(void* arg) {
    TestState* s = (TestState*)arg;

    s.value = 10;
    s.step = 1;
    stack_ctx_suspend();  // yield 10

    s.value = 20;
    s.step = 2;
    stack_ctx_suspend();  // yield 20

    s.value = 30;
    s.step = 3;
    // returns — trampoline marks COMPLETED
}

// Deeply recursive entry — tests stack depth on separate stack
fn void test_entry_recursive(void* arg) {
    TestState* s = (TestState*)arg;
    s.value = test_recursive_sum(100);
}

fn int test_recursive_sum(int n) {
    if (n <= 0) return 0;
    return n + test_recursive_sum(n - 1);
}

// THE critical test: suspend/resume (generator pattern)
fn bool test_suspend_resume() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;
    state.step = 0;

    stack_ctx_init(c, &test_entry_generator, &state);

    // First switch: runs until first stack_ctx_suspend()
    StackContext main_ctx;
    stack_ctx_switch_to(c, &main_ctx);

    bool ok = (c.status == CTX_SUSPENDED) && (state.value == 10) && (state.step == 1);

    // Resume: runs until second stack_ctx_suspend()
    stack_ctx_resume(c, &main_ctx);

    ok = ok && (c.status == CTX_SUSPENDED) && (state.value == 20) && (state.step == 2);

    // Resume again: runs to completion
    stack_ctx_resume(c, &main_ctx);

    ok = ok && (c.status == CTX_COMPLETED) && (state.value == 30) && (state.step == 3);

    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

// Test that recursive code works on separate stack
fn bool test_stack_ctx_recursion() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;

    stack_ctx_init(c, &test_entry_recursive, &state);

    StackContext main_ctx;
    stack_ctx_switch_to(c, &main_ctx);

    // sum(1..100) = 5050
    bool ok = (c.status == CTX_COMPLETED) && (state.value == 5050);

    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}

// Test two coroutines interleaving
fn bool test_two_coros_interleaved() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c1 = stack_ctx_create(&pool);
    StackCtx* c2 = stack_ctx_create(&pool);
    if (c1 == null || c2 == null) { stack_pool_shutdown(&pool); return false; }

    TestState s1;
    TestState s2;
    s1.value = 0; s1.step = 0;
    s2.value = 0; s2.step = 0;

    stack_ctx_init(c1, &test_entry_generator, &s1);
    stack_ctx_init(c2, &test_entry_generator, &s2);

    StackContext main_ctx;

    // Run c1 to first yield
    stack_ctx_switch_to(c1, &main_ctx);
    bool ok = (s1.value == 10) && (c1.status == CTX_SUSPENDED);

    // Run c2 to first yield
    stack_ctx_switch_to(c2, &main_ctx);
    ok = ok && (s2.value == 10) && (c2.status == CTX_SUSPENDED);

    // Resume c1 to second yield
    stack_ctx_resume(c1, &main_ctx);
    ok = ok && (s1.value == 20);

    // Resume c2 to completion (skip second yield, go to end)
    stack_ctx_resume(c2, &main_ctx);
    ok = ok && (s2.value == 20) && (c2.status == CTX_SUSPENDED);
    stack_ctx_resume(c2, &main_ctx);
    ok = ok && (s2.value == 30) && (c2.status == CTX_COMPLETED);

    // Resume c1 to completion
    stack_ctx_resume(c1, &main_ctx);
    ok = ok && (s1.value == 30) && (c1.status == CTX_COMPLETED);

    stack_ctx_destroy(c1, &pool);
    stack_ctx_destroy(c2, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}
