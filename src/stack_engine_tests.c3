module main;

struct TestState {
    int value;
    int step;
}

struct TestDeferLog {
    int[8] order;
    int count;
}

struct TestDeferArg {
    TestDeferLog* log;
    int id;
    int destroy_hits;
    int clone_hits;
}

fn void test_defer_on_destroy(void* ptr) {
    TestDeferArg* arg = (TestDeferArg*)ptr;
    arg.destroy_hits++;
    if (arg.log != null && arg.log.count < 8) {
        arg.log.order[(usz)arg.log.count] = arg.id;
        arg.log.count++;
    }
}

fn void test_defer_on_clone(void* ptr) {
    TestDeferArg* arg = (TestDeferArg*)ptr;
    arg.clone_hits++;
}

// --- Tests ---

fn bool test_stack_region() {
    StackRegion r = stack_region_alloc(DEFAULT_STACK_SIZE);
    if (r.base == null) return false;

    bool ok = r.total_size == DEFAULT_STACK_SIZE + GUARD_SIZE;
    ok = ok && r.usable_size == DEFAULT_STACK_SIZE;
    ok = ok && r.stack_top == (void*)((usz)r.base + r.total_size);

    // Verify usable area is writable
    char* mid = (char*)((usz)r.base + GUARD_SIZE + DEFAULT_STACK_SIZE / 2);
    *mid = 0xAB;
    ok = ok && *mid == (char)0xAB;

    stack_region_free(&r);
    return ok;
}

fn bool test_pool_lifecycle() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c1 = stack_ctx_create(&pool);
    if (c1 == null) return false;
    uint id1 = c1.id;

    stack_ctx_destroy(c1, &pool);
    if (pool.pool_size != 1) return false;

    StackCtx* c2 = stack_ctx_create(&pool);
    if (c2 == null) return false;
    if (pool.pool_size != 0) return false;

    bool same_ptr = (c2 == c1);
    bool new_id = (c2.id != id1);

    stack_ctx_destroy(c2, &pool);
    stack_pool_shutdown(&pool);
    return same_ptr && new_id;
}

fn bool test_pool_double_destroy_idempotent() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) return false;

    stack_ctx_destroy(c, &pool);
    if (pool.pool_size != 1) {
        stack_pool_shutdown(&pool);
        return false;
    }

    // Repeat destroy should be a no-op for already pooled contexts.
    stack_ctx_destroy(c, &pool);
    if (pool.pool_size != 1) {
        stack_pool_shutdown(&pool);
        return false;
    }

    StackCtx* reused = stack_ctx_create(&pool);
    bool ok = reused == c && pool.pool_size == 0;
    if (reused != null) {
        stack_ctx_destroy(reused, &pool);
    }
    stack_pool_shutdown(&pool);
    return ok;
}

