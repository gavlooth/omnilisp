module main;

fn bool test_stack_ctx_lifecycle_destroy_isolation() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    StackCtxDeferOps ops;
    ops.destroy_fn = &test_defer_on_destroy;
    ops.clone_fn = &test_defer_on_clone;

    TestDeferLog life_log;
    TestDeferLog defer_log;
    life_log.count = 0;
    defer_log.count = 0;

    TestDeferArg life_arg;
    TestDeferArg defer_arg;
    life_arg.log = &life_log; life_arg.id = 31; life_arg.destroy_hits = 0; life_arg.clone_hits = 0;
    defer_arg.log = &defer_log; defer_arg.id = 41; defer_arg.destroy_hits = 0; defer_arg.clone_hits = 0;

    bool ok = stack_ctx_lifecycle_attach(c, &ops, &life_arg);
    ok = ok && stack_ctx_defer(c, &ops, &defer_arg);
    ok = ok && stack_ctx_undefer(c);

    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);

    ok = ok && life_arg.destroy_hits == 1;
    ok = ok && defer_arg.destroy_hits == 0;
    ok = ok && life_log.count == 1 && life_log.order[0] == 31;
    return ok;
}

fn bool test_stack_ctx_lifecycle_clone_hook() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* source = stack_ctx_create(&pool);
    if (source == null) { stack_pool_shutdown(&pool); return false; }

    TestState state;
    state.value = 0;
    state.step = 0;
    stack_ctx_init(source, &test_entry_generator, &state);

    StackContext main_ctx;
    stack_ctx_switch_to(source, &main_ctx);  // suspend once
    if (source.status != CTX_SUSPENDED) {
        stack_ctx_destroy(source, &pool);
        stack_pool_shutdown(&pool);
        return false;
    }

    StackCtxDeferOps ops;
    ops.destroy_fn = &test_defer_on_destroy;
    ops.clone_fn = &test_defer_on_clone;

    TestDeferLog log;
    log.count = 0;
    TestDeferArg arg;
    arg.log = &log;
    arg.id = 51;
    arg.destroy_hits = 0;
    arg.clone_hits = 0;

    bool ok = stack_ctx_lifecycle_attach(source, &ops, &arg);
    StackCtx* clone = stack_ctx_clone(source, &pool);
    ok = ok && clone != null;
    ok = ok && arg.clone_hits == 1;

    if (clone != null) stack_ctx_destroy(clone, &pool);
    stack_ctx_destroy(source, &pool);
    stack_pool_shutdown(&pool);

    ok = ok && arg.destroy_hits == 2;
    return ok;
}
