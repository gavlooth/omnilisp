module main;

fn bool test_stack_ctx_defer_update_arg() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    StackCtxDeferOps ops;
    ops.destroy_fn = &test_defer_on_destroy;
    ops.clone_fn = null;

    TestDeferLog log;
    log.count = 0;

    TestDeferArg first;
    TestDeferArg second;
    first.log = &log; first.id = 1; first.destroy_hits = 0; first.clone_hits = 0;
    second.log = &log; second.id = 2; second.destroy_hits = 0; second.clone_hits = 0;

    usz slot = c.defer_count;
    bool ok = stack_ctx_defer(c, &ops, &first);
    ok = ok && stack_ctx_defer_update_arg(c, slot, &second);
    ok = ok && !stack_ctx_defer_update_arg(c, 42, &second);

    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);

    ok = ok && first.destroy_hits == 0;
    ok = ok && second.destroy_hits == 1;
    ok = ok && log.count == 1 && log.order[0] == 2;
    return ok;
}

fn bool test_stack_ctx_defer_update_arg_clone_isolation() {
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
    ops.clone_fn = null;

    TestDeferLog source_log;
    TestDeferLog clone_log;
    source_log.count = 0;
    clone_log.count = 0;

    TestDeferArg source_arg;
    TestDeferArg clone_arg;
    source_arg.log = &source_log; source_arg.id = 11; source_arg.destroy_hits = 0; source_arg.clone_hits = 0;
    clone_arg.log = &clone_log; clone_arg.id = 22; clone_arg.destroy_hits = 0; clone_arg.clone_hits = 0;

    usz slot = source.defer_count;
    bool ok = stack_ctx_defer(source, &ops, &source_arg);

    StackCtx* clone = stack_ctx_clone(source, &pool);
    ok = ok && clone != null;
    if (clone == null) {
        stack_ctx_destroy(source, &pool);
        stack_pool_shutdown(&pool);
        return false;
    }

    ok = ok && stack_ctx_defer_update_arg(clone, slot, &clone_arg);

    stack_ctx_destroy(clone, &pool);
    stack_ctx_destroy(source, &pool);
    stack_pool_shutdown(&pool);

    ok = ok && source_arg.destroy_hits == 1;
    ok = ok && clone_arg.destroy_hits == 1;
    ok = ok && source_log.count == 1 && source_log.order[0] == 11;
    ok = ok && clone_log.count == 1 && clone_log.order[0] == 22;
    return ok;
}
