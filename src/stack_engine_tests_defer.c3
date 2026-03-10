module main;

fn bool test_stack_ctx_defer_destroy_lifo() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    StackCtxDeferOps ops;
    ops.destroy_fn = &test_defer_on_destroy;
    ops.clone_fn = null;

    TestDeferLog log;
    log.count = 0;

    TestDeferArg a;
    TestDeferArg b;
    TestDeferArg d;
    a.log = &log; a.id = 1; a.destroy_hits = 0; a.clone_hits = 0;
    b.log = &log; b.id = 2; b.destroy_hits = 0; b.clone_hits = 0;
    d.log = &log; d.id = 3; d.destroy_hits = 0; d.clone_hits = 0;

    bool ok = stack_ctx_defer(c, &ops, &a);
    ok = ok && stack_ctx_defer(c, &ops, &b);
    ok = ok && stack_ctx_defer(c, &ops, &d);

    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);

    ok = ok && a.destroy_hits == 1 && b.destroy_hits == 1 && d.destroy_hits == 1;
    ok = ok && log.count == 3;
    ok = ok && log.order[0] == 3 && log.order[1] == 2 && log.order[2] == 1;
    return ok;
}

fn bool test_stack_ctx_defer_undefer_pop() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) { stack_pool_shutdown(&pool); return false; }

    StackCtxDeferOps ops;
    ops.destroy_fn = &test_defer_on_destroy;
    ops.clone_fn = null;

    TestDeferLog log;
    log.count = 0;

    TestDeferArg keep;
    TestDeferArg popd;
    keep.log = &log; keep.id = 1; keep.destroy_hits = 0; keep.clone_hits = 0;
    popd.log = &log; popd.id = 2; popd.destroy_hits = 0; popd.clone_hits = 0;

    bool ok = stack_ctx_defer(c, &ops, &keep);
    ok = ok && stack_ctx_defer(c, &ops, &popd);
    ok = ok && stack_ctx_undefer(c);
    ok = ok && !stack_ctx_undefer(null);

    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);

    ok = ok && keep.destroy_hits == 1;
    ok = ok && popd.destroy_hits == 0;
    ok = ok && log.count == 1 && log.order[0] == 1;
    return ok;
}

fn bool test_stack_ctx_defer_clone_hook() {
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
    arg.id = 7;
    arg.destroy_hits = 0;
    arg.clone_hits = 0;

    bool ok = stack_ctx_defer(source, &ops, &arg);
    StackCtx* clone = stack_ctx_clone(source, &pool);
    ok = ok && clone != null;
    ok = ok && arg.clone_hits == 1;

    if (clone != null) {
        stack_ctx_destroy(clone, &pool);
    }
    stack_ctx_destroy(source, &pool);
    stack_pool_shutdown(&pool);

    // One destroy from clone + one destroy from source.
    ok = ok && arg.destroy_hits == 2;
    return ok;
}
