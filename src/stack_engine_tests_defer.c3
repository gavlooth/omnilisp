module main;

struct ShutdownForeignCtxProbe {
    StackCtx* target;
    bool callback_ran;
    bool target_owner_visible;
}

fn void test_shutdown_touch_foreign_ctx(void* ptr) {
    ShutdownForeignCtxProbe* probe = (ShutdownForeignCtxProbe*)ptr;
    probe.callback_ran = true;
    if (probe.target == null) return;
    // Regression guard: this read must remain valid while shutdown callbacks run.
    // Old single-pass shutdown could free target before this callback.
    probe.target_owner_visible = probe.target.owner_pool != null;
}

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

fn bool test_stack_pool_shutdown_defer_cross_ctx_order() {
    StackPool pool;
    stack_pool_init(&pool);

    // Create holder first, then target: shutdown walks all_list head-first,
    // so target is visited before holder callback runs.
    StackCtx* holder = stack_ctx_create(&pool);
    StackCtx* target = stack_ctx_create(&pool);
    if (holder == null || target == null) {
        stack_pool_shutdown(&pool);
        return false;
    }

    ShutdownForeignCtxProbe probe;
    probe.target = target;
    probe.callback_ran = false;
    probe.target_owner_visible = false;

    StackCtxDeferOps ops;
    ops.destroy_fn = &test_shutdown_touch_foreign_ctx;
    ops.clone_fn = null;

    bool ok = stack_ctx_defer(holder, &ops, &probe);
    stack_pool_shutdown(&pool);
    ok = ok && probe.callback_ran;
    ok = ok && probe.target_owner_visible;
    return ok;
}

fn bool test_stack_ctx_entry_reserve_rejects_byte_overflow() {
    StackCtxDeferEntry[STACK_CTX_DEFER_INLINE_CAP] inline_entries;
    StackCtxDeferEntry* heap_entries = null;

    usz inline_capacity = STACK_CTX_DEFER_INLINE_CAP;
    usz target_cap = inline_capacity + 1;
    if (!stack_ctx_entry_reserve(&inline_entries[0], &heap_entries, 0, &inline_capacity, STACK_CTX_DEFER_INLINE_CAP, target_cap, false)) {
        return false;
    }
    usz first_capacity = inline_capacity;

    // Build an impossible element count that should overflow byte-size math.
    usz max_cap_items = usz.max / StackCtxDeferEntry.sizeof;
    if (max_cap_items == usz.max) return false;
    usz overflow_items = max_cap_items + 1;

    bool rejected = !stack_ctx_entry_reserve(&inline_entries[0], &heap_entries, 0, &inline_capacity, STACK_CTX_DEFER_INLINE_CAP, overflow_items, false);

    if (heap_entries != null) {
        mem::free(heap_entries);
    }

    return rejected && inline_capacity == first_capacity;
}
