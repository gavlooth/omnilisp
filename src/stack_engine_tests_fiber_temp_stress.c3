module main;

fn void test_entry_scope_yield_once(void* arg) {
    FiberTempScopeState* state = (FiberTempScopeState*)arg;

    ScopeRegion* child1 = scope_create(state.parent_scope);
    bool ok1 = true;
    for (usz i = 0; i < 32; i++) {
        if (child1.alloc(64) == null) {
            ok1 = false;
            break;
        }
    }
    scope_release(child1);
    if (!ok1) {
        state.ok = 0;
        return;
    }

    stack_ctx_suspend();

    ScopeRegion* child2 = scope_create(state.parent_scope);
    bool ok2 = true;
    for (usz i = 0; i < 32; i++) {
        if (child2.alloc(64) == null) {
            ok2 = false;
            break;
        }
    }
    scope_release(child2);
    state.ok = ok2 ? 1 : 0;
}

fn bool test_stack_ctx_fiber_temp_clone_discard_stress() {
    StackPool pool;
    stack_pool_init(&pool);

    ScopeRegion* parent_scope = scope_create(null);
    if (parent_scope == null) {
        stack_pool_shutdown(&pool);
        return false;
    }

    bool ok = true;
    usz ctx_returns_before = g_fiber_temp_pool_stats.ctx_return_count;
    usz lifecycle_clone_before = g_fiber_temp_pool_stats.lifecycle_clone_callbacks;
    usz lifecycle_destroy_before = g_fiber_temp_pool_stats.lifecycle_destroy_callbacks;
    usz lifecycle_deferred_before = g_fiber_temp_pool_stats.lifecycle_destroy_deferred;
    usz lifecycle_flush_before = g_fiber_temp_pool_stats.lifecycle_destroy_flush_chunks;

    for (usz iter = 0; iter < 32; iter++) {
        StackCtx* source = stack_ctx_create(&pool);
        if (source == null) {
            ok = false;
            break;
        }

        FiberTempScopeState state;
        state.parent_scope = parent_scope;
        state.ok = 0;
        state.bypass_escape = 0;
        state.eligible_slow = 0;
        state.ctx_returns = 0;
        stack_ctx_init(source, &test_entry_scope_yield_once, &state);

        StackContext main_ctx;
        stack_ctx_switch_to(source, &main_ctx);
        if (source.status != CTX_SUSPENDED) {
            ok = false;
            stack_ctx_destroy(source, &pool);
            break;
        }

        StackCtx* clone = stack_ctx_clone(source, &pool);
        if (clone == null) {
            ok = false;
            stack_ctx_destroy(source, &pool);
            break;
        }

        // Discard clone immediately; source must remain valid.
        stack_ctx_destroy(clone, &pool);
        stack_ctx_resume(source, &main_ctx);
        if (source.status != CTX_COMPLETED || state.ok != 1) {
            ok = false;
            stack_ctx_destroy(source, &pool);
            break;
        }

        stack_ctx_destroy(source, &pool);
    }

    if (scope_fiber_temp_flag_enabled()) {
        usz ctx_returns_after = g_fiber_temp_pool_stats.ctx_return_count;
        usz lifecycle_clone_after = g_fiber_temp_pool_stats.lifecycle_clone_callbacks;
        usz lifecycle_destroy_after = g_fiber_temp_pool_stats.lifecycle_destroy_callbacks;
        usz lifecycle_deferred_after = g_fiber_temp_pool_stats.lifecycle_destroy_deferred;
        usz lifecycle_flush_after = g_fiber_temp_pool_stats.lifecycle_destroy_flush_chunks;
        ok = ok && ctx_returns_after > ctx_returns_before;
        ok = ok && lifecycle_clone_after > lifecycle_clone_before;
        ok = ok && lifecycle_destroy_after > lifecycle_destroy_before;
        ok = ok && lifecycle_deferred_after > lifecycle_deferred_before;
        ok = ok && lifecycle_flush_after > lifecycle_flush_before;
    }

    scope_release(parent_scope);
    stack_pool_shutdown(&pool);
    return ok;
}
