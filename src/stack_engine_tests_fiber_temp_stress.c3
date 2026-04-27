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
    FiberTempPoolStats fiber_before = fiber_temp_pool_stats_snapshot();

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
        FiberTempPoolStats fiber_after = fiber_temp_pool_stats_snapshot();
        ok = ok && fiber_after.ctx_return_count > fiber_before.ctx_return_count;
        ok = ok && fiber_after.lifecycle_clone_callbacks > fiber_before.lifecycle_clone_callbacks;
        ok = ok && fiber_after.lifecycle_destroy_callbacks > fiber_before.lifecycle_destroy_callbacks;
        ok = ok && fiber_after.lifecycle_destroy_deferred > fiber_before.lifecycle_destroy_deferred;
        ok = ok && fiber_after.lifecycle_destroy_flush_chunks > fiber_before.lifecycle_destroy_flush_chunks;
    }

    scope_release(parent_scope);
    stack_pool_shutdown(&pool);
    return ok;
}
