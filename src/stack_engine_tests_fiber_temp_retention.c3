module main;

fn bool test_stack_ctx_fiber_temp_retention_guard() {
    if (!scope_fiber_temp_flag_enabled()) return true;

    StackPool pool;
    stack_pool_init(&pool);

    ScopeRegion* parent_scope = scope_create(null);
    if (parent_scope == null) {
        stack_pool_shutdown(&pool);
        return false;
    }

    bool ok = true;
    usz pooled_before = fiber_temp_chunk_pool_count_snapshot();
    FiberTempPoolStats fiber_before = fiber_temp_pool_stats_snapshot();

    for (usz iter = 0; iter < 128; iter++) {
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

        stack_ctx_destroy(clone, &pool);
        stack_ctx_resume(source, &main_ctx);
        if (source.status != CTX_COMPLETED || state.ok != 1) {
            ok = false;
            stack_ctx_destroy(source, &pool);
            break;
        }

        stack_ctx_destroy(source, &pool);
    }

    usz pooled_after = fiber_temp_chunk_pool_count_snapshot();
    FiberTempPoolStats fiber_after = fiber_temp_pool_stats_snapshot();

    ok = ok && fiber_after.ctx_pool_created > fiber_before.ctx_pool_created;
    ok = ok && fiber_after.lifecycle_destroy_flush_chunks > fiber_before.lifecycle_destroy_flush_chunks;
    // Retention guard: repeated clone/discard cycles should not cause runaway
    // growth in global pooled chunks.
    ok = ok && pooled_after <= pooled_before + 64;

    scope_release(parent_scope);
    stack_pool_shutdown(&pool);
    return ok;
}
