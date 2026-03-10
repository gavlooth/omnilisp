module main;

struct FiberTempScopeState {
    ScopeRegion* parent_scope;
    int ok;
    int bypass_escape;
    int eligible_slow;
    int ctx_returns;
}

fn void test_entry_scope_create_in_stack_ctx(void* arg) {
    FiberTempScopeState* state = (FiberTempScopeState*)arg;
    usz bypass_before = g_fiber_temp_pool_stats.bypass_escape_activity_allocs;
    usz eligible_before = g_fiber_temp_pool_stats.eligible_slow_allocs;
    usz ctx_returns_before = g_fiber_temp_pool_stats.ctx_return_count;

    ScopeRegion* child1 = scope_create(state.parent_scope);
    bool ok1 = true;
    for (usz i = 0; i < 32; i++) {
        if (child1.alloc(64) == null) {
            ok1 = false;
            break;
        }
    }
    scope_release(child1);

    ScopeRegion* child2 = scope_create(state.parent_scope);
    bool ok2 = true;
    void* e = child2.alloc_escape(32);
    if (e == null) ok2 = false;
    for (usz i = 0; i < 32; i++) {
        if (child2.alloc(64) == null) {
            ok2 = false;
            break;
        }
    }
    scope_release(child2);

    state.ok = (ok1 && ok2) ? 1 : 0;
    usz bypass_after = g_fiber_temp_pool_stats.bypass_escape_activity_allocs;
    usz eligible_after = g_fiber_temp_pool_stats.eligible_slow_allocs;
    usz ctx_returns_after = g_fiber_temp_pool_stats.ctx_return_count;
    state.bypass_escape = bypass_after > bypass_before ? 1 : 0;
    state.eligible_slow = eligible_after > eligible_before ? 1 : 0;
    state.ctx_returns = ctx_returns_after > ctx_returns_before ? 1 : 0;
}

fn bool test_stack_ctx_scope_create_in_context() {
    StackPool pool;
    stack_pool_init(&pool);

    ScopeRegion* parent_scope = scope_create(null);
    StackCtx* c = stack_ctx_create(&pool);
    if (c == null || parent_scope == null) {
        if (c != null) stack_ctx_destroy(c, &pool);
        if (parent_scope != null) scope_release(parent_scope);
        stack_pool_shutdown(&pool);
        return false;
    }

    FiberTempScopeState state;
    state.parent_scope = parent_scope;
    state.ok = 0;
    state.bypass_escape = 0;
    state.eligible_slow = 0;
    state.ctx_returns = 0;
    stack_ctx_init(c, &test_entry_scope_create_in_stack_ctx, &state);

    StackContext main_ctx;
    stack_ctx_switch_to(c, &main_ctx);
    bool ok = c.status == CTX_COMPLETED && state.ok == 1;
    if (scope_fiber_temp_flag_enabled()) {
        ok = ok && state.bypass_escape == 1;
        ok = ok && state.eligible_slow == 1;
        ok = ok && state.ctx_returns == 1;
    }

    scope_release(parent_scope);
    stack_ctx_destroy(c, &pool);
    stack_pool_shutdown(&pool);
    return ok;
}
