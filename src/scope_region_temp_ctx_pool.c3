module main;

fn void fiber_temp_ctx_pool_on_clone(void* arg) {
    FiberTempCtxPool* pool = (FiberTempCtxPool*)arg;
    if (pool == null) return;
    scope_telemetry_inc(&g_fiber_temp_pool_stats.lifecycle_clone_callbacks);
    pool.refcount++;
}

fn void fiber_temp_ctx_pool_on_destroy(void* arg) {
    FiberTempCtxPool* pool = (FiberTempCtxPool*)arg;
    if (pool == null) return;
    scope_telemetry_inc(&g_fiber_temp_pool_stats.lifecycle_destroy_callbacks);
    if (pool.refcount > 1) {
        scope_telemetry_inc(&g_fiber_temp_pool_stats.lifecycle_destroy_deferred);
        pool.refcount--;
        return;
    }

    ScopeChunk* chunk = pool.chunks;
    pool.chunks = null;
    pool.count = 0;
    while (chunk != null) {
        ScopeChunk* next = chunk.next;
        scope_telemetry_inc(&g_fiber_temp_pool_stats.lifecycle_destroy_flush_chunks);
        scope_chunk_reclaim_temp_global(chunk);
        chunk = next;
    }
    mem::free(pool);
}

fn FiberTempCtxPool* fiber_temp_ctx_pool_find_current() @inline {
    if (g_current_stack_ctx == null) return null;
    return (FiberTempCtxPool*)stack_ctx_find_lifecycle_arg(
        g_current_stack_ctx,
        &fiber_temp_ctx_pool_on_destroy,
        &fiber_temp_ctx_pool_on_clone
    );
}

fn FiberTempCtxPool* fiber_temp_ctx_pool_get_or_create() {
    if (g_current_stack_ctx == null) return null;
    FiberTempCtxPool* existing = fiber_temp_ctx_pool_find_current();
    if (existing != null) return existing;

    FiberTempCtxPool* pool = (FiberTempCtxPool*)mem::malloc(FiberTempCtxPool.sizeof);
    if (pool == null) return null;
    pool.chunks = null;
    pool.count = 0;
    pool.refcount = 1;

    StackCtxDeferOps ops;
    ops.destroy_fn = &fiber_temp_ctx_pool_on_destroy;
    ops.clone_fn = &fiber_temp_ctx_pool_on_clone;
    if (!stack_ctx_lifecycle_attach(g_current_stack_ctx, &ops, (void*)pool)) {
        mem::free(pool);
        return null;
    }
    scope_telemetry_inc(&g_fiber_temp_pool_stats.ctx_pool_created);
    return pool;
}

fn ScopeChunk* fiber_temp_chunk_take_from_ctx_pool(FiberTempCtxPool* pool, usz min_bytes) {
    if (pool == null) return null;
    ScopeChunk* prev = null;
    ScopeChunk* cur = pool.chunks;
    while (cur != null) {
        if (cur.capacity >= min_bytes) {
            ScopeChunk* next = cur.next;
            if (prev != null) {
                prev.next = next;
            } else {
                pool.chunks = next;
            }
            pool.count--;
            cur.next = null;
            return cur;
        }
        prev = cur;
        cur = cur.next;
    }
    return null;
}
