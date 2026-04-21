module main;

fn void scope_chunk_reclaim_temp_global(ScopeChunk* chunk) {
    if (chunk == null) return;
    scope_global_lock();
    if (g_fiber_temp_chunk_pool_count < FIBER_TEMP_CHUNK_POOL_MAX) {
        chunk.next = g_fiber_temp_chunk_pool;
        g_fiber_temp_chunk_pool = chunk;
        g_fiber_temp_chunk_pool_count++;
        g_fiber_temp_pool_stats.return_count++;
        fiber_temp_pool_record_peak();
        scope_global_unlock();
        return;
    }
    g_fiber_temp_pool_stats.drop_frees++;
    scope_global_unlock();
    mem::free(chunk);
}

fn ScopeChunk* fiber_temp_chunk_try_take(usz min_bytes) {
    if (!scope_fiber_temp_flag_enabled()) return null;

    FiberTempCtxPool* ctx_pool = fiber_temp_ctx_pool_find_current();
    if (ctx_pool != null) {
        ScopeChunk* local = fiber_temp_chunk_take_from_ctx_pool(ctx_pool, min_bytes);
        if (local != null) {
            g_fiber_temp_pool_stats.take_hits++;
            g_fiber_temp_pool_stats.ctx_take_hits++;
            return local;
        }
    }

    scope_global_lock();
    ScopeChunk* prev = null;
    ScopeChunk* cur = g_fiber_temp_chunk_pool;
    while (cur != null) {
        if (cur.capacity >= min_bytes) {
            ScopeChunk* next = cur.next;
            if (prev != null) {
                prev.next = next;
            } else {
                g_fiber_temp_chunk_pool = next;
            }
            g_fiber_temp_chunk_pool_count--;
            g_fiber_temp_pool_stats.take_hits++;
            scope_global_unlock();
            cur.next = null;
            return cur;
        }
        prev = cur;
        cur = cur.next;
    }
    g_fiber_temp_pool_stats.take_misses++;
    scope_global_unlock();
    return null;
}

fn void scope_chunk_reclaim_temp(ScopeChunk* chunk) {
    if (chunk == null) return;
    if (!scope_fiber_temp_flag_enabled()) {
        mem::free(chunk);
        return;
    }

    FiberTempCtxPool* ctx_pool = fiber_temp_ctx_pool_get_or_create();
    if (ctx_pool != null && ctx_pool.count < FIBER_TEMP_CTX_POOL_MAX) {
        chunk.next = ctx_pool.chunks;
        ctx_pool.chunks = chunk;
        ctx_pool.count++;
        g_fiber_temp_pool_stats.return_count++;
        g_fiber_temp_pool_stats.ctx_return_count++;
        return;
    }

    scope_chunk_reclaim_temp_global(chunk);
}

fn void scope_temp_chunk_release(ScopeRegion* scope, ScopeChunk* chunk) @inline {
    scope_telemetry_note_temp_chunk_release(chunk);
    if (scope != null && scope.fiber_temp_eligible) {
        scope_chunk_reclaim_temp(chunk);
        return;
    }
    mem::free(chunk);
}
