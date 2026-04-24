module main;

import std::io;

struct ScopeTransferStats {
    usz splice_calls;
    usz splice_escape_chunk_bytes;
    usz splice_temp_chunk_bytes_freed;
    usz splice_temp_dtors_run;
    usz splice_escape_dtors_transferred;
}

ScopeTransferStats g_scope_transfer_stats;

struct ScopeMemoryTelemetryStats {
    usz total_temp_bytes_allocated;
    usz total_escape_bytes_allocated;
    usz temp_chunk_bytes_allocated;
    usz escape_chunk_bytes_allocated;
    usz temp_bytes_freed;
    usz escape_bytes_freed;
    usz temp_slow_alloc_count;
    usz escape_slow_alloc_count;
    usz temp_slow_requested_bytes;
    usz escape_slow_requested_bytes;
    usz temp_slow_selected_chunk_bytes;
    usz escape_slow_selected_chunk_bytes;
    usz temp_live_chunk_count;
    usz escape_live_chunk_count;
    usz temp_peak_live_chunk_count;
    usz escape_peak_live_chunk_count;
    usz temp_unused_bytes_at_reset;
    usz escape_unused_bytes_at_reset;
    usz temp_unused_bytes_at_destroy;
    usz escape_unused_bytes_at_destroy;
    usz scope_create_count;
    usz scope_release_count;
    usz scope_destroy_count;
    usz scope_recycle_count;
    usz scope_fresh_alloc_count;
    usz scope_recycle_hit_count;
}

ScopeMemoryTelemetryStats g_scope_memory_telemetry_stats;

struct FiberTempPoolStats {
    usz take_hits;
    usz take_misses;
    usz return_count;
    usz drop_frees;
    usz peak_pool_count;
    usz ctx_take_hits;
    usz ctx_return_count;
    usz ctx_pool_created;
    usz take_hit_chunk_bytes;
    usz return_chunk_bytes;
    usz drop_free_chunk_bytes;
    usz ctx_take_hit_chunk_bytes;
    usz ctx_return_chunk_bytes;
    usz lifecycle_clone_callbacks;
    usz lifecycle_destroy_callbacks;
    usz lifecycle_destroy_deferred;
    usz lifecycle_destroy_flush_chunks;
    usz eligible_slow_allocs;
    usz bypass_large_allocs;
    usz bypass_escape_activity_allocs;
}

struct FiberTempCtxPool {
    ScopeChunk* chunks;
    usz         count;
    uint        refcount;
}

ScopeChunk* g_fiber_temp_chunk_pool = null;
usz g_fiber_temp_chunk_pool_count = 0;
const usz FIBER_TEMP_CHUNK_POOL_MAX = 2048;
const usz FIBER_TEMP_CTX_POOL_MAX = 64;
const usz FIBER_TEMP_ELIGIBLE_MAX_ALLOC = 4096;
FiberTempPoolStats g_fiber_temp_pool_stats;

fn bool scope_test_quiet_requested() @inline {
    return scope_c_getenv("OMNI_TEST_QUIET") != null ||
        (scope_c_getenv("OMNI_TEST_SUMMARY") != null && scope_c_getenv("OMNI_TEST_VERBOSE") == null);
}

fn bool scope_fiber_temp_flag_enabled() @inline {
    return scope_c_getenv("OMNI_FIBER_TEMP") != null;
}

fn bool scope_fiber_temp_context_eligible() @inline {
    if (!scope_fiber_temp_flag_enabled()) return false;
    return g_current_stack_ctx != null;
}

fn bool scope_memory_shape_telemetry_enabled() @inline {
    $if $feature(OMNI_BOUNDARY_INSTR_COUNTERS):
        return true;
    $else
        $if $feature(OMNI_BOUNDARY_INSTR_TRACE):
            return true;
        $else
            $if $feature(OMNI_BOUNDARY_INSTR_BENCHMARK):
                return true;
            $else
                return false;
            $endif
        $endif
    $endif
}

fn bool fiber_temp_alloc_size_eligible(usz aligned_size) @inline {
    return aligned_size <= FIBER_TEMP_ELIGIBLE_MAX_ALLOC;
}

fn bool fiber_temp_scope_shape_eligible(ScopeRegion* scope) @inline {
    return scope.escape_chunks == null && scope.escape_dtors == null;
}

fn void fiber_temp_pool_record_peak() @inline {
    if (g_fiber_temp_chunk_pool_count > g_fiber_temp_pool_stats.peak_pool_count) {
        g_fiber_temp_pool_stats.peak_pool_count = g_fiber_temp_chunk_pool_count;
    }
}

fn usz scope_chunk_bytes(ScopeChunk* chunk) @inline {
    usz total = 0;
    while (chunk != null) {
        total += chunk.capacity;
        chunk = chunk.next;
    }
    return total;
}

fn usz scope_dtor_count(ScopeDtor* dtor) @inline {
    usz total = 0;
    while (dtor != null) {
        total++;
        dtor = dtor.next;
    }
    return total;
}

fn void scope_telemetry_note_temp_alloc(usz aligned_size) @inline {
    g_scope_memory_telemetry_stats.total_temp_bytes_allocated += aligned_size;
}

fn void scope_telemetry_note_escape_alloc(usz aligned_size) @inline {
    g_scope_memory_telemetry_stats.total_escape_bytes_allocated += aligned_size;
}

fn void scope_telemetry_note_temp_chunk_alloc(ScopeChunk* chunk) @inline {
    if (chunk == null) return;
    g_scope_memory_telemetry_stats.temp_chunk_bytes_allocated += chunk.capacity;
    if (!scope_memory_shape_telemetry_enabled()) return;
    g_scope_memory_telemetry_stats.temp_live_chunk_count++;
    if (g_scope_memory_telemetry_stats.temp_live_chunk_count > g_scope_memory_telemetry_stats.temp_peak_live_chunk_count) {
        g_scope_memory_telemetry_stats.temp_peak_live_chunk_count = g_scope_memory_telemetry_stats.temp_live_chunk_count;
    }
}

fn void scope_telemetry_note_escape_chunk_alloc(ScopeChunk* chunk) @inline {
    if (chunk == null) return;
    g_scope_memory_telemetry_stats.escape_chunk_bytes_allocated += chunk.capacity;
    if (!scope_memory_shape_telemetry_enabled()) return;
    g_scope_memory_telemetry_stats.escape_live_chunk_count++;
    if (g_scope_memory_telemetry_stats.escape_live_chunk_count > g_scope_memory_telemetry_stats.escape_peak_live_chunk_count) {
        g_scope_memory_telemetry_stats.escape_peak_live_chunk_count = g_scope_memory_telemetry_stats.escape_live_chunk_count;
    }
}

fn void scope_telemetry_note_temp_chunk_release(ScopeChunk* chunk) @inline {
    if (chunk == null) return;
    g_scope_memory_telemetry_stats.temp_bytes_freed += chunk.capacity;
    if (!scope_memory_shape_telemetry_enabled()) return;
    if (g_scope_memory_telemetry_stats.temp_live_chunk_count > 0) {
        g_scope_memory_telemetry_stats.temp_live_chunk_count--;
    }
}

fn void scope_telemetry_note_escape_chunk_release(ScopeChunk* chunk) @inline {
    if (chunk == null) return;
    g_scope_memory_telemetry_stats.escape_bytes_freed += chunk.capacity;
    if (!scope_memory_shape_telemetry_enabled()) return;
    if (g_scope_memory_telemetry_stats.escape_live_chunk_count > 0) {
        g_scope_memory_telemetry_stats.escape_live_chunk_count--;
    }
}

fn void scope_telemetry_note_temp_slow_alloc_request(usz requested, usz selected_capacity) @inline {
    if (!scope_memory_shape_telemetry_enabled()) return;
    g_scope_memory_telemetry_stats.temp_slow_alloc_count++;
    g_scope_memory_telemetry_stats.temp_slow_requested_bytes += requested;
    g_scope_memory_telemetry_stats.temp_slow_selected_chunk_bytes += selected_capacity;
}

fn void scope_telemetry_note_escape_slow_alloc_request(usz requested, usz selected_capacity) @inline {
    if (!scope_memory_shape_telemetry_enabled()) return;
    g_scope_memory_telemetry_stats.escape_slow_alloc_count++;
    g_scope_memory_telemetry_stats.escape_slow_requested_bytes += requested;
    g_scope_memory_telemetry_stats.escape_slow_selected_chunk_bytes += selected_capacity;
}

fn usz scope_temp_current_unused_bytes(ScopeRegion* scope) @inline {
    if (scope == null || scope.bump == null || scope.limit == null) return 0;
    if ((usz)scope.limit < (usz)scope.bump) return 0;
    return (usz)(scope.limit - scope.bump);
}

fn usz scope_escape_current_unused_bytes(ScopeRegion* scope) @inline {
    if (scope == null || scope.escape_bump == null || scope.escape_limit == null) return 0;
    if ((usz)scope.escape_limit < (usz)scope.escape_bump) return 0;
    return (usz)(scope.escape_limit - scope.escape_bump);
}

fn void scope_telemetry_note_scope_unused_at_reset(ScopeRegion* scope, bool include_escape) @inline {
    if (!scope_memory_shape_telemetry_enabled()) return;
    g_scope_memory_telemetry_stats.temp_unused_bytes_at_reset += scope_temp_current_unused_bytes(scope);
    if (include_escape) {
        g_scope_memory_telemetry_stats.escape_unused_bytes_at_reset += scope_escape_current_unused_bytes(scope);
    }
}

fn void scope_telemetry_note_scope_unused_at_destroy(ScopeRegion* scope) @inline {
    if (!scope_memory_shape_telemetry_enabled()) return;
    g_scope_memory_telemetry_stats.temp_unused_bytes_at_destroy += scope_temp_current_unused_bytes(scope);
    g_scope_memory_telemetry_stats.escape_unused_bytes_at_destroy += scope_escape_current_unused_bytes(scope);
}

fn void scope_telemetry_note_scope_fresh_alloc() @inline {
    if (!scope_memory_shape_telemetry_enabled()) return;
    g_scope_memory_telemetry_stats.scope_fresh_alloc_count++;
}

fn void scope_telemetry_note_scope_recycle_hit() @inline {
    if (!scope_memory_shape_telemetry_enabled()) return;
    g_scope_memory_telemetry_stats.scope_recycle_hit_count++;
}

fn void fiber_temp_telemetry_note_take_hit(ScopeChunk* chunk, bool from_ctx) @inline {
    if (chunk == null || !scope_memory_shape_telemetry_enabled()) return;
    g_fiber_temp_pool_stats.take_hit_chunk_bytes += chunk.capacity;
    if (from_ctx) g_fiber_temp_pool_stats.ctx_take_hit_chunk_bytes += chunk.capacity;
}

fn void fiber_temp_telemetry_note_return(ScopeChunk* chunk, bool to_ctx) @inline {
    if (chunk == null || !scope_memory_shape_telemetry_enabled()) return;
    g_fiber_temp_pool_stats.return_chunk_bytes += chunk.capacity;
    if (to_ctx) g_fiber_temp_pool_stats.ctx_return_chunk_bytes += chunk.capacity;
}

fn void fiber_temp_telemetry_note_drop_free(ScopeChunk* chunk) @inline {
    if (chunk == null || !scope_memory_shape_telemetry_enabled()) return;
    g_fiber_temp_pool_stats.drop_free_chunk_bytes += chunk.capacity;
}

fn void scope_print_memory_stats() {
    if (scope_test_quiet_requested()) return;
    io::printn("=== Scope Transfer Stats ===");
    io::printfn("scope_splice_escapes calls: %d", g_scope_transfer_stats.splice_calls);
    io::printfn("scope_splice escape bytes moved: %d", g_scope_transfer_stats.splice_escape_chunk_bytes);
    io::printfn("scope_splice temp bytes freed: %d", g_scope_transfer_stats.splice_temp_chunk_bytes_freed);
    io::printfn("scope_splice temp dtors run: %d", g_scope_transfer_stats.splice_temp_dtors_run);
    io::printfn("scope_splice escape dtors moved: %d", g_scope_transfer_stats.splice_escape_dtors_transferred);
}
