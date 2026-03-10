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

struct FiberTempPoolStats {
    usz take_hits;
    usz take_misses;
    usz return_count;
    usz drop_frees;
    usz peak_pool_count;
    usz ctx_take_hits;
    usz ctx_return_count;
    usz ctx_pool_created;
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

fn bool scope_fiber_temp_flag_enabled() @inline {
    return scope_c_getenv("OMNI_FIBER_TEMP") != null;
}

fn bool scope_fiber_temp_context_eligible() @inline {
    if (!scope_fiber_temp_flag_enabled()) return false;
    return g_current_stack_ctx != null;
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

fn void scope_print_memory_stats() {
    io::printn("=== Scope Transfer Stats ===");
    io::printfn("scope_splice_escapes calls: %d", (long)g_scope_transfer_stats.splice_calls);
    io::printfn("scope_splice escape bytes moved: %d", (long)g_scope_transfer_stats.splice_escape_chunk_bytes);
    io::printfn("scope_splice temp bytes freed: %d", (long)g_scope_transfer_stats.splice_temp_chunk_bytes_freed);
    io::printfn("scope_splice temp dtors run: %d", (long)g_scope_transfer_stats.splice_temp_dtors_run);
    io::printfn("scope_splice escape dtors moved: %d", (long)g_scope_transfer_stats.splice_escape_dtors_transferred);
}
