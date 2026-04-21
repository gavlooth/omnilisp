module main;

// =============================================================================
// Scope adoption (merge child memory into parent)
// =============================================================================

fn void scope_splice_escapes(ScopeRegion* parent, ScopeRegion* child) {
    if (child == null || parent == null) return;
    scope_guard_owner(parent, "scope_splice_escapes(parent)");
    scope_guard_owner(child, "scope_splice_escapes(child)");
    scope_invariant_distinct_scopes(parent, child);
    scope_assert_escape_tail_consistency(parent);
    scope_assert_escape_tail_consistency(child);

    g_scope_transfer_stats.splice_calls++;
    g_scope_transfer_stats.splice_escape_chunk_bytes += scope_chunk_bytes(child.escape_chunks);
    g_scope_transfer_stats.splice_temp_chunk_bytes_freed += scope_chunk_bytes(child.chunks);
    g_scope_transfer_stats.splice_temp_dtors_run += scope_dtor_count(child.dtors);
    g_scope_transfer_stats.splice_escape_dtors_transferred += scope_dtor_count(child.escape_dtors);

    // Move child's ESCAPE dtors to parent's ESCAPE dtor list.
    if (child.escape_dtors != null) {
        ScopeDtor* parent_head = parent.escape_dtors;
        if (parent_head == null) {
            parent.escape_dtors_tail = child.escape_dtors_tail;
        } else {
            assert(child.escape_dtors_tail != null, "scope_splice_escapes:child dtors tail missing");
            child.escape_dtors_tail.next = parent_head;
        }
        parent.escape_dtors = child.escape_dtors;
        child.escape_dtors = null;
        child.escape_dtors_tail = null;
    }

    // Move child's ESCAPE chunks to parent's ESCAPE chunk list.
    if (child.escape_chunks != null) {
        ScopeChunk* parent_head = parent.escape_chunks;
        if (parent_head == null) {
            parent.escape_chunks_tail = child.escape_chunks_tail;
        } else {
            assert(child.escape_chunks_tail != null, "scope_splice_escapes:child chunks tail missing");
            child.escape_chunks_tail.next = parent_head;
        }
        parent.escape_chunks = child.escape_chunks;
        // Parent adopts child's active ESCAPE head chunk and allocation cursor.
        // Keep bump/limit aligned with the new head to preserve lane shape.
        parent.escape_bump = child.escape_bump;
        parent.escape_limit = child.escape_limit;
        child.escape_chunks = null;
        child.escape_chunks_tail = null;
    }

    scope_assert_escape_tail_consistency(parent);
    scope_assert_escape_tail_consistency(child);

    // Destroy TEMP lane immediately.
    scope_run_dtors(child);
    child.dtors = null;

    ScopeChunk* c = child.chunks;
    while (c != null) {
        ScopeChunk* next = c.next;
        scope_temp_chunk_release(child, c);
        c = next;
    }
    child.chunks = null;
    child.bump = null;
    child.limit = null;
    child.escape_bump = null;
    child.escape_limit = null;
    child.escape_chunks_tail = null;
    child.escape_dtors_tail = null;
    child.fiber_temp_eligible = false;
    scope_assert_escape_tail_consistency(child);

    // Detach and release child scope ownership.
    if (child.parent != null) {
        scope_release(child.parent);
        child.parent = null;
    }

    // Recycle child struct.
    child.refcount = 0;
    child.generation++;
    child.escape_generation++;
    if (!scope_freelist_recycle_enabled()) {
        mem::free(child);
        return;
    }
    scope_global_lock();
    if (g_scope_freelist_count < SCOPE_FREELIST_MAX) {
        g_scope_memory_telemetry_stats.scope_recycle_count++;
        child.pool_next = g_scope_freelist;
        g_scope_freelist = child;
        g_scope_freelist_count++;
        scope_global_unlock();
    } else {
        scope_global_unlock();
        mem::free(child);
    }
}

// =============================================================================
// Freelist cleanup (for shutdown)
// =============================================================================

/**
 * Free all ScopeRegion structs in the global freelist.
 * Call at program shutdown.
 */
fn void scope_freelist_cleanup() {
    scope_global_lock();
    ScopeRegion* s = g_scope_freelist;
    g_scope_freelist = null;
    g_scope_freelist_count = 0;
    scope_global_unlock();

    while (s != null) {
        ScopeRegion* next = s.pool_next;
        mem::free(s);
        s = next;
    }

    scope_global_lock();
    ScopeChunk* chunk = g_fiber_temp_chunk_pool;
    g_fiber_temp_chunk_pool = null;
    g_fiber_temp_chunk_pool_count = 0;
    scope_global_unlock();

    while (chunk != null) {
        ScopeChunk* next = chunk.next;
        mem::free(chunk);
        chunk = next;
    }
}
