module main;

// =============================================================================
// Scope reset (for loop reuse — Perceus reuse analysis)
// =============================================================================

fn ScopeChunk* scope_require_temp_baseline_chunk(ScopeRegion* scope) @inline {
    assert(scope.chunks != null, "scope_reset: missing baseline TEMP chunk");
    return scope.chunks;
}

/**
 * Reset a scope for reuse: run destructors, reset bump pointer to start of first chunk.
 * Only safe when RC == 1 (no external references — nothing escaped).
 *
 * Frees overflow chunks (keeps only the first), resets bump to start.
 * O(1) amortized per loop iteration when no escapes occur.
 */
fn void scope_reset(ScopeRegion* scope) {
    scope_guard_owner(scope, "scope_reset");
    assert(scope.refcount == 1, "scope_reset: refcount must be 1 (nothing escaped)");
    scope_assert_escape_tail_consistency(scope);
    scope_telemetry_note_scope_unused_at_reset(scope, true);
    // Run destructors first for both lanes
    scope_run_dtors(scope);
    scope_run_escape_dtors(scope);

    // Free overflow chunks (keep only the oldest = last in linked list)
    ScopeChunk* keep = null;
    ScopeChunk* c = scope_require_temp_baseline_chunk(scope);
    while (c != null) {
        ScopeChunk* next = c.next;
        if (next == null) {
            // This is the oldest (first allocated) chunk — keep it
            keep = c;
        } else {
            scope_temp_chunk_release(scope, c);
        }
        c = next;
    }

    // Reset to the kept chunk
    assert(keep != null, "scope_reset: keep chunk must exist");
    keep.next = null;
    scope.chunks = keep;
    scope.bump = scope_chunk_data(keep);
    scope.limit = scope.bump + keep.capacity;

    // Free all ESCAPE chunks on full reset.
    ScopeChunk* ec = scope.escape_chunks;
    while (ec != null) {
        ScopeChunk* next = ec.next;
        scope_telemetry_note_escape_chunk_release(ec);
        mem::free(ec);
        ec = next;
    }
    scope.escape_chunks = null;
    scope.escape_chunks_tail = null;
    scope.escape_bump = null;
    scope.escape_limit = null;

    scope.dtors = null;
    scope.escape_dtors = null;
    scope.escape_dtors_tail = null;
    scope.alloc_bytes = 0;
    scope.alloc_count = 0;
    scope_assert_escape_tail_consistency(scope);
}

/**
 * Reset only the TEMP lane for reuse while preserving ESCAPE lane allocations.
 * Safe only when RC == 1 (no external scope ownership changes during reset).
 */
fn void scope_reset_temp_lane(ScopeRegion* scope) {
    scope_guard_owner(scope, "scope_reset_temp_lane");
    assert(scope.refcount == 1, "scope_reset_temp_lane: refcount must be 1");
    scope_assert_escape_tail_consistency(scope);
    scope_telemetry_note_scope_unused_at_reset(scope, false);

    // TEMP dtors only; ESCAPE dtors remain registered.
    scope_run_dtors(scope);

    // Free overflow TEMP chunks, keep oldest TEMP chunk.
    ScopeChunk* keep = null;
    ScopeChunk* c = scope_require_temp_baseline_chunk(scope);
    while (c != null) {
        ScopeChunk* next = c.next;
        if (next == null) {
            keep = c;
        } else {
            scope_temp_chunk_release(scope, c);
        }
        c = next;
    }

    assert(keep != null, "scope_reset_temp_lane: keep chunk must exist");
    keep.next = null;
    scope.chunks = keep;
    scope.bump = scope_chunk_data(keep);
    scope.limit = scope.bump + keep.capacity;

    scope.dtors = null;
    scope.alloc_bytes = 0;
    scope.alloc_count = 0;
    scope_assert_escape_tail_consistency(scope);
}

// =============================================================================
// Scope membership check (for escape-scope optimization)
// =============================================================================

/**
 * Check whether a pointer was allocated within a given scope.
 * Walks the scope's chunk list checking address ranges.
 * Used by copy_to_parent to skip values not in the scope being released.
 */
fn bool is_in_scope(void* ptr, ScopeRegion* scope) {
    if (ptr == null || scope == null) return false;
    ScopeChunk* chunk = scope.chunks;
    while (chunk != null) {
        char* data = scope_chunk_data(chunk);
        if ((char*)ptr >= data && (char*)ptr < data + chunk.capacity) return true;
        chunk = chunk.next;
    }
    chunk = scope.escape_chunks;
    while (chunk != null) {
        char* data = scope_chunk_data(chunk);
        if ((char*)ptr >= data && (char*)ptr < data + chunk.capacity) return true;
        chunk = chunk.next;
    }
    return false;
}
