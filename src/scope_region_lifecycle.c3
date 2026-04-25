module main;

<*
 * Create a new ScopeRegion, optionally as a child of `parent`.
 * If parent is non-null, it is retained (RC incremented).
 * Returns a scope with refcount=1 (the caller owns it), or null on OOM.
*>
fn ScopeRegion* scope_create(ScopeRegion* parent = null) {
    ScopeRegion* scope = null;
    uint generation = 0;
    uint escape_generation = 0;
    bool fiber_temp_eligible = scope_fiber_temp_context_eligible() && parent != null;
    bool reused_from_freelist = false;

    // Global freelist and generation stamps are shared across threads.
    if (scope_freelist_recycle_enabled()) {
        scope_global_lock();
        if (g_scope_freelist != null) {
            scope = g_scope_freelist;
            g_scope_freelist = scope.pool_next;
            g_scope_freelist_count--;
            reused_from_freelist = true;
        }
        generation = ++g_scope_generation_counter;
        escape_generation = ++g_scope_generation_counter;
        scope_global_unlock();
    } else {
        scope_global_lock();
        generation = ++g_scope_generation_counter;
        escape_generation = ++g_scope_generation_counter;
        scope_global_unlock();
    }

    if (scope == null) {
        scope = (ScopeRegion*)mem::malloc(ScopeRegion.sizeof);
        if (scope == null) return null;
        scope_telemetry_note_scope_fresh_alloc();
        scope.chunks = null;
        scope.escape_chunks = null;
        scope.escape_chunks_tail = null;
        scope.escape_dtors_tail = null;
    } else {
        if (reused_from_freelist) scope_telemetry_note_scope_recycle_hit();
        assert(scope.escape_chunks == null, "scope_create: recycled scope escape_chunks must be cleared");
        assert(scope.escape_chunks_tail == null, "scope_create: recycled scope escape_chunks_tail must be cleared");
        assert(scope.escape_dtors == null, "scope_create: recycled scope escape_dtors must be cleared");
        assert(scope.escape_dtors_tail == null, "scope_create: recycled scope escape_dtors_tail must be cleared");
    }
    scope.generation = generation;
    scope.escape_generation = escape_generation;

    // Reuse existing chunk if present, else allocate
    ScopeChunk* chunk = scope.chunks;
    if (chunk == null) {
        chunk = fiber_temp_eligible
            ? scope_chunk_alloc_temp(SCOPE_CHUNK_INITIAL)
            : scope_chunk_alloc_raw(SCOPE_CHUNK_INITIAL);
        if (chunk == null) {
            mem::free(scope);
            return null;
        }
        scope_telemetry_note_temp_chunk_alloc(chunk);
        chunk.next = null;
        scope.chunks = chunk;
    }

    // Initialize scope
    scope.refcount = 1;
    scope.parent = parent;
    scope.first_child = null;
    scope.next_sibling = null;
    scope.prev_sibling = null;
    scope.bump = scope_chunk_data(chunk);
    scope.limit = scope.bump + chunk.capacity;
    scope.escape_bump = null;
    scope.escape_limit = null;
    scope.escape_chunks_tail = null;
    scope.escape_alloc_source = SCOPE_ESCAPE_ALLOC_UNKNOWN;
    scope_telemetry_reset_temp_sequence_state(scope);
    scope_telemetry_reset_escape_sequence_state(scope);
    scope.dtors = null;
    scope.escape_dtors = null;
    scope.escape_dtors_tail = null;
    scope.alloc_bytes = 0;
    scope.alloc_count = 0;
    scope.pool_next = null;
    scope.owner_thread_token = scope_current_thread_token();
    scope.fiber_temp_eligible = fiber_temp_eligible;

    // Retain parent
    if (parent != null) {
        scope_retain(parent);
        scope.next_sibling = parent.first_child;
        if (parent.first_child != null) {
            parent.first_child.prev_sibling = scope;
        }
        parent.first_child = scope;
    }

    scope_assert_escape_tail_consistency(scope);

    g_scope_memory_telemetry_stats.scope_create_count++;
    return scope;
}

fn void scope_detach_from_parent(ScopeRegion* scope, bool release_parent = true) {
    if (scope == null || scope.parent == null) return;
    ScopeRegion* parent = scope.parent;
    scope_guard_owner(parent, "scope_detach_from_parent(parent)");
    if (scope.prev_sibling != null) {
        scope.prev_sibling.next_sibling = scope.next_sibling;
    } else if (parent.first_child == scope) {
        parent.first_child = scope.next_sibling;
    }
    if (scope.next_sibling != null) {
        scope.next_sibling.prev_sibling = scope.prev_sibling;
    }
    scope.parent = null;
    scope.next_sibling = null;
    scope.prev_sibling = null;
    if (release_parent) {
        scope_release(parent);
    }
}

/**
 * Increment the reference count of a scope.
 */
fn void scope_retain(ScopeRegion* scope) @inline {
    if (scope == null) return;
    scope_guard_owner(scope, "scope_retain");
    scope_invariant_refcount_live(scope);
    assert(scope.refcount < uint.max, "scope_retain: refcount overflow");
    scope.refcount++;
}

/**
 * Decrement the reference count. Destroys the scope when RC hits 0.
 */
fn void scope_release(ScopeRegion* scope) @inline {
    if (scope == null) return;
    g_scope_memory_telemetry_stats.scope_release_count++;
    scope_guard_owner(scope, "scope_release");
    if (scope.refcount == uint.max) {
        // Parent teardown is already running dtors. Child-scope parent releases
        // during that pass must not recurse into the half-destroyed parent.
        return;
    }
    scope_invariant_refcount_live(scope);
    if (--scope.refcount == 0) {
        scope_destroy(scope);
    }
}
