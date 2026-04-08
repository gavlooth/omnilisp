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

    // Global freelist and generation stamps are shared across threads.
    if (scope_freelist_recycle_enabled()) {
        scope_global_lock();
        if (g_scope_freelist != null) {
            scope = g_scope_freelist;
            g_scope_freelist = scope.pool_next;
            g_scope_freelist_count--;
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
        scope.chunks = null;
        scope.escape_chunks = null;
        scope.escape_chunks_tail = null;
        scope.escape_dtors_tail = null;
    } else {
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
        chunk.next = null;
        scope.chunks = chunk;
    }

    // Initialize scope
    scope.refcount = 1;
    scope.parent = parent;
    scope.bump = scope_chunk_data(chunk);
    scope.limit = scope.bump + chunk.capacity;
    scope.escape_bump = null;
    scope.escape_limit = null;
    scope.escape_chunks_tail = null;
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
    }

    scope_assert_escape_tail_consistency(scope);

    return scope;
}

/**
 * Increment the reference count of a scope.
 */
fn void scope_retain(ScopeRegion* scope) @inline {
    if (scope == null) return;
    scope_guard_owner(scope, "scope_retain");
    scope_invariant_refcount_live(scope);
    scope.refcount++;
}

/**
 * Decrement the reference count. Destroys the scope when RC hits 0.
 */
fn void scope_release(ScopeRegion* scope) @inline {
    if (scope == null) return;
    scope_guard_owner(scope, "scope_release");
    scope_invariant_refcount_live(scope);
    if (--scope.refcount == 0) {
        scope_destroy(scope);
    }
}
