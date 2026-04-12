module main;

// =============================================================================
// Chunk allocation
// =============================================================================

bool g_scope_force_chunk_alloc_oom = false;

/**
 * Allocate a new chunk with at least `min_bytes` of usable data space.
 * The chunk struct header is placed at the start; data follows inline.
 */
fn ScopeChunk* scope_chunk_alloc_raw(usz min_bytes) {
    if (g_scope_force_chunk_alloc_oom ||
        scope_c_getenv("OMNI_SCOPE_FORCE_CHUNK_ALLOC_OOM") != null) {
        return null;
    }
    // Double from initial, capped at max
    usz capacity = SCOPE_CHUNK_INITIAL;
    while (capacity < min_bytes) {
        if (capacity > usz.max / 2) {
            capacity = min_bytes;
            break;
        }
        capacity *= 2;
        if (capacity > SCOPE_CHUNK_MAX) {
            capacity = min_bytes;  // Oversized allocation — exact fit
            break;
        }
    }
    if (capacity > usz.max - ScopeChunk.sizeof) return null;
    usz total = ScopeChunk.sizeof + capacity;
    ScopeChunk* chunk = (ScopeChunk*)mem::malloc(total);
    if (chunk == null) return null;
    chunk.next = null;
    chunk.capacity = capacity;
    return chunk;
}

fn ScopeChunk* scope_chunk_alloc_temp(usz min_bytes) {
    ScopeChunk* pooled = fiber_temp_chunk_try_take(min_bytes);
    if (pooled != null) return pooled;
    return scope_chunk_alloc_raw(min_bytes);
}

/**
 * Get the data start pointer for a chunk (immediately after the header).
 */
fn char* scope_chunk_data(ScopeChunk* chunk) @inline {
    return (char*)chunk + ScopeChunk.sizeof;
}

// =============================================================================
// Destructor registration
// =============================================================================

/**
 * Register a destructor to run when this scope is destroyed.
 * The ScopeDtor struct itself is bump-allocated in the scope's chunks.
 */
fn void scope_register_dtor(ScopeRegion* scope, void* ptr, ScopeDestructorFn func) {
    scope_guard_owner(scope, "scope_register_dtor");
    ScopeDtor* d = (ScopeDtor*)scope.alloc(ScopeDtor.sizeof);
    assert(d != null, "scope_register_dtor: destructor alloc failed");
    d.ptr = ptr;
    d.func = func;
    d.next = scope.dtors;
    scope.dtors = d;
}

/**
 * Register a destructor to run when this scope's ESCAPE lane is destroyed.
 * The ScopeDtor struct itself is ESCAPE-lane allocated.
 */
fn void scope_register_dtor_escape(ScopeRegion* scope, void* ptr, ScopeDestructorFn func) {
    scope_guard_owner(scope, "scope_register_dtor_escape");
    scope_assert_escape_tail_consistency(scope);
    ScopeDtor* d = (ScopeDtor*)scope.alloc_escape(ScopeDtor.sizeof);
    assert(d != null, "scope_register_dtor_escape: destructor alloc failed");
    d.ptr = ptr;
    d.func = func;
    ScopeDtor* previous_head = scope.escape_dtors;
    d.next = previous_head;
    scope.escape_dtors = d;
    if (previous_head == null) {
        scope.escape_dtors_tail = d;
    }
    scope_assert_escape_tail_consistency(scope);
}

fn void scope_noop_dtor(void* ptr) @inline {
    (void)ptr;
}

/**
 * Tombstone the newest matching TEMP-lane destructor without mutating dtor
 * list topology. This is used by rollback paths that must manually unwind an
 * already-materialized object before normal scope teardown.
 */
fn bool scope_cancel_dtor(ScopeRegion* scope, void* ptr, ScopeDestructorFn func) {
    if (scope == null || ptr == null || func == null) return false;
    scope_guard_owner(scope, "scope_cancel_dtor");
    ScopeDtor* d = scope.dtors;
    while (d != null) {
        if (d.ptr == ptr && d.func == func) {
            d.ptr = null;
            d.func = &scope_noop_dtor;
            return true;
        }
        d = d.next;
    }
    return false;
}

fn bool scope_cancel_dtor_escape(ScopeRegion* scope, void* ptr, ScopeDestructorFn func) {
    if (scope == null || ptr == null || func == null) return false;
    scope_guard_owner(scope, "scope_cancel_dtor_escape");
    scope_assert_escape_tail_consistency(scope);
    ScopeDtor* d = scope.escape_dtors;
    while (d != null) {
        if (d.ptr == ptr && d.func == func) {
            d.ptr = null;
            d.func = &scope_noop_dtor;
            return true;
        }
        d = d.next;
    }
    return false;
}

/**
 * Check if a pointer points to memory allocated within this scope's chunks.
 * O(N) where N is the number of chunks in this scope (usually 1 or 2).
 */
fn bool scope_contains(ScopeRegion* scope, void* ptr) {
    if (scope == null || ptr == null) return false;
    usz p = (usz)ptr;
    ScopeChunk* chunk = scope.chunks;
    while (chunk != null) {
        usz start = (usz)chunk + ScopeChunk.sizeof;
        usz end = start + chunk.capacity;
        if (p >= start && p < end) return true;
        chunk = chunk.next;
    }
    chunk = scope.escape_chunks;
    while (chunk != null) {
        usz start = (usz)chunk + ScopeChunk.sizeof;
        usz end = start + chunk.capacity;
        if (p >= start && p < end) return true;
        chunk = chunk.next;
    }
    return false;
}
