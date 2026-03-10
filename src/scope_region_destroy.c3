module main;

fn void scope_run_dtor_chain(ScopeDtor* d) @inline {
    while (d != null) {
        d.func(d.ptr);
        d = d.next;
    }
}

<* @require scope != null *>
fn void scope_run_dtors(ScopeRegion* scope) {
    scope_run_dtor_chain(scope.dtors);
}

<* @require scope != null *>
fn void scope_run_escape_dtors(ScopeRegion* scope) {
    scope_run_dtor_chain(scope.escape_dtors);
}

<*
 @require scope != null
 @require scope.refcount == 0 : "scope_destroy: refcount not zero"
*>
fn void scope_destroy(ScopeRegion* scope) {
    scope_guard_owner(scope, "scope_destroy");
    scope_assert_escape_tail_consistency(scope);
    // 1. Run destructors (reverse order — LIFO) for both lanes
    scope_run_dtors(scope);
    scope_run_escape_dtors(scope);

    // 2. Release parent scope (may cascade)
    if (scope.parent != null) {
        scope_release(scope.parent);
        scope.parent = null;
    }

    // 3. Free all TEMP chunks
    ScopeChunk* c = scope.chunks;
    while (c != null) {
        ScopeChunk* next = c.next;
        scope_temp_chunk_release(scope, c);
        c = next;
    }

    // 4. Free all ESCAPE chunks
    c = scope.escape_chunks;
    while (c != null) {
        ScopeChunk* next = c.next;
        mem::free(c);
        c = next;
    }

    scope.chunks = null;
    scope.bump = null;
    scope.limit = null;
    scope.escape_chunks = null;
    scope.escape_chunks_tail = null;
    scope.escape_bump = null;
    scope.escape_limit = null;
    scope.dtors = null;
    scope.escape_dtors = null;
    scope.escape_dtors_tail = null;
    scope.fiber_temp_eligible = false;
    scope_assert_escape_tail_consistency(scope);

    // 5. Recycle ScopeRegion struct (increment generation, add to freelist)
    scope.generation++;
    scope.escape_generation++;
    if (!scope_freelist_recycle_enabled()) {
        mem::free(scope);
        return;
    }
    scope_global_lock();
    if (g_scope_freelist_count < SCOPE_FREELIST_MAX) {
        scope.pool_next = g_scope_freelist;
        g_scope_freelist = scope;
        g_scope_freelist_count++;
        scope_global_unlock();
    } else {
        scope_global_unlock();
        mem::free(scope);
    }
}
