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
    g_scope_memory_telemetry_stats.scope_destroy_count++;
    scope_guard_owner(scope, "scope_destroy");
    scope_assert_escape_tail_consistency(scope);
    scope_telemetry_note_scope_unused_at_destroy(scope);
    scope.refcount = uint.max;
    // 1. Run destructors (reverse order — LIFO) for both lanes
    scope_run_dtors(scope);
    scope_run_escape_dtors(scope);

    // Owned-root teardown can still have retained descendants after dtors have
    // released every root-owned wrapper. Reclaim those before parent memory goes
    // away; normal scope destruction should already have an empty child list.
    if (scope.first_child != null) {
        scope_destroy_owned_descendants(scope);
    }

    // 2. Release parent scope (may cascade)
    scope_detach_from_parent(scope);

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
        scope_telemetry_note_escape_chunk_release(c);
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
    scope.refcount = 0;
    scope.first_child = null;
    scope.next_sibling = null;
    scope.prev_sibling = null;
    if (!scope_freelist_recycle_enabled()) {
        mem::free(scope);
        return;
    }
    scope_global_lock();
    if (g_scope_freelist_count < SCOPE_FREELIST_MAX) {
        g_scope_memory_telemetry_stats.scope_recycle_count++;
        scope.pool_next = g_scope_freelist;
        g_scope_freelist = scope;
        g_scope_freelist_count++;
        scope_global_unlock();
    } else {
        scope_global_unlock();
        mem::free(scope);
    }
}

fn void scope_destroy_owned_descendants(ScopeRegion* scope) {
    if (scope == null) return;
    scope_guard_owner(scope, "scope_destroy_owned_descendants");
    uint saved_refcount = scope.refcount;
    scope.refcount = uint.max;
    while (scope.first_child != null) {
        ScopeRegion* child = scope.first_child;
        scope_destroy_owned_descendants(child);
        child.refcount = 0;
        scope_destroy(child);
    }
    scope.refcount = saved_refcount;
}

<*
 @require scope != null
*>
fn void scope_destroy_owned_root(ScopeRegion* scope) {
    scope_guard_owner(scope, "scope_destroy_owned_root");
    if (scope.refcount == uint.max) return;
    scope.refcount = 0;
    scope_destroy(scope);
}
