module main;


fn void stack_pool_link_ctx(StackPool* pool, StackCtx* c) @inline {
    c.all_next = pool.all_list;
    pool.all_list = c;
    pool.all_count++;
}

fn void stack_pool_unlink_ctx(StackPool* pool, StackCtx* c) {
    StackCtx* prev = null;
    StackCtx* cur = pool.all_list;
    while (cur != null) {
        if (cur == c) {
            if (prev == null) {
                pool.all_list = cur.all_next;
            } else {
                prev.all_next = cur.all_next;
            }
            c.all_next = null;
            assert(pool.all_count > 0, "stack pool all_count underflow");
            pool.all_count--;
            return;
        }
        prev = cur;
        cur = cur.all_next;
    }
    unreachable("stack pool context unlink mismatch");
}

// =============================================================================
// SECTION 8: COROUTINE LIFECYCLE
// =============================================================================

/**
 * Create a coroutine. Pops from pool if available, else allocates fresh.
 */
fn StackCtx* stack_ctx_create(StackPool* pool) {
    stack_require_pool_owner(pool, "stack_ctx_create");
    if (pool.shutting_down) {
        return null;
    }
    StackCtx* c;

    if (pool.free_list != null) {
        // Recycle from pool
        c = pool.free_list;
        pool.free_list = c.pool_next;
        pool.pool_size--;
    } else {
        // Fresh allocation
        c = (StackCtx*)mem::malloc(StackCtx.sizeof);
        c.stack = stack_region_alloc(pool.stack_size);
        if (c.stack.base == null) {
            mem::free(c);
            return null;  // mmap failed
        }
        // Register guard page for stack overflow detection
        stack_guard_register(c.stack.base, GUARD_SIZE);
        stack_pool_link_ctx(pool, c);
    }

    // Initialize state
    StackContext empty_ctx;
    for (usz i = 0; i < StackContext.sizeof; i++) {
        ((char*)&empty_ctx)[i] = 0;
    }
    c.ctx = empty_ctx;
    c.status = CTX_READY;
    c.result = null;
    c.parent_ctx = null;
    c.parent_stack_ctx = null;
    c.asan_fake_stack = null;
    c.owner_pool = pool;
    c.in_pool_cache = false;
    c.pool_next = null;
    c.user_data = null;
    c.owner_thread_token = stack_current_thread_token();
    c.defer_heap = null;
    c.defer_count = 0;
    c.defer_capacity = STACK_CTX_DEFER_INLINE_CAP;
    c.lifecycle_heap = null;
    c.lifecycle_count = 0;
    c.lifecycle_capacity = STACK_CTX_LIFECYCLE_INLINE_CAP;
    c.id = pool.next_id;
    pool.next_id++;

    return c;
}

fn void stack_ctx_destroy(StackCtx* c, StackPool* pool) {
    stack_require_ctx_owner(c, "stack_ctx_destroy");
    stack_require_pool_owner(pool, "stack_ctx_destroy");
    if (c.owner_pool != pool) {
        unreachable("stack_ctx_destroy: pool ownership mismatch");
    }
    if (c.in_pool_cache) {
        // Idempotency guard: repeated destroy on a pooled context must not
        // re-enqueue the same node and corrupt the free list.
        return;
    }
    if (c == g_current_stack_ctx || c.status == CTX_RUNNING) {
        unreachable("stack_ctx_destroy: attempted to free running context");
    }
    if (pool.shutting_down) {
        // Reentrant destroy during pool shutdown: the shutdown walker is the sole
        // authority for unlinking and final free.
        c.status = CTX_DEAD;
        c.result = null;
        c.parent_ctx = null;
        c.parent_stack_ctx = null;
        c.asan_fake_stack = null;
        return;
    }
    stack_ctx_run_deferred_destroy(c);
    stack_ctx_run_lifecycle_destroy(c);
    stack_ctx_clear_defer_storage(c);
    stack_ctx_clear_lifecycle_storage(c);
    if (pool.pool_size < pool.max_pool) {
        // Return to pool — keep the stack allocation
        c.status = CTX_DEAD;
        c.result = null;
        c.parent_ctx = null;
        c.parent_stack_ctx = null;
        c.asan_fake_stack = null;
        c.user_data = null;
        c.in_pool_cache = true;
        c.pool_next = pool.free_list;
        pool.free_list = c;
        pool.pool_size++;
    } else {
        // Pool full — unregister guard page and free everything
        stack_pool_unlink_ctx(pool, c);
        c.in_pool_cache = false;
        stack_guard_unregister(c.stack.base);
        stack_region_free(&c.stack);
        mem::free(c);
    }
}
