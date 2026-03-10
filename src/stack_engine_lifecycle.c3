module main;


// =============================================================================
// SECTION 8: COROUTINE LIFECYCLE
// =============================================================================

/**
 * Create a coroutine. Pops from pool if available, else allocates fresh.
 */
fn StackCtx* stack_ctx_create(StackPool* pool) {
    stack_require_pool_owner(pool, "stack_ctx_create");
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
        c.pool_next = pool.free_list;
        pool.free_list = c;
        pool.pool_size++;
    } else {
        // Pool full — unregister guard page and free everything
        stack_guard_unregister(c.stack.base);
        stack_region_free(&c.stack);
        mem::free(c);
    }
}
