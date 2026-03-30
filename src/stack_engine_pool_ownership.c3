module main;

import std::io;

// =============================================================================
// SECTION 7: STACK POOL (free-list of recycled stacks)
// =============================================================================

struct StackPool {
    StackCtx*  free_list;   // Singly-linked list of available coros
    StackCtx*  all_list;    // Intrusive list of all contexts owned by this pool
    usz    pool_size;   // Current count in free list
    usz    all_count;   // Total contexts owned by this pool (free + active)
    usz    max_pool;    // Max cached before freeing
    uint   next_id;     // Monotonic ID counter
    usz    stack_size;  // Default stack size for this pool
    void*  owner_thread_token; // Thread-affinity token for this pool
    bool   shutting_down;
}

const usz STACK_POOL_MAX = 64;

tlocal char g_stack_thread_marker;

fn void* stack_current_thread_token() @inline {
    return (void*)&g_stack_thread_marker;
}

fn usz stack_pool_cache_limit() @inline {
    // Under ASAN prefer direct free over stack-context reuse. Suspended
    // continuation/handle paths can otherwise recycle sanitizer fake-stack
    // metadata across destroy/create boundaries and obscure the first bad
    // runtime state transition with reuse-only crashes.
    return stack_runtime_asan_enabled() ? 0 : STACK_POOL_MAX;
}

fn void stack_thread_violation(char[] op) @inline {
    io::eprintfn("stack-engine thread-affinity violation in %s", op);
    unreachable("stack-engine used from non-owner thread");
}

fn void stack_require_pool_owner(StackPool* pool, char[] op) @inline {
    if (pool == null) return;
    if (pool.owner_thread_token != stack_current_thread_token()) {
        stack_thread_violation(op);
    }
}

fn void stack_require_ctx_owner(StackCtx* c, char[] op) @inline {
    if (c == null) return;
    if (c.owner_thread_token != stack_current_thread_token()) {
        stack_thread_violation(op);
    }
}

fn void stack_pool_init(StackPool* pool) {
    pool.owner_thread_token = stack_current_thread_token();
    pool.free_list = null;
    pool.all_list = null;
    pool.pool_size = 0;
    pool.all_count = 0;
    pool.max_pool = stack_pool_cache_limit();
    pool.next_id = 1;
    pool.stack_size = stack_runtime_asan_enabled() ? ASAN_STACK_SIZE : DEFAULT_STACK_SIZE;
    pool.shutting_down = false;
    stack_guard_init();
}

fn void stack_pool_shutdown(StackPool* pool) {
    stack_require_pool_owner(pool, "stack_pool_shutdown");
    if (pool.shutting_down) return;
    pool.shutting_down = true;
    // Two-phase shutdown: run deferred/lifecycle callbacks for all contexts
    // before freeing any StackCtx allocations. Deferred callbacks may release
    // scopes that still reference other contexts in this pool.
    StackCtx* shutdown_list = pool.all_list;
    pool.all_list = null;
    pool.free_list = null;
    usz shutdown_count = 0;
    StackCtx* c = shutdown_list;
    while (c != null) {
        shutdown_count++;
        stack_require_ctx_owner(c, "stack_pool_shutdown_ctx");
        if (c == g_current_stack_ctx || c.status == CTX_RUNNING) {
            unreachable("stack_pool_shutdown: attempted to free running context");
        }
        c.in_pool_cache = false;
        stack_ctx_run_deferred_destroy(c);
        stack_ctx_run_lifecycle_destroy(c);
        stack_ctx_clear_defer_storage(c);
        stack_ctx_clear_lifecycle_storage(c);
        c = c.all_next;
    }
    c = shutdown_list;
    while (c != null) {
        StackCtx* next = c.all_next;
        c.all_next = null;
        stack_guard_unregister(c.stack.base);
        stack_region_free(&c.stack);
        mem::free(c);
        c = next;
    }
    assert(pool.all_count == shutdown_count, "stack pool all_count mismatch during shutdown");
    pool.free_list = null;
    pool.all_list = null;
    pool.pool_size = 0;
    pool.all_count = 0;
    pool.shutting_down = false;
    stack_guard_shutdown();
}
