module main;

import std::io;

// =============================================================================
// SECTION 7: STACK POOL (free-list of recycled stacks)
// =============================================================================

struct StackPool {
    StackCtx*  free_list;   // Singly-linked list of available coros
    usz    pool_size;   // Current count in free list
    usz    max_pool;    // Max cached before freeing
    uint   next_id;     // Monotonic ID counter
    usz    stack_size;  // Default stack size for this pool
    void*  owner_thread_token; // Thread-affinity token for this pool
}

const usz STACK_POOL_MAX = 64;

tlocal char g_stack_thread_marker;

fn void* stack_current_thread_token() @inline {
    return (void*)&g_stack_thread_marker;
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
    pool.pool_size = 0;
    pool.max_pool = STACK_POOL_MAX;
    pool.next_id = 1;
    pool.stack_size = stack_runtime_asan_enabled() ? ASAN_STACK_SIZE : DEFAULT_STACK_SIZE;
    stack_guard_init();
}

fn void stack_pool_shutdown(StackPool* pool) {
    stack_require_pool_owner(pool, "stack_pool_shutdown");
    // Free all cached coros and unregister their guard pages.
    StackCtx* c = pool.free_list;
    while (c != null) {
        StackCtx* next = c.pool_next;
        stack_require_ctx_owner(c, "stack_pool_shutdown_ctx");
        if (c.defer_heap != null) {
            mem::free(c.defer_heap);
            c.defer_heap = null;
        }
        if (c.lifecycle_heap != null) {
            mem::free(c.lifecycle_heap);
            c.lifecycle_heap = null;
        }
        stack_guard_unregister(c.stack.base);
        stack_region_free(&c.stack);
        mem::free(c);
        c = next;
    }
    pool.free_list = null;
    pool.pool_size = 0;
    stack_guard_shutdown();
}
