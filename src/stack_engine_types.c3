/**
 * =============================================================================
 * STACK ENGINE TYPES — Stack context declarations and callback metadata
 * =============================================================================
 */
module main;

// =============================================================================
// SECTION 4: COROUTINE STATUS
// =============================================================================

enum StackCtxStatus : char {
    CTX_READY,       // Created, not yet started
    CTX_RUNNING,     // Currently executing
    CTX_SUSPENDED,   // Yielded/shifted, can be resumed
    CTX_COMPLETED,   // Returned normally
    CTX_DEAD         // Error or invalidated
}

/**
 * Entry function signature for coroutines.
 */
alias StackCtxEntryFn = fn void(void*);

/**
 * Generic cleanup callbacks for suspended/destroyed stack contexts.
 *
 * The stack engine treats these as opaque callbacks. Runtime layers provide
 * the semantic ownership actions (for example retain/release behaviors).
 */
alias StackCtxDeferFn = fn void(void*);

struct StackCtxDeferOps {
    StackCtxDeferFn destroy_fn; // Called on stack_ctx_destroy for active entries
    StackCtxDeferFn clone_fn;   // Called on stack_ctx_clone when entry is duplicated
}

struct StackCtxDeferEntry {
    StackCtxDeferFn destroy_fn;
    StackCtxDeferFn clone_fn;
    void* arg;
}

const usz STACK_CTX_DEFER_INLINE_CAP = 8;
const usz STACK_CTX_LIFECYCLE_INLINE_CAP = 4;

/**
 * Bootstrap state — tells the trampoline what function to call.
 */
struct StackCtxBootstrap {
    StackCtxEntryFn entry;
    void* arg;
    StackCtx* self;
}

// =============================================================================
// SECTION 6: COROUTINE HANDLE
// =============================================================================

/**
 * StackCtx — An execution context with its own stack.
 *
 * This is the runtime unit that backs delimited continuations, effect handlers,
 * and user-facing coroutines. The name "StackCtx" is internal — users see
 * "continuation" or "coroutine" depending on the abstraction layer.
 */
struct StackCtx {
    StackContext ctx;         // Saved register state
    StackRegion stack;        // Owned stack memory
    StackCtxStatus status;
    void* result;             // Return value when COMPLETED
    StackContext* parent_ctx; // Where to switch back to on yield/complete
    StackCtx* parent_stack_ctx; // Parent stack context (null = main stack)
    void* asan_fake_stack;    // ASAN fiber-switch token for this stack
    uint id;                  // Unique identifier
    StackPool* owner_pool;    // Owning pool authority for teardown
    bool in_pool_cache;       // True when currently parked on StackPool.free_list
    StackCtx* pool_next;      // Free-list link for StackPool
    StackCtx* all_next;       // Intrusive owner-list link for StackPool
    StackCtxBootstrap boot;   // Bootstrap info for first switch
    void* user_data;          // Application-level data (e.g. CoroutineThunkState*)
    void* owner_thread_token; // Thread-affinity token for this context
    StackCtxDeferEntry[STACK_CTX_DEFER_INLINE_CAP] defer_inline; // Inline fast-path storage
    StackCtxDeferEntry* defer_heap; // Overflow storage when inline slots are exceeded
    usz defer_count;          // Number of active defer entries
    usz defer_capacity;       // Capacity of defer_heap (or inline cap when heap is null)
    StackCtxDeferEntry[STACK_CTX_LIFECYCLE_INLINE_CAP] lifecycle_inline; // Persistent lifecycle callbacks
    StackCtxDeferEntry* lifecycle_heap; // Overflow storage for lifecycle callbacks
    usz lifecycle_count;      // Number of active lifecycle entries
    usz lifecycle_capacity;   // Capacity of lifecycle_heap (or inline cap when null)
}
