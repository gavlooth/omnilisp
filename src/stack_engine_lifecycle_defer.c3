module main;

fn StackCtxDeferEntry* stack_ctx_defer_entry_at(StackCtx* c, usz idx) @inline {
    if (c.defer_heap != null) return &c.defer_heap[idx];
    return &c.defer_inline[idx];
}

fn bool stack_ctx_defer_reserve(StackCtx* c, usz needed) {
    if (needed <= STACK_CTX_DEFER_INLINE_CAP && c.defer_heap == null) {
        c.defer_capacity = STACK_CTX_DEFER_INLINE_CAP;
        return true;
    }

    if (c.defer_heap == null) {
        usz new_cap = STACK_CTX_DEFER_INLINE_CAP * 2;
        while (new_cap < needed) new_cap *= 2;

        StackCtxDeferEntry* buf = (StackCtxDeferEntry*)mem::malloc(StackCtxDeferEntry.sizeof * new_cap);
        if (buf == null) return false;
        g_stack_defer_heap_alloc_count++;
        for (usz i = 0; i < c.defer_count; i++) {
            buf[i] = c.defer_inline[i];
        }
        c.defer_heap = buf;
        c.defer_capacity = new_cap;
        return true;
    }

    if (needed <= c.defer_capacity) return true;

    usz new_cap = c.defer_capacity * 2;
    while (new_cap < needed) new_cap *= 2;
    StackCtxDeferEntry* grown = (StackCtxDeferEntry*)mem::malloc(StackCtxDeferEntry.sizeof * new_cap);
    if (grown == null) return false;
    g_stack_defer_heap_alloc_count++;
    for (usz i = 0; i < c.defer_count; i++) {
        grown[i] = c.defer_heap[i];
    }
    mem::free(c.defer_heap);
    c.defer_heap = grown;
    c.defer_capacity = new_cap;
    return true;
}

/**
 * Register a cleanup callback for the active stack context.
 *
 * Entries are executed in LIFO order during stack_ctx_destroy().
 */
fn bool stack_ctx_defer(StackCtx* c, StackCtxDeferOps* ops, void* arg) {
    if (c == null || ops == null) return false;
    stack_require_ctx_owner(c, "stack_ctx_defer");
    if (ops.destroy_fn == null && ops.clone_fn == null) return false;

    usz next = c.defer_count + 1;
    if (!stack_ctx_defer_reserve(c, next)) return false;

    StackCtxDeferEntry* e = stack_ctx_defer_entry_at(c, c.defer_count);
    e.destroy_fn = ops.destroy_fn;
    e.clone_fn = ops.clone_fn;
    e.arg = arg;
    c.defer_count = next;
    g_stack_defer_push_count++;
    if (c.defer_count > g_stack_defer_peak_depth) {
        g_stack_defer_peak_depth = c.defer_count;
    }
    return true;
}

/**
 * Pop one defer entry without running its destroy callback.
 *
 * Returns false when no defer entries are active.
 */
fn bool stack_ctx_undefer(StackCtx* c) {
    if (c == null || c.defer_count == 0) return false;
    stack_require_ctx_owner(c, "stack_ctx_undefer");
    c.defer_count--;
    StackCtxDeferEntry* e = stack_ctx_defer_entry_at(c, c.defer_count);
    e.destroy_fn = null;
    e.clone_fn = null;
    e.arg = null;
    g_stack_defer_undefer_count++;
    return true;
}

/**
 * Update the argument payload for an existing defer entry.
 *
 * This is used by runtime layers when ownership of the protected resource
 * moves (for example TCO scope recycling replacing the active scope).
 */
fn bool stack_ctx_defer_update_arg(StackCtx* c, usz idx, void* arg) {
    if (c == null) return false;
    stack_require_ctx_owner(c, "stack_ctx_defer_update_arg");
    if (idx >= c.defer_count) return false;
    StackCtxDeferEntry* e = stack_ctx_defer_entry_at(c, idx);
    e.arg = arg;
    g_stack_defer_update_arg_count++;
    return true;
}

fn void stack_ctx_run_deferred_destroy(StackCtx* c) {
    stack_require_ctx_owner(c, "stack_ctx_run_deferred_destroy");
    while (c.defer_count > 0) {
        c.defer_count--;
        StackCtxDeferEntry* e = stack_ctx_defer_entry_at(c, c.defer_count);
        if (e.destroy_fn != null) {
            e.destroy_fn(e.arg);
            g_stack_defer_destroy_callback_count++;
        }
        e.destroy_fn = null;
        e.clone_fn = null;
        e.arg = null;
    }
}

fn void stack_ctx_clear_defer_storage(StackCtx* c) {
    stack_require_ctx_owner(c, "stack_ctx_clear_defer_storage");
    if (c.defer_heap != null) {
        mem::free(c.defer_heap);
        c.defer_heap = null;
    }
    c.defer_count = 0;
    c.defer_capacity = STACK_CTX_DEFER_INLINE_CAP;
}
