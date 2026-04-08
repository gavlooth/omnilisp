module main;

fn StackCtxDeferEntry* stack_ctx_defer_entry_at(StackCtx* c, usz idx) @inline {
    return stack_ctx_entry_at(&c.defer_inline[0], c.defer_heap, idx);
}

fn bool stack_ctx_defer_reserve(StackCtx* c, usz needed) {
    return stack_ctx_entry_reserve(&c.defer_inline[0], &c.defer_heap, c.defer_count, &c.defer_capacity, STACK_CTX_DEFER_INLINE_CAP, needed, true);
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
    stack_ctx_entry_run_destroy(&c.defer_inline[0], c.defer_heap, &c.defer_count, true);
}

fn void stack_ctx_clear_defer_storage(StackCtx* c) {
    stack_require_ctx_owner(c, "stack_ctx_clear_defer_storage");
    stack_ctx_entry_clear(&c.defer_heap, &c.defer_count, &c.defer_capacity, STACK_CTX_DEFER_INLINE_CAP);
}
