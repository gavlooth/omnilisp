module main;

fn StackCtxDeferEntry* stack_ctx_lifecycle_entry_at(StackCtx* c, usz idx) @inline {
    if (c.lifecycle_heap != null) return &c.lifecycle_heap[idx];
    return &c.lifecycle_inline[idx];
}

fn bool stack_ctx_lifecycle_reserve(StackCtx* c, usz needed) {
    if (needed <= STACK_CTX_LIFECYCLE_INLINE_CAP && c.lifecycle_heap == null) {
        c.lifecycle_capacity = STACK_CTX_LIFECYCLE_INLINE_CAP;
        return true;
    }

    if (c.lifecycle_heap == null) {
        usz new_cap = STACK_CTX_LIFECYCLE_INLINE_CAP * 2;
        while (new_cap < needed) new_cap *= 2;
        StackCtxDeferEntry* buf = (StackCtxDeferEntry*)mem::malloc(StackCtxDeferEntry.sizeof * new_cap);
        if (buf == null) return false;
        for (usz i = 0; i < c.lifecycle_count; i++) {
            buf[i] = c.lifecycle_inline[i];
        }
        c.lifecycle_heap = buf;
        c.lifecycle_capacity = new_cap;
        return true;
    }

    if (needed <= c.lifecycle_capacity) return true;
    usz new_cap = c.lifecycle_capacity * 2;
    while (new_cap < needed) new_cap *= 2;
    StackCtxDeferEntry* grown = (StackCtxDeferEntry*)mem::malloc(StackCtxDeferEntry.sizeof * new_cap);
    if (grown == null) return false;
    for (usz i = 0; i < c.lifecycle_count; i++) {
        grown[i] = c.lifecycle_heap[i];
    }
    mem::free(c.lifecycle_heap);
    c.lifecycle_heap = grown;
    c.lifecycle_capacity = new_cap;
    return true;
}

/**
 * Attach a persistent lifecycle callback entry to a context.
 *
 * Unlike stack_ctx_defer(...), these entries are not affected by
 * stack_ctx_undefer(...). They are cloned with the context and run when
 * the context is destroyed.
 */
fn bool stack_ctx_lifecycle_attach(StackCtx* c, StackCtxDeferOps* ops, void* arg) {
    if (c == null || ops == null) return false;
    stack_require_ctx_owner(c, "stack_ctx_lifecycle_attach");
    if (ops.destroy_fn == null && ops.clone_fn == null) return false;
    usz next = c.lifecycle_count + 1;
    if (!stack_ctx_lifecycle_reserve(c, next)) return false;
    StackCtxDeferEntry* e = stack_ctx_lifecycle_entry_at(c, c.lifecycle_count);
    e.destroy_fn = ops.destroy_fn;
    e.clone_fn = ops.clone_fn;
    e.arg = arg;
    c.lifecycle_count = next;
    return true;
}

fn void* stack_ctx_find_lifecycle_arg(StackCtx* c, StackCtxDeferFn destroy_fn, StackCtxDeferFn clone_fn) {
    if (c == null) return null;
    stack_require_ctx_owner(c, "stack_ctx_find_lifecycle_arg");
    if (destroy_fn == null && clone_fn == null) return null;
    for (usz i = 0; i < c.lifecycle_count; i++) {
        StackCtxDeferEntry* e = stack_ctx_lifecycle_entry_at(c, i);
        if (e.destroy_fn == destroy_fn && e.clone_fn == clone_fn) {
            return e.arg;
        }
    }
    return null;
}

fn void stack_ctx_run_lifecycle_destroy(StackCtx* c) {
    stack_require_ctx_owner(c, "stack_ctx_run_lifecycle_destroy");
    while (c.lifecycle_count > 0) {
        c.lifecycle_count--;
        StackCtxDeferEntry* e = stack_ctx_lifecycle_entry_at(c, c.lifecycle_count);
        if (e.destroy_fn != null) {
            e.destroy_fn(e.arg);
        }
        e.destroy_fn = null;
        e.clone_fn = null;
        e.arg = null;
    }
}

fn void stack_ctx_clear_lifecycle_storage(StackCtx* c) {
    stack_require_ctx_owner(c, "stack_ctx_clear_lifecycle_storage");
    if (c.lifecycle_heap != null) {
        mem::free(c.lifecycle_heap);
        c.lifecycle_heap = null;
    }
    c.lifecycle_count = 0;
    c.lifecycle_capacity = STACK_CTX_LIFECYCLE_INLINE_CAP;
}

fn bool stack_ctx_clone_lifecycle_entries(StackCtx* source, StackCtx* clone) {
    stack_require_ctx_owner(source, "stack_ctx_clone_lifecycle_entries_source");
    stack_require_ctx_owner(clone, "stack_ctx_clone_lifecycle_entries_clone");
    if (!stack_ctx_lifecycle_reserve(clone, source.lifecycle_count)) return false;
    for (usz i = 0; i < source.lifecycle_count; i++) {
        StackCtxDeferEntry src_e = *stack_ctx_lifecycle_entry_at(source, i);
        StackCtxDeferEntry* dst_e = stack_ctx_lifecycle_entry_at(clone, i);
        dst_e.destroy_fn = src_e.destroy_fn;
        dst_e.clone_fn = src_e.clone_fn;
        dst_e.arg = src_e.arg;
        if (dst_e.clone_fn != null) {
            dst_e.clone_fn(dst_e.arg);
        }
    }
    clone.lifecycle_count = source.lifecycle_count;
    return true;
}
