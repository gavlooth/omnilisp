module main;

fn StackCtxDeferEntry* stack_ctx_lifecycle_entry_at(StackCtx* c, usz idx) @inline {
    return stack_ctx_entry_at(&c.lifecycle_inline[0], c.lifecycle_heap, idx);
}

fn bool stack_ctx_lifecycle_reserve(StackCtx* c, usz needed) {
    return stack_ctx_entry_reserve(&c.lifecycle_inline[0], &c.lifecycle_heap, c.lifecycle_count, &c.lifecycle_capacity, STACK_CTX_LIFECYCLE_INLINE_CAP, needed, false);
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
    stack_ctx_entry_run_destroy(&c.lifecycle_inline[0], c.lifecycle_heap, &c.lifecycle_count, false);
}

fn void stack_ctx_clear_lifecycle_storage(StackCtx* c) {
    stack_require_ctx_owner(c, "stack_ctx_clear_lifecycle_storage");
    stack_ctx_entry_clear(&c.lifecycle_heap, &c.lifecycle_count, &c.lifecycle_capacity, STACK_CTX_LIFECYCLE_INLINE_CAP);
}

fn bool stack_ctx_clone_lifecycle_entries(StackCtx* source, StackCtx* clone) {
    stack_require_ctx_owner(source, "stack_ctx_clone_lifecycle_entries_source");
    stack_require_ctx_owner(clone, "stack_ctx_clone_lifecycle_entries_clone");
    if (!stack_ctx_entry_clone_all(&source.lifecycle_inline[0], source.lifecycle_heap, source.lifecycle_count, &clone.lifecycle_inline[0], &clone.lifecycle_heap, &clone.lifecycle_capacity, STACK_CTX_LIFECYCLE_INLINE_CAP, false)) return false;
    clone.lifecycle_count = source.lifecycle_count;
    return true;
}
