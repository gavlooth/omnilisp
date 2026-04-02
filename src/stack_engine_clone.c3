module main;

/**
 * Clone a suspended coroutine for multi-shot continuation invocation.
 *
 * Creates a new coroutine with an identical copy of the source's stack
 * contents and register state. Adjusts all frame pointers (RBP chain)
 * in the cloned stack to point into the new stack.
 *
 * @param source  Suspended coroutine to clone
 * @param pool    Stack pool for allocation
 * @return New coroutine (caller owns), or null on failure
 */
fn StackCtx* stack_ctx_clone(StackCtx* source, StackPool* pool) {
    stack_require_ctx_owner(source, "stack_ctx_clone_source");
    stack_require_pool_owner(pool, "stack_ctx_clone_pool");
    if (source.status != CTX_SUSPENDED) return null;
    if (source.stack.base == null || source.stack.stack_top == null || source.stack.usable_size == 0) return null;

    usz src_lo = (usz)source.stack.base + GUARD_SIZE;
    usz src_hi = (usz)source.stack.stack_top;
    usz src_rsp = (usz)source.ctx.rsp;
    if (src_rsp < src_lo || src_rsp > src_hi) return null;
    if ((usz)source.ctx.rbp != 0 && ((usz)source.ctx.rbp < src_lo || (usz)source.ctx.rbp > src_hi)) return null;

    StackCtx* clone = stack_ctx_create(pool);
    if (clone == null) return null;

    // Copy only the active stack segment [RSP, stack_top). This preserves live frames
    // and avoids touching unrelated bytes below RSP.
    usz active_bytes = src_hi - src_rsp;
    if (active_bytes > clone.stack.usable_size) {
        stack_ctx_destroy(clone, pool);
        return null;
    }

    usz dst_hi = (usz)clone.stack.stack_top;
    usz dst_rsp = dst_hi - active_bytes;
    stack_raw_copy((void*)dst_rsp, (void*)src_rsp, active_bytes);

    // Copy register context and relocate stack pointers into the cloned stack.
    clone.ctx = source.ctx;
    clone.status = CTX_SUSPENDED;
    clone.result = source.result;
    clone.parent_ctx = null;
    clone.parent_stack_ctx = null;
    clone.asan_fake_stack = null;
    clone.defer_count = 0;

    long delta = (long)(dst_rsp - src_rsp);
    clone.ctx.rsp = (ulong)((long)source.ctx.rsp + delta);
    clone.ctx.rbp = (ulong)((long)source.ctx.rbp + delta);

    // Walk and fixup the RBP chain on the cloned stack.
    usz lo = (usz)clone.stack.base + GUARD_SIZE;
    usz hi = (usz)clone.stack.stack_top;

    ulong* bp = (ulong*)(ulong)clone.ctx.rbp;
    for (int safety = 0; safety < 10000; safety++) {
        usz bp_addr = (usz)bp;
        if (bp_addr < lo || bp_addr >= hi || bp_addr == 0) break;

        ulong old_val = *bp;
        if (old_val == 0) break;  // End of chain

        if (old_val >= src_lo && old_val < src_hi) {
            *bp = (ulong)((long)old_val + delta);
        } else {
            break;
        }

        bp = (ulong*)*bp;
    }

    // Duplicate defer metadata and apply semantic clone hooks.
    if (source.defer_count > 0) {
        if (!stack_ctx_entry_clone_all(&source.defer_inline[0], source.defer_heap, source.defer_count, &clone.defer_inline[0], &clone.defer_heap, &clone.defer_capacity, STACK_CTX_DEFER_INLINE_CAP, true)) {
            stack_ctx_destroy(clone, pool);
            return null;
        }
        clone.defer_count = source.defer_count;
    }

    if (!stack_ctx_clone_lifecycle_entries(source, clone)) {
        stack_ctx_destroy(clone, pool);
        return null;
    }

    return clone;
}
