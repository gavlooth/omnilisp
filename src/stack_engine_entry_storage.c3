module main;

fn StackCtxDeferEntry* stack_ctx_entry_at(StackCtxDeferEntry* inline_entries, StackCtxDeferEntry* heap_entries, usz idx) @inline {
    if (heap_entries != null) return &heap_entries[idx];
    return &inline_entries[idx];
}

fn bool stack_ctx_entry_reserve(StackCtxDeferEntry* inline_entries, StackCtxDeferEntry** heap_entries, usz count, usz* capacity, usz inline_cap, usz needed, bool track_heap_allocs) {
    if (needed <= inline_cap && *heap_entries == null) {
        *capacity = inline_cap;
        return true;
    }

    if (*heap_entries == null) {
        usz new_cap = inline_cap * 2;
        while (new_cap < needed) new_cap *= 2;

        StackCtxDeferEntry* buf = (StackCtxDeferEntry*)mem::malloc(StackCtxDeferEntry.sizeof * new_cap);
        if (buf == null) return false;
        if (track_heap_allocs) g_stack_defer_heap_alloc_count++;

        for (usz i = 0; i < count; i++) {
            buf[i] = inline_entries[i];
        }
        *heap_entries = buf;
        *capacity = new_cap;
        return true;
    }

    if (needed <= *capacity) return true;

    usz new_cap = *capacity * 2;
    while (new_cap < needed) new_cap *= 2;

    StackCtxDeferEntry* grown = (StackCtxDeferEntry*)mem::malloc(StackCtxDeferEntry.sizeof * new_cap);
    if (grown == null) return false;
    if (track_heap_allocs) g_stack_defer_heap_alloc_count++;

    for (usz i = 0; i < count; i++) {
        grown[i] = (*heap_entries)[i];
    }
    mem::free(*heap_entries);
    *heap_entries = grown;
    *capacity = new_cap;
    return true;
}

fn void stack_ctx_entry_run_destroy(StackCtxDeferEntry* inline_entries, StackCtxDeferEntry* heap_entries, usz* count_ptr, bool track_destroy_callbacks) {
    while (*count_ptr > 0) {
        (*count_ptr)--;
        StackCtxDeferEntry* e = stack_ctx_entry_at(inline_entries, heap_entries, *count_ptr);
        if (e.destroy_fn != null) {
            e.destroy_fn(e.arg);
            if (track_destroy_callbacks) g_stack_defer_destroy_callback_count++;
        }
        e.destroy_fn = null;
        e.clone_fn = null;
        e.arg = null;
    }
}

fn void stack_ctx_entry_clear(StackCtxDeferEntry** heap_entries, usz* count_ptr, usz* capacity_ptr, usz inline_cap) {
    if (*heap_entries != null) {
        mem::free(*heap_entries);
        *heap_entries = null;
    }
    *count_ptr = 0;
    *capacity_ptr = inline_cap;
}

fn bool stack_ctx_entry_clone_all(StackCtxDeferEntry* src_inline, StackCtxDeferEntry* src_heap, usz src_count, StackCtxDeferEntry* dst_inline, StackCtxDeferEntry** dst_heap, usz* dst_capacity, usz inline_cap, bool track_clone_metrics) {
    if (!stack_ctx_entry_reserve(dst_inline, dst_heap, 0, dst_capacity, inline_cap, src_count, false)) return false;

    for (usz i = 0; i < src_count; i++) {
        StackCtxDeferEntry src_e = *stack_ctx_entry_at(src_inline, src_heap, i);
        StackCtxDeferEntry* dst_e = stack_ctx_entry_at(dst_inline, *dst_heap, i);
        dst_e.destroy_fn = src_e.destroy_fn;
        dst_e.clone_fn = src_e.clone_fn;
        dst_e.arg = src_e.arg;
        if (track_clone_metrics) g_stack_defer_entries_cloned++;
        if (dst_e.clone_fn != null) {
            dst_e.clone_fn(dst_e.arg);
            if (track_clone_metrics) g_stack_defer_clone_callback_count++;
        }
    }
    return true;
}
