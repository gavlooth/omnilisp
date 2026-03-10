module main;

// Global bootstrap pointer — set before first context switch into a stack context.
// Safe because context switches are cooperative (single-threaded).
tlocal StackCtxBootstrap* g_stack_ctx_bootstrap;
// ASAN fake-stack token for the main thread stack.
tlocal void* g_main_asan_fake_stack;

fn void* stack_ctx_bottom(StackCtx* c) @inline {
    return (void*)((usz)c.stack.base + GUARD_SIZE);
}

fn void asan_switch_out(StackCtx* from_ctx, StackCtx* to_ctx) @inline {
    void** save_slot;
    if (from_ctx != null) {
        save_slot = &from_ctx.asan_fake_stack;
    } else {
        save_slot = &g_main_asan_fake_stack;
    }

    void* to_bottom = null;
    usz to_size = 0;
    if (to_ctx != null) {
        to_bottom = stack_ctx_bottom(to_ctx);
        to_size = to_ctx.stack.usable_size;
    }
    stack_asan_start_switch(save_slot, to_bottom, to_size);
}

fn void asan_switch_in(StackCtx* current_ctx) @inline {
    void* token = current_ctx != null ? current_ctx.asan_fake_stack : g_main_asan_fake_stack;
    stack_asan_finish_switch(token);
}

/**
 * Trampoline that runs when a coroutine starts for the first time.
 *
 * This is a normal C3 function (not naked), so it can call arbitrary code.
 * It reads the bootstrap info from the thread-local, calls the entry function,
 * then marks the coroutine as completed and switches back to the parent.
 */
fn void stack_ctx_trampoline() {
    StackCtxBootstrap* boot = g_stack_ctx_bootstrap;
    StackCtxEntryFn entry = boot.entry;
    void* arg = boot.arg;

    // Set current context from bootstrap (only valid for first switch)
    g_current_stack_ctx = boot.self;
    asan_switch_in(g_current_stack_ctx);

    // Call the actual entry function
    entry(arg);

    // Entry returned — mark as completed and switch back to parent.
    // IMPORTANT: Re-read g_current_stack_ctx here (do NOT use the stale boot.self).
    // After stack_ctx_clone + stack_ctx_resume, g_current_stack_ctx points to the clone,
    // but local variables on the cloned stack still reference the original.
    StackCtx* self = g_current_stack_ctx;
    self.status = CTX_COMPLETED;
    g_current_stack_ctx = null;
    asan_switch_out(self, self.parent_stack_ctx);
    stack_context_switch(&self.ctx, self.parent_ctx);

    // Should never reach here
}
