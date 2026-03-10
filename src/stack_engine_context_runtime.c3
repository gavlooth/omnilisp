module main;

/**
 * Switch into a stack context. Sets up bootstrap and parent, then context-switches.
 *
 * @param c          Stack context to run
 * @param parent_ctx Where to save the caller's context
 */
fn void stack_ctx_switch_to(StackCtx* c, StackContext* parent_ctx) {
    stack_require_ctx_owner(c, "stack_ctx_switch_to");
    c.parent_ctx = parent_ctx;

    // Set bootstrap for first switch (trampoline reads this)
    if (c.status == CTX_READY) {
        g_stack_ctx_bootstrap = &c.boot;
        c.status = CTX_RUNNING;
    }

    StackCtx* saved = g_current_stack_ctx;
    c.parent_stack_ctx = saved;
    g_current_stack_ctx = c;

    // D1: Save parent FPU state before switch
    fpu_save(&parent_ctx.mxcsr, &parent_ctx.x87cw);

    // D2: Protected switch with stack overflow recovery
    asan_switch_out(saved, c);
    int overflow = stack_guard_protected_switch(parent_ctx, &c.ctx);
    asan_switch_in(saved);

    // D1: Restore parent FPU state (also after overflow — siglongjmp doesn't restore FPU)
    fpu_restore(parent_ctx.mxcsr, parent_ctx.x87cw);

    if (overflow != 0) {
        c.status = CTX_DEAD;
    }

    g_current_stack_ctx = saved;
}

// Thread-local: currently executing coroutine (null if on main stack)
tlocal StackCtx* g_current_stack_ctx;

/**
 * Suspend the current coroutine and switch back to its parent.
 * This is the primitive behind yield/shift/signal.
 */
fn void stack_ctx_suspend() {
    StackCtx* c = g_current_stack_ctx;
    assert(c != null, "stack_ctx_suspend: not inside a coroutine");
    stack_require_ctx_owner(c, "stack_ctx_suspend");
    c.status = CTX_SUSPENDED;
    // D1: Save FPU state before switch to parent
    fpu_save(&c.ctx.mxcsr, &c.ctx.x87cw);
    asan_switch_out(c, c.parent_stack_ctx);
    stack_context_switch(&c.ctx, c.parent_ctx);
    asan_switch_in(c);
    // Execution resumes here when parent calls stack_ctx_resume
    // D1: Restore FPU state on resume
    fpu_restore(c.ctx.mxcsr, c.ctx.x87cw);
}

/**
 * Resume a suspended coroutine. Updates tracking and switches.
 */
fn void stack_ctx_resume(StackCtx* c, StackContext* parent_ctx) {
    stack_require_ctx_owner(c, "stack_ctx_resume");
    assert(c.status == CTX_SUSPENDED, "stack_ctx_resume: context not suspended");
    c.parent_ctx = parent_ctx;
    c.status = CTX_RUNNING;
    StackCtx* saved = g_current_stack_ctx;
    c.parent_stack_ctx = saved;
    g_current_stack_ctx = c;

    // D1: Save parent FPU state before switch
    fpu_save(&parent_ctx.mxcsr, &parent_ctx.x87cw);

    // D2: Protected switch with stack overflow recovery
    asan_switch_out(saved, c);
    int overflow = stack_guard_protected_switch(parent_ctx, &c.ctx);
    asan_switch_in(saved);

    // D1: Restore parent FPU state
    fpu_restore(parent_ctx.mxcsr, parent_ctx.x87cw);

    if (overflow != 0) {
        c.status = CTX_DEAD;
    }

    g_current_stack_ctx = saved;
}
