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

    // Save parent backend FP/control state before switch.
    stack_backend_save_fp_state(parent_ctx);

    // D2: Protected switch with stack overflow recovery
    asan_switch_out(saved, c);
    int overflow = stack_backend_protected_switch(parent_ctx, &c.ctx);
    asan_switch_in(saved);

    // Restore parent backend FP/control state (also after overflow).
    stack_backend_restore_fp_state(parent_ctx);

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
    // Save backend FP/control state before switching to parent.
    stack_backend_save_fp_state(&c.ctx);
    asan_switch_out(c, c.parent_stack_ctx);
    stack_backend_switch(&c.ctx, c.parent_ctx);

    // Execution resumes here when parent calls stack_ctx_resume.
    //
    // IMPORTANT: this frame can be resumed from a cloned continuation stack.
    // In that path, local `c` may still point at the source template context
    // copied in suspended frame slots. Rebind from TLS after switch return.
    StackCtx* resumed = g_current_stack_ctx;
    assert(resumed != null, "stack_ctx_suspend: resumed without current context");
    stack_require_ctx_owner(resumed, "stack_ctx_suspend:resumed");
    asan_switch_in(resumed);
    // Restore backend FP/control state on the actually resumed context.
    stack_backend_restore_fp_state(&resumed.ctx);
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

    // Save parent backend FP/control state before switch.
    stack_backend_save_fp_state(parent_ctx);

    // D2: Protected switch with stack overflow recovery
    asan_switch_out(saved, c);
    int overflow = stack_backend_protected_switch(parent_ctx, &c.ctx);
    asan_switch_in(saved);

    // Restore parent backend FP/control state.
    stack_backend_restore_fp_state(parent_ctx);

    if (overflow != 0) {
        c.status = CTX_DEAD;
    }

    g_current_stack_ctx = saved;
}
