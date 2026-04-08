module main;

import std::io;

fn bool test_stack_ctx_thread_affinity_state() {
    StackPool pool;
    stack_pool_init(&pool);

    bool ok = pool.owner_thread_token == stack_current_thread_token();
    StackCtx* c = stack_ctx_create(&pool);
    ok = ok && c != null;
    if (c != null) {
        ok = ok && c.owner_thread_token == stack_current_thread_token();
        stack_ctx_destroy(c, &pool);
    }

    stack_pool_shutdown(&pool);
    return ok;
}

/**
 * Deliberately violate StackCtx thread-affinity ownership.
 *
 * This probe is for external harness usage (non-default path) to verify that
 * fail-fast ownership guards are active. It should terminate the process via
 * thread-affinity violation handling before returning.
 */
fn int run_stack_engine_affinity_violation_probe() {
    StackPool pool;
    stack_pool_init(&pool);

    StackCtx* c = stack_ctx_create(&pool);
    if (c == null) {
        stack_pool_shutdown(&pool);
        io::eprintfn("stack affinity probe: failed to create stack context");
        return 2;
    }

    // Corrupt owner token to force a deterministic owner mismatch.
    c.owner_thread_token = null;

    // Should fail-fast inside stack_ctx_destroy(...) ownership checks.
    stack_ctx_destroy(c, &pool);

    // If we got here, the guard path did not fire.
    stack_pool_shutdown(&pool);
    io::eprintfn("stack affinity probe: expected owner-thread violation did not trigger");
    return 3;
}
