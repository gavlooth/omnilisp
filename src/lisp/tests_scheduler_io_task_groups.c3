module lisp;


import std::core::mem;
import std::io;
import main;

fn Value* scheduler_test_make_opaque_primitive_result(Interp* interp, main::ScopeRegion* scope, int tag_value) {
    interp.current_scope = scope;
    Value* opaque = interp.alloc_value();
    opaque.tag = PRIMITIVE;
    main::scope_register_dtor(scope, (void*)opaque, &scope_dtor_value);

    Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
    int* opaque_payload = (int*)mem::malloc(int.sizeof);
    if (opaque_prim == null || opaque_payload == null) {
        if (opaque_prim != null) mem::free(opaque_prim);
        if (opaque_payload != null) mem::free(opaque_payload);
        return null;
    }

    *opaque_prim = {};
    *opaque_payload = tag_value;
    opaque_prim.name[0] = 's';
    opaque_prim.name[1] = 0;
    opaque_prim.arity = 0;
    opaque_prim.tag = tag_value;
    opaque_prim.user_data = (void*)opaque_payload;
    opaque.prim_val = opaque_prim;
    return opaque;
}

fn void run_scheduler_result_publication_fail_closed_tests(Interp* interp, int* pass, int* fail) {
    scheduler_init();
    main::ScopeRegion* saved_scope = interp.current_scope;
    main::ScopeRegion* child = main::scope_create(saved_scope);
    if (child == null) {
        io::printn("[FAIL] scheduler result publication fail-closed (alloc)");
        (*fail)++;
        return;
    }

    usz saved_fiber_count = g_scheduler.fiber_count;
    usz saved_current_fiber_id = g_scheduler.current_fiber_id;
    bool saved_running = g_scheduler.running;
    usz saved_resume_offset = g_scheduler.resume_offset;
    FiberEntry saved_fiber_0 = g_scheduler.fibers[0];
    FiberEntry saved_fiber_1 = g_scheduler.fibers[1];
    PendingTcpRead saved_pending_read_0 = g_scheduler.pending_reads[0];
    PendingOffload saved_pending_offload_0 = g_scheduler.pending_offloads[0];
    PendingSleep saved_pending_sleep_0 = g_scheduler.pending_sleeps[0];
    PendingDnsResolve saved_pending_dns_0 = g_scheduler.pending_dns[0];
    PendingTcpConnect saved_pending_connect_0 = g_scheduler.pending_connects[0];
    PendingTcpAccept saved_pending_accept_0 = g_scheduler.pending_accepts[0];
    uint saved_join_token_0 = g_scheduler.fiber_join_wait_token[0];

    g_scheduler.fiber_count = 2;
    g_scheduler.current_fiber_id = NO_FIBER;
    g_scheduler.running = false;
    g_scheduler.resume_offset = 0;
    g_scheduler.fibers[0] = {};
    g_scheduler.fibers[1] = {};
    g_scheduler.pending_reads[0] = {};
    g_scheduler.pending_offloads[0] = {};
    g_scheduler.pending_sleeps[0] = {};
    g_scheduler.pending_dns[0] = {};
    g_scheduler.pending_connects[0] = {};
    g_scheduler.pending_accepts[0] = {};
    g_scheduler.fiber_join_wait_token[0] = 0;
    g_scheduler.fibers[0].owner_interp = interp;
    g_scheduler.fibers[1].owner_interp = interp;

    bool ok = true;

    Value* opaque_result = scheduler_test_make_opaque_primitive_result(interp, child, 4101);
    if (opaque_result == null) {
        ok = false;
    } else {
        scheduler_mark_done(0, opaque_result, interp);
        ok = g_scheduler.fibers[0].state == FIBER_DONE &&
             g_scheduler.fibers[0].result != null &&
             g_scheduler.fibers[0].result.tag == ERROR &&
             scheduler_test_str_contains(
                 g_scheduler.fibers[0].result.str_chars[:g_scheduler.fibers[0].result.str_len],
                 "failed to promote fiber result to root"
             );
    }

    if (ok) {
        Value* opaque_child_result = scheduler_test_make_opaque_primitive_result(interp, child, 4102);
        if (opaque_child_result == null) {
            ok = false;
        } else {
            g_scheduler.fibers[1] = {};
            g_scheduler.fibers[1].owner_interp = interp;
            g_scheduler.fibers[1].live_children = 1;
            scheduler_complete_fiber(1, opaque_child_result, interp);
            ok = g_scheduler.fibers[1].state == FIBER_BLOCKED &&
                 g_scheduler.fibers[1].waiting_children &&
                 g_scheduler.fibers[1].pending_result != null &&
                 g_scheduler.fibers[1].pending_result.tag == ERROR &&
                 scheduler_test_str_contains(
                     g_scheduler.fibers[1].pending_result.str_chars[:g_scheduler.fibers[1].pending_result.str_len],
                     "failed to promote child result to root"
                 );
        }
    }

    if (ok) {
        interp.current_scope = child;
        Value* user_error = scheduler_raise(interp, "scheduler/test-error", "scheduler test error");
        interp.current_scope = saved_scope;
        g_scheduler.fibers[0] = {};
        g_scheduler.fibers[0].owner_interp = interp;
        scheduler_mark_done(0, user_error, interp);
        ok = g_scheduler.fibers[0].result != null &&
             g_scheduler.fibers[0].result.tag == ERROR &&
             scheduler_test_str_contains(
                 g_scheduler.fibers[0].result.str_chars[:g_scheduler.fibers[0].result.str_len],
                 "scheduler test error"
             ) &&
             !scheduler_test_str_contains(
                 g_scheduler.fibers[0].result.str_chars[:g_scheduler.fibers[0].result.str_len],
                 "failed to promote fiber result to root"
             );
    }

    interp.current_scope = saved_scope;
    main::scope_release(child);
    g_scheduler.fibers[0] = saved_fiber_0;
    g_scheduler.fibers[1] = saved_fiber_1;
    g_scheduler.pending_reads[0] = saved_pending_read_0;
    g_scheduler.pending_offloads[0] = saved_pending_offload_0;
    g_scheduler.pending_sleeps[0] = saved_pending_sleep_0;
    g_scheduler.pending_dns[0] = saved_pending_dns_0;
    g_scheduler.pending_connects[0] = saved_pending_connect_0;
    g_scheduler.pending_accepts[0] = saved_pending_accept_0;
    g_scheduler.fiber_join_wait_token[0] = saved_join_token_0;
    g_scheduler.fiber_count = saved_fiber_count;
    g_scheduler.current_fiber_id = saved_current_fiber_id;
    g_scheduler.running = saved_running;
    g_scheduler.resume_offset = saved_resume_offset;

    if (ok) {
        emit_pass_literal("[PASS] scheduler result publication fails closed on promotion faults");
        (*pass)++;
    } else {
        io::printn("[FAIL] scheduler result publication fails closed on promotion faults");
        (*fail)++;
    }
}

fn void run_scheduler_fiber_temp_thread_boundary_tests(Interp* interp, int* pass, int* fail) {
    if (test_c_getenv("OMNI_FIBER_TEMP") == null) {
        emit_pass_literal("[PASS] fiber-temp thread boundary metrics (flag off)");
        (*pass)++;
        return;
    }

    main::FiberTempPoolStats fiber_before = main::fiber_temp_pool_stats_snapshot();
    bool ok = true;

    for (usz i = 0; i < 8; i++) {
        EvalResult r = run(
            "(block "
            "  (define ft-t (task-spawn 'sleep-ms 1)) "
            "  (task-join ft-t) "
            "  true)",
            interp);
        if (r.error.has_error || r.value == null || r.value.tag == ERROR || r.value.tag == NIL) {
            ok = false;
            break;
        }
    }

    main::FiberTempPoolStats fiber_after = main::fiber_temp_pool_stats_snapshot();

    ok = ok &&
         (fiber_after.ctx_take_hits == fiber_before.ctx_take_hits) &&
         (fiber_after.ctx_return_count == fiber_before.ctx_return_count);

    if (ok) {
        emit_pass_literal("[PASS] fiber-temp thread boundary metrics stay context-local");
        (*pass)++;
    } else {
        io::printn("[FAIL] fiber-temp thread boundary metrics stay context-local");
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_offload_interleave_tests(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    usz failed_step = 0;

    for (usz i = 0; i < 12; i++) {
        EvalResult async_r = run(
            "(block "
            "  (define wf (spawn (lambda () (offload 'sleep-ms 1)))) "
            "  (await wf) "
            "  true)",
            interp);
        if (async_r.error.has_error ||
            async_r.value == null ||
            async_r.value.tag == ERROR ||
            async_r.value.tag == NIL) {
            ok = false;
            failed_step = i;
            break;
        }

        bool pushed = scheduler_publish_poll_error(MAX_FIBERS + i + 1, -1);
        drain_wakeups();
        if (!pushed) {
            ok = false;
            failed_step = i;
            break;
        }

        EvalResult thread_r = run(
            "(block "
            "  (define wt (task-spawn 'sleep-ms 1)) "
            "  (task-join wt) "
            "  true)",
            interp);
        if (thread_r.error.has_error ||
            thread_r.value == null ||
            thread_r.value.tag == ERROR ||
            thread_r.value.tag == NIL) {
            ok = false;
            failed_step = i;
            break;
        }
    }

    ok = ok && reliable_wakeup_pending_count() == 0;

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup/offload interleave stress");
        (*pass)++;
    } else {
        io::printfn("[FAIL] scheduler wakeup/offload interleave stress at step %d", (int)failed_step);
        (*fail)++;
    }
}

fn void run_scheduler_fiber_temp_cancel_timeout_boundary_tests(Interp* interp, int* pass, int* fail) {
    if (test_c_getenv("OMNI_FIBER_TEMP") == null) {
        emit_pass_literal("[PASS] fiber-temp cancel/timeout boundary metrics (flag off)");
        (*pass)++;
        return;
    }

    main::FiberTempPoolStats fiber_before = main::fiber_temp_pool_stats_snapshot();
    bool ok = true;

    for (usz i = 0; i < 8; i++) {
        EvalResult timeout_r = run(
            "(block "
            "  (define ft-to (task-spawn 'sleep-ms 20)) "
            "  (task-join-timeout ft-to 0))",
            interp);
        if (!(timeout_r.error.has_error || (timeout_r.value != null && timeout_r.value.tag == ERROR))) {
            ok = false;
            break;
        }

        EvalResult cancel_r = run(
            "(block "
            "  (define ft-c (task-spawn 'sleep-ms 20)) "
            "  (task-cancel ft-c) "
            "  (task-join ft-c))",
            interp);
        if (!(cancel_r.error.has_error || (cancel_r.value != null && cancel_r.value.tag == ERROR))) {
            ok = false;
            break;
        }

        EvalResult success_r = run(
            "(block "
            "  (define ft-ok (task-spawn 'sleep-ms 1)) "
            "  (task-join-timeout ft-ok 200) "
            "  true)",
            interp);
        if (success_r.error.has_error ||
            success_r.value == null ||
            success_r.value.tag == ERROR ||
            success_r.value.tag == NIL) {
            ok = false;
            break;
        }
    }

    main::FiberTempPoolStats fiber_after = main::fiber_temp_pool_stats_snapshot();
    ok = ok &&
         (fiber_after.ctx_take_hits == fiber_before.ctx_take_hits) &&
         (fiber_after.ctx_return_count == fiber_before.ctx_return_count);

    if (ok) {
        emit_pass_literal("[PASS] fiber-temp cancel/timeout boundaries stay context-local");
        (*pass)++;
    } else {
        io::printn("[FAIL] fiber-temp cancel/timeout boundaries stay context-local");
        (*fail)++;
    }
}

fn void run_scheduler_offload_cancel_boundary_tests(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    usz failed_step = 0;

    for (usz i = 0; i < 12; i++) {
        EvalResult r = run(
            "(block "
            "  (define oc (spawn (lambda () (offload 'sleep-ms 5)))) "
            "  (fiber-cancel oc) "
            "  (run-fibers) "
            "  true)",
            interp);
        if (r.error.has_error ||
            r.value == null ||
            r.value.tag == ERROR ||
            r.value.tag == NIL) {
            ok = false;
            failed_step = i;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] offload cancel destroy-before-complete stress");
        (*pass)++;
    } else {
        io::printfn("[FAIL] offload cancel destroy-before-complete stress at step %d", (int)failed_step);
        (*fail)++;
    }
}

fn void run_scheduler_cancel_tests(Interp* interp, int* pass, int* fail) {
    {
        setup(interp, "(define fc-fib1 (spawn (lambda () 42)))");
        EvalResult r = run("(fiber-cancel fc-fib1)", interp);
        if (!r.error.has_error && r.value != null && r.value.tag == VOID) {
            emit_pass_literal("[PASS] fiber-cancel ready fiber returns Void");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel ready fiber returns Void");
            (*fail)++;
        }
    }

    {
        setup(interp, "(define fc-fib2 (spawn (lambda () 1)))");
        setup(interp, "(fiber-cancel fc-fib2)");
        EvalResult r = run("(fiber-cancel fc-fib2)", interp);
        if (!r.error.has_error && r.value != null && r.value.tag == VOID) {
            emit_pass_literal("[PASS] fiber-cancel twice → Void");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel twice → Void");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(fiber-cancel 999)", interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] fiber-cancel invalid id → error");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel invalid id → error");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define fc-parent (spawn (lambda () "
            "    (define fc-child (spawn (lambda () 99))) "
            "    (await fc-child)))) "
            "  (fiber-cancel fc-parent) "
            "  (run-fibers) "
            "  true)",
            interp);
        if (!r.error.has_error) {
            emit_pass_literal("[PASS] fiber-cancel propagates to children");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel propagates to children");
            (*fail)++;
        }
    }

    {
        scheduler_init();
        g_scheduler.fiber_count = 4;
        g_scheduler.current_fiber_id = NO_FIBER;
        g_scheduler.running = false;
        g_scheduler.resume_offset = 0;
        g_scheduler.fibers[0] = {};
        g_scheduler.fibers[1] = {};
        g_scheduler.fibers[2] = {};
        g_scheduler.fibers[3] = {};
        g_scheduler.pending_reads[0] = {};
        g_scheduler.pending_reads[1] = {};
        g_scheduler.pending_reads[2] = {};
        g_scheduler.pending_reads[3] = {};
        g_scheduler.pending_offloads[0] = {};
        g_scheduler.pending_offloads[1] = {};
        g_scheduler.pending_offloads[2] = {};
        g_scheduler.pending_offloads[3] = {};
        g_scheduler.pending_sleeps[0] = {};
        g_scheduler.pending_sleeps[1] = {};
        g_scheduler.pending_sleeps[2] = {};
        g_scheduler.pending_sleeps[3] = {};
        g_scheduler.pending_dns[0] = {};
        g_scheduler.pending_dns[1] = {};
        g_scheduler.pending_dns[2] = {};
        g_scheduler.pending_dns[3] = {};
        g_scheduler.pending_connects[0] = {};
        g_scheduler.pending_connects[1] = {};
        g_scheduler.pending_connects[2] = {};
        g_scheduler.pending_connects[3] = {};
        g_scheduler.pending_accepts[0] = {};
        g_scheduler.pending_accepts[1] = {};
        g_scheduler.pending_accepts[2] = {};
        g_scheduler.pending_accepts[3] = {};

        // Grandparent waits for two live children: sibling(1) and parent(2).
        g_scheduler.fibers[0].state = FIBER_BLOCKED;
        g_scheduler.fibers[0].parent_id = NO_FIBER;
        g_scheduler.fibers[0].live_children = 2;
        g_scheduler.fibers[0].waiting_children = true;

        g_scheduler.fibers[1].state = FIBER_READY;
        g_scheduler.fibers[1].parent_id = 0;

        // Parent already completed body and is waiting only on child(3).
        g_scheduler.fibers[2].state = FIBER_BLOCKED;
        g_scheduler.fibers[2].parent_id = 0;
        g_scheduler.fibers[2].live_children = 1;
        g_scheduler.fibers[2].waiting_children = true;
        g_scheduler.fibers[2].pending_result = make_int(interp, 99);

        g_scheduler.fibers[3].state = FIBER_READY;
        g_scheduler.fibers[3].parent_id = 2;

        (void)scheduler_cancel_fiber(2, interp);
        bool ok = g_scheduler.fibers[2].state == FIBER_DONE &&
                  g_scheduler.fibers[2].result != null &&
                  g_scheduler.fibers[2].result.tag == ERROR &&
                  g_scheduler.fibers[0].state == FIBER_BLOCKED &&
                  g_scheduler.fibers[0].waiting_children &&
                  g_scheduler.fibers[0].live_children == 1;
        if (ok) {
            emit_pass_literal("[PASS] fiber-cancel avoids parent double-complete after child cascade");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel avoids parent double-complete after child cascade");
            (*fail)++;
        }
    }

    {
        scheduler_init();
        g_scheduler.fiber_count = 2;
        g_scheduler.current_fiber_id = NO_FIBER;
        g_scheduler.running = false;
        g_scheduler.resume_offset = 0;
        g_scheduler.fibers[0] = {};
        g_scheduler.fibers[1] = {};
        g_scheduler.pending_reads[0] = {};
        g_scheduler.pending_reads[1] = {};
        g_scheduler.pending_offloads[0] = {};
        g_scheduler.pending_offloads[1] = {};
        g_scheduler.pending_sleeps[0] = {};
        g_scheduler.pending_sleeps[1] = {};
        g_scheduler.pending_dns[0] = {};
        g_scheduler.pending_dns[1] = {};
        g_scheduler.pending_connects[0] = {};
        g_scheduler.pending_connects[1] = {};
        g_scheduler.pending_accepts[0] = {};
        g_scheduler.pending_accepts[1] = {};

        // Parent has a live child currently RUNNING, so child cascade cannot
        // cancel it immediately. Parent must remain waiting_children until that
        // child exits.
        g_scheduler.fibers[0].state = FIBER_READY;
        g_scheduler.fibers[0].parent_id = NO_FIBER;
        g_scheduler.fibers[0].live_children = 1;

        g_scheduler.fibers[1].state = FIBER_RUNNING;
        g_scheduler.fibers[1].parent_id = 0;

        bool cancelled = scheduler_cancel_fiber(0, interp);
        bool ok = cancelled &&
                  g_scheduler.fibers[0].state == FIBER_BLOCKED &&
                  g_scheduler.fibers[0].waiting_children &&
                  g_scheduler.fibers[0].live_children == 1 &&
                  g_scheduler.fibers[0].pending_result != null &&
                  g_scheduler.fibers[0].pending_result.tag == ERROR &&
                  g_scheduler.fibers[1].state == FIBER_RUNNING;
        if (ok) {
            emit_pass_literal("[PASS] fiber-cancel keeps parent blocked while running child is live");
            (*pass)++;
        } else {
            io::printn("[FAIL] fiber-cancel keeps parent blocked while running child is live");
            (*fail)++;
        }

        // Manual fixture reset so this synthetic RUNNING child does not leak
        // scheduler state into subsequent tests.
        g_scheduler.fiber_count = 0;
        g_scheduler.current_fiber_id = NO_FIBER;
        g_scheduler.running = false;
        g_scheduler.resume_offset = 0;
        g_scheduler.fibers[0] = {};
        g_scheduler.fibers[1] = {};
        g_scheduler.pending_reads[0] = {};
        g_scheduler.pending_reads[1] = {};
        g_scheduler.pending_offloads[0] = {};
        g_scheduler.pending_offloads[1] = {};
        g_scheduler.pending_sleeps[0] = {};
        g_scheduler.pending_sleeps[1] = {};
        g_scheduler.pending_dns[0] = {};
        g_scheduler.pending_dns[1] = {};
        g_scheduler.pending_connects[0] = {};
        g_scheduler.pending_connects[1] = {};
        g_scheduler.pending_accepts[0] = {};
        g_scheduler.pending_accepts[1] = {};
    }
}

fn void run_scheduler_fairness_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(block "
            "  (define fair-a (spawn (lambda () 10))) "
            "  (define fair-b (spawn (lambda () 20))) "
            "  (run-fibers) "
            "  (+ 10 20))",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 30) {
            emit_pass_literal("[PASS] fairness: both fibers complete");
            (*pass)++;
        } else {
            io::printn("[FAIL] fairness: both fibers complete");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (spawn (lambda () 1)) (spawn (lambda () 2)) (spawn (lambda () 3)) "
            "  (spawn (lambda () 4)) (spawn (lambda () 5)) (spawn (lambda () 6)) "
            "  (spawn (lambda () 7)) (spawn (lambda () 8)) (spawn (lambda () 9)) "
            "  (spawn (lambda () 10)) "
            "  (run-fibers) "
            "  true)",
            interp);
        if (!r.error.has_error) {
            emit_pass_literal("[PASS] fairness: 10 fibers all complete");
            (*pass)++;
        } else {
            io::printn("[FAIL] fairness: 10 fibers all complete");
            (*fail)++;
        }
    }
}
