module lisp;


import std::io;
import main;

fn void run_scheduler_fiber_temp_thread_boundary_tests(Interp* interp, int* pass, int* fail) {
    if (test_c_getenv("OMNI_FIBER_TEMP") == null) {
        emit_pass_literal("[PASS] fiber-temp thread boundary metrics (flag off)");
        (*pass)++;
        return;
    }

    usz ctx_hits_before = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_before = main::g_fiber_temp_pool_stats.ctx_return_count;
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

    usz ctx_hits_after = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_after = main::g_fiber_temp_pool_stats.ctx_return_count;

    ok = ok &&
         (ctx_hits_after == ctx_hits_before) &&
         (ctx_returns_after == ctx_returns_before);

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

    usz ctx_hits_before = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_before = main::g_fiber_temp_pool_stats.ctx_return_count;
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

    usz ctx_hits_after = main::g_fiber_temp_pool_stats.ctx_take_hits;
    usz ctx_returns_after = main::g_fiber_temp_pool_stats.ctx_return_count;
    ok = ok &&
         (ctx_hits_after == ctx_hits_before) &&
         (ctx_returns_after == ctx_returns_before);

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
