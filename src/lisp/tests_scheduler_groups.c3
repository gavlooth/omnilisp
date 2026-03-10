module lisp;


import std::io;
import main;
fn void run_scheduler_spawn_and_await_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_spawn_basic_tests(interp, pass, fail);
    run_scheduler_await_structure_tests(interp, pass, fail);
    run_scheduler_run_loop_failure_tests(interp, pass, fail);
}

fn void run_scheduler_spawn_basic_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(spawn (lambda () 42))", interp);
        if (!r.error.has_error && is_int(r.value)) {
            emit_pass_literal("[PASS] spawn returns id");
            (*pass)++;
        } else {
            io::printn("[FAIL] spawn returns id");
            (*fail)++;
        }
    }

    {
        setup(interp, "(spawn (lambda () (+ 1 2)))");
        EvalResult r = run("(run-fibers)", interp);
        if (!r.error.has_error) {
            emit_pass_literal("[PASS] run-fibers completes");
            (*pass)++;
        } else {
            io::printn("[FAIL] run-fibers");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define p (spawn (lambda () (let (c (spawn (lambda () 7))) (+ 1 (await c)))))) "
            "  (await p))",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 8) {
            emit_pass_literal("[PASS] await direct child");
            (*pass)++;
        } else {
            io::printfn("[FAIL] await direct child (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }
}

fn void run_scheduler_await_structure_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run(
            "(begin "
            "  (define sched-done 0) "
            "  (define p "
            "    (spawn (lambda () "
            "      (begin "
            "        (spawn (lambda () (begin (set! sched-done 1) 99))) "
            "        42)))) "
            "  (await p) "
            "  sched-done)",
            interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 1) {
            emit_pass_literal("[PASS] parent waits for unawaited child");
            (*pass)++;
        } else {
            io::printfn("[FAIL] parent waits for unawaited child (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(begin "
            "  (define b (spawn (lambda () 99))) "
            "  (define a (spawn (lambda () (await b)))) "
            "  (await a))",
            interp);
        if (r.error.has_error || (r.value != null && r.value.tag == ERROR)) {
            emit_pass_literal("[PASS] await non-child rejected");
            (*pass)++;
        } else {
            io::printn("[FAIL] await non-child rejected");
            (*fail)++;
        }
    }

    {
        setup(interp, "(define fib-a (spawn (lambda () 10)))");
        setup(interp, "(define fib-b (spawn (lambda () 20)))");
        setup(interp, "(run-fibers)");
        emit_pass_literal("[PASS] run-fibers completes");
        (*pass)++;
    }
}

fn void run_scheduler_run_loop_failure_tests(Interp* interp, int* pass, int* fail) {
    test_error_contains(
        interp,
        "await surfaces round-limit failure",
        "(begin "
        "  (define p (spawn (lambda () (let loop (i 0) (begin (yield nil) (loop i)))))) "
        "  (await p))",
        "max rounds",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "run-fibers surfaces round-limit failure",
        "(begin "
        "  (spawn (lambda () (let loop (i 0) (begin (yield nil) (loop i))))) "
        "  (run-fibers))",
        "max rounds",
        pass,
        fail
    );

    {
        EvalResult r = run(
            "(begin "
            "  (spawn (lambda () 7)) "
            "  (run-fibers) "
            "  true)",
            interp
        );
        if (!r.error.has_error &&
            r.value != null &&
            r.value.tag != ERROR &&
            r.value.tag != NIL) {
            emit_pass_literal("[PASS] scheduler recovers after run-loop failure");
            (*pass)++;
        } else {
            io::printn("[FAIL] scheduler recovers after run-loop failure");
            (*fail)++;
        }
    }
}

fn void scheduler_test_prepare_no_async(void** saved_async) {
    scheduler_init();
    *saved_async = g_scheduler.async_handle;
    g_scheduler.async_handle = null;
    scheduler_reset_wakeup_queue();
}

fn void scheduler_test_restore_no_async(void* saved_async) {
    g_scheduler.async_handle = saved_async;
}

fn void scheduler_test_cleanup_no_async(void* saved_async) {
    scheduler_reset_wakeup_queue();
    scheduler_test_restore_no_async(saved_async);
}

fn void scheduler_test_prepare_single_fiber_no_async(void** saved_async, FiberState state) {
    scheduler_test_prepare_no_async(saved_async);
    g_scheduler.fiber_count = 1;
    g_scheduler.current_fiber_id = NO_FIBER;
    g_scheduler.fibers[0] = {};
    g_scheduler.fibers[0].state = state;
}

fn void scheduler_test_prepare_blocked_pending_read(void** saved_async) {
    scheduler_test_prepare_single_fiber_no_async(saved_async, FIBER_BLOCKED);
    g_scheduler.pending_reads[0] = {};
    g_scheduler.pending_reads[0].active = true;
}

fn void scheduler_test_prepare_blocked_pending_offload(void** saved_async) {
    scheduler_test_prepare_single_fiber_no_async(saved_async, FIBER_BLOCKED);
    g_scheduler.pending_offloads[0] = {};
    g_scheduler.pending_offloads[0].active = true;
}

fn void scheduler_test_cleanup_single_fiber_no_async(void* saved_async) {
    if (g_scheduler.pending_reads[0].buffer != null) {
        mem::free(g_scheduler.pending_reads[0].buffer);
    }
    if (g_scheduler.pending_offloads[0].completion != null) {
        scheduler_free_offload_completion(g_scheduler.pending_offloads[0].completion);
    }
    g_scheduler.pending_reads[0] = {};
    g_scheduler.pending_offloads[0] = {};
    g_scheduler.fibers[0] = {};
    g_scheduler.fiber_count = 0;
    scheduler_reset_wakeup_queue();
    scheduler_test_restore_no_async(saved_async);
}

fn void run_scheduler_wakeup_smoke_test(int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);

    usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
    bool pushed = scheduler_publish_timer_expired(MAX_FIBERS + 1);
    drain_wakeups();
    usz pending = reliable_wakeup_pending_count();
    usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();
    scheduler_test_cleanup_no_async(saved_async);

    if (pushed && pending == 0 && (enqueues_after - enqueues_before) == 1) {
        emit_pass_literal("[PASS] reliable wakeup smoke");
        (*pass)++;
    } else {
        io::printfn("[FAIL] reliable wakeup smoke (pushed=%s pending=%d enqueues=%d)",
            pushed ? "yes" : "no", (long)pending, (long)(enqueues_after - enqueues_before));
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_wraparound_test(int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_no_async(&saved_async);
    usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
    usz failures_before = g_scheduler.wakeup_enqueue_failures.load();

    usz target = 1024;
    usz pushed = 0;
    for (usz i = 0; i < target; i++) {
        if (scheduler_publish_poll_error(MAX_FIBERS + i + 1, -1)) pushed++;
    }
    drain_wakeups();
    usz pending = reliable_wakeup_pending_count();
    usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();
    usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
    scheduler_test_cleanup_no_async(saved_async);

    if (pushed == target &&
        pending == 0 &&
        (enqueues_after - enqueues_before) == target &&
        (failures_after - failures_before) == 0) {
        emit_pass_literal("[PASS] reliable wakeup flood");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] reliable wakeup flood (pushed=%d pending=%d enqueues=%d failures=%d)",
            (long)pushed,
            (long)pending,
            (long)(enqueues_after - enqueues_before),
            (long)(failures_after - failures_before)
        );
        (*fail)++;
    }
}
fn void run_scheduler_timer_wakeup_test(int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_blocked_pending_read(&saved_async);

    bool pushed = scheduler_publish_timer_expired(0);
    drain_wakeups();

    bool ok = pushed &&
              g_scheduler.pending_reads[0].timed_out &&
              g_scheduler.pending_reads[0].completed &&
              g_scheduler.fibers[0].state == FIBER_READY;

    scheduler_test_cleanup_single_fiber_no_async(saved_async);

    if (ok) {
        emit_pass_literal("[PASS] timer wakeup transitions in drain");
        (*pass)++;
    } else {
        io::printn("[FAIL] timer wakeup transitions in drain");
        (*fail)++;
    }
}

fn void run_scheduler_atomic_cas_semantics_test(Interp* interp, int* pass, int* fail) {
    setup(interp, "(define sched-cas (atomic 7))");
    EvalResult ok_swap = run("(begin (atomic-cas! sched-cas 7 9) (atomic-read sched-cas))", interp);
    EvalResult bad_swap = run("(begin (atomic-cas! sched-cas 7 11) (atomic-read sched-cas))", interp);
    if (!ok_swap.error.has_error && is_int(ok_swap.value) && ok_swap.value.int_val == 9 &&
        !bad_swap.error.has_error && is_int(bad_swap.value) && bad_swap.value.int_val == 9) {
        emit_pass_literal("[PASS] atomic-cas semantics");
        (*pass)++;
    } else {
        io::printn("[FAIL] atomic-cas semantics");
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_wakeup_smoke_test(pass, fail);
    run_scheduler_wakeup_wraparound_test(pass, fail);
    run_scheduler_timer_wakeup_test(pass, fail);
    run_scheduler_atomic_cas_semantics_test(interp, pass, fail);
}

fn void run_scheduler_tests(Interp* interp, int* pass, int* fail) {
    io::printn("\n--- Scheduler Tests ---");
    run_scheduler_spawn_and_await_tests(interp, pass, fail);
    run_scheduler_wakeup_tests(interp, pass, fail);
    run_scheduler_offload_thread_tests(interp, pass, fail);
    run_scheduler_fiber_temp_thread_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_offload_interleave_tests(interp, pass, fail);
    run_scheduler_fiber_temp_cancel_timeout_boundary_tests(interp, pass, fail);
    run_scheduler_offload_cancel_boundary_tests(interp, pass, fail);
    run_scheduler_cancel_tests(interp, pass, fail);
    run_scheduler_fairness_tests(interp, pass, fail);
    run_scheduler_boundary_stress_tests(interp, pass, fail);
    run_scheduler_wakeup_wraparound_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_mixed_event_boundary_tests(interp, pass, fail);
    run_scheduler_invalid_offload_wakeup_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_drop_counter_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_full_payload_ownership_boundary_tests(interp, pass, fail);
    run_scheduler_inactive_offload_ready_payload_cleanup_boundary_tests(interp, pass, fail);
    run_scheduler_offload_worker_retry_full_wakeup_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_offload_ready_barrier_boundary_tests(interp, pass, fail);
    run_scheduler_duplicate_offload_ready_boundary_tests(interp, pass, fail);
    run_scheduler_consume_pending_offload_boundary_tests(interp, pass, fail);
    run_scheduler_consume_pending_tcp_read_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_completion_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_cancel_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_worker_cancel_interleave_boundary_tests(interp, pass, fail);
    run_scheduler_task_join_timeout_then_join_boundary_tests(interp, pass, fail);
    run_scheduler_shared_retire_queue_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_ready_barrier_boundary_tests(interp, pass, fail);
    run_scheduler_mixed_boundary_state_restore_tests(interp, pass, fail);
    run_scheduler_wakeup_offload_error_boundary_tests(interp, pass, fail);
}

fn void run_scheduler_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_scheduler_tests, pass, fail);
}
