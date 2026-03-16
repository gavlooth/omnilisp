module lisp;


import std::io;
import std::core::mem;
import main;
fn void run_scheduler_spawn_and_await_tests(Interp* interp, int* pass, int* fail) {
    run_scheduler_spawn_basic_tests(interp, pass, fail);
    run_scheduler_spawn_error_tests(interp, pass, fail);
    run_scheduler_offload_setup_error_tests(interp, pass, fail);
    run_scheduler_spawn_reuse_capacity_tests(interp, pass, fail);
    run_scheduler_await_structure_tests(interp, pass, fail);
    run_scheduler_run_loop_failure_tests(interp, pass, fail);
}

fn bool scheduler_test_str_contains(char[] haystack, char[] needle) {
    if (needle.len == 0) return false;
    if (haystack.len < needle.len) return false;
    for (usz i = 0; i <= haystack.len - needle.len; i++) {
        bool ok = true;
        for (usz j = 0; j < needle.len; j++) {
            if (haystack[i + j] != needle[j]) {
                ok = false;
                break;
            }
        }
        if (ok) return true;
    }
    return false;
}

fn void run_scheduler_spawn_error_tests(Interp* interp, int* pass, int* fail) {
    {
        usz saved_fiber_count = g_scheduler.fiber_count;
        g_scheduler.fiber_count = MAX_FIBERS;
        EvalResult full = run("(spawn (lambda () 42))", interp);
        g_scheduler.fiber_count = saved_fiber_count;
        if (full.error.has_error && scheduler_test_str_contains(full.error.message[:256], "spawn: scheduler full")) {
            emit_pass_literal("[PASS] spawn reports scheduler full");
            (*pass)++;
        } else {
            io::printfn("[FAIL] spawn reports scheduler full (error=%s)", full.error.has_error ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* temp_scope = main::scope_create(saved_scope);
        if (temp_scope == null) {
            io::printn("[FAIL] spawn invalid coroutine path (alloc)");
            (*fail)++;
            return;
        }

        interp.current_scope = temp_scope;
        Value* broken = boundary_alloc_value_in_scope(interp, temp_scope, true);
        broken.tag = COROUTINE;
        broken.coroutine_val = null;
        bool promo_failed = false;
        usz id = scheduler_add_fiber(broken, NO_FIBER, interp, &promo_failed);

        main::scope_release(temp_scope);
        interp.current_scope = saved_scope;

        if (id == MAX_FIBERS && !promo_failed) {
            emit_pass_literal("[PASS] scheduler_add_fiber rejects invalid coroutine deterministically");
            (*pass)++;
        } else {
            io::printfn("[FAIL] scheduler_add_fiber invalid coroutine path (id=%d promo_failed=%s)", (long)id, promo_failed ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_scheduler_offload_setup_error_tests(Interp* interp, int* pass, int* fail) {
    scheduler_init();
    void* saved_uv_loop = g_scheduler.uv_loop;
    bool saved_uv_loop_ready = g_scheduler.uv_loop_ready;
    usz saved_fiber_count = g_scheduler.fiber_count;
    usz saved_current_fiber_id = g_scheduler.current_fiber_id;
    FiberEntry saved_fiber_0 = g_scheduler.fibers[0];
    PendingOffload saved_pending_offload_0 = g_scheduler.pending_offloads[0];

    g_scheduler.fiber_count = 1;
    g_scheduler.current_fiber_id = 0;
    g_scheduler.fibers[0] = {};
    g_scheduler.fibers[0].state = FIBER_RUNNING;
    g_scheduler.pending_offloads[0] = {};
    g_scheduler.uv_loop_ready = false;
    g_scheduler.uv_loop = null;

    OffloadWork work = {};
    work.op = OFFLOAD_SLEEP_MS;
    work.int_arg = 1;
    Value* err = scheduler_begin_fiber_offload(&work, 0, interp);

    bool pending_slot_clear = !g_scheduler.pending_offloads[0].active
        && !g_scheduler.pending_offloads[0].completed
        && g_scheduler.pending_offloads[0].completion == null;
    bool state_restored = g_scheduler.fibers[0].state == FIBER_RUNNING;

    g_scheduler.pending_offloads[0] = saved_pending_offload_0;
    g_scheduler.fibers[0] = saved_fiber_0;
    g_scheduler.current_fiber_id = saved_current_fiber_id;
    g_scheduler.fiber_count = saved_fiber_count;
    g_scheduler.uv_loop_ready = saved_uv_loop_ready;
    g_scheduler.uv_loop = saved_uv_loop;

    if (err != null && err.tag == ERROR && pending_slot_clear && state_restored) {
        emit_pass_literal("[PASS] offload setup failure rolls back pending slot and fiber state");
        (*pass)++;
    } else {
        io::printn("[FAIL] offload setup failure rollback");
        (*fail)++;
    }
}

fn void run_scheduler_spawn_reuse_capacity_tests(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (let loop (i 0) "
        "    (if (= i 300) "
        "      true "
        "      (block "
        "        (define f (spawn (lambda () i))) "
        "        (await f) "
        "        (loop (+ i 1))))))",
        interp
    );
    if (!r.error.has_error && r.value != null && r.value.tag == SYMBOL && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] sequential spawn+await reuses scheduler capacity beyond MAX_FIBERS");
        (*pass)++;
    } else {
        io::printfn("[FAIL] sequential spawn+await capacity reuse (error=%s)", r.error.has_error ? "yes" : "no");
        (*fail)++;
    }
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
    test_tag(interp, "run-fibers returns Void", "(run-fibers)", VOID, pass, fail);

    {
        EvalResult r = run(
            "(block "
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
            "(block "
            "  (define sched-done 0) "
            "  (define p "
            "    (spawn (lambda () "
            "      (block "
            "        (spawn (lambda () (block (set! sched-done 1) 99))) "
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
            "(block "
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
        Interp* foreign = (Interp*)mem::malloc(Interp.sizeof);
        bool cross_owner_ok = foreign != null;
        usz foreign_id = usz.max;
        if (cross_owner_ok) {
            foreign.init();
            register_primitives(foreign);
            register_stdlib(foreign);
            EvalResult spawned = run("(spawn (lambda () 33))", foreign);
            cross_owner_ok = !spawned.error.has_error &&
                             spawned.value != null &&
                             spawned.value.tag == INT &&
                             spawned.value.int_val >= 0;
            if (cross_owner_ok) foreign_id = (usz)spawned.value.int_val;
        }

        if (cross_owner_ok) {
            char[96] buf;
            char[] src = io::bprintf(&buf, "(await %d)", (long)foreign_id)!!;
            EvalResult local_wait = run(src, interp);
            cross_owner_ok = (local_wait.error.has_error &&
                              scheduler_test_str_contains(
                                  local_wait.error.message[:256],
                                  "fiber belongs to a different interpreter"
                              )) ||
                             (local_wait.value != null &&
                              local_wait.value.tag == ERROR &&
                              scheduler_test_str_contains(
                                  local_wait.value.str_chars[:local_wait.value.str_len],
                                  "fiber belongs to a different interpreter"
                              ));
        }

        if (foreign != null) {
            if (foreign_id != usz.max) {
                char[96] cleanup_buf;
                char[] cleanup_src = io::bprintf(&cleanup_buf, "(await %d)", (long)foreign_id)!!;
                (void)run(cleanup_src, foreign);
            }
            foreign.destroy();
            mem::free(foreign);
        }

        if (cross_owner_ok) {
            emit_pass_literal("[PASS] cross-interpreter await rejected");
            (*pass)++;
        } else {
            io::printn("[FAIL] cross-interpreter await rejected");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define p "
            "    (spawn (lambda () "
            "      (checkpoint "
            "        (block "
            "          (define c (spawn (lambda () 7))) "
            "          (await c)))))) "
            "  (await p))",
            interp);
        bool nested_rejected = false;
        if (r.error.has_error) {
            nested_rejected = scheduler_test_str_contains(
                r.error.message[:256],
                "blocking scheduler op requires the fiber root context"
            );
        } else if (r.value != null && r.value.tag == ERROR) {
            nested_rejected = scheduler_test_str_contains(
                r.value.str_chars[:r.value.str_len],
                "blocking scheduler op requires the fiber root context"
            );
        }
        if (nested_rejected) {
            emit_pass_literal("[PASS] nested-context await in fiber rejected");
            (*pass)++;
        } else {
            io::printn("[FAIL] nested-context await in fiber rejected");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define p (spawn (lambda () (run-fibers)))) "
            "  (await p))",
            interp);
        bool nested_run_rejected = false;
        if (r.error.has_error) {
            nested_run_rejected = scheduler_test_str_contains(
                r.error.message[:256],
                "scheduler run loop is already active"
            );
        } else if (r.value != null && r.value.tag == ERROR) {
            nested_run_rejected = scheduler_test_str_contains(
                r.value.str_chars[:r.value.str_len],
                "scheduler run loop is already active"
            );
        }
        if (nested_run_rejected) {
            emit_pass_literal("[PASS] nested run-fibers in fiber rejected");
            (*pass)++;
        } else {
            io::printn("[FAIL] nested run-fibers in fiber rejected");
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
        "(block "
        "  (define p (spawn (lambda () (let loop (i 0) (block (yield nil) (loop i)))))) "
        "  (await p))",
        "max rounds",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "run-fibers surfaces round-limit failure",
        "(block "
        "  (spawn (lambda () (let loop (i 0) (block (yield nil) (loop i))))) "
        "  (run-fibers))",
        "max rounds",
        pass,
        fail
    );

    {
        EvalResult r = run(
            "(block "
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

fn void run_scheduler_offload_wakeup_epoch_guard_test(Interp* interp, int* pass, int* fail) {
    void* saved_async = null;
    scheduler_test_prepare_blocked_pending_offload(&saved_async);

    g_scheduler.fibers[0].wake_epoch = 7001;
    g_scheduler.pending_offloads[0].wake_epoch = 7001;

    OffloadCompletion* stale = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    OffloadCompletion* fresh = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    bool ok = stale != null && fresh != null;
    if (ok) {
        *stale = {};
        stale.kind = OFFLOAD_RES_INT;
        stale.int_result = 11;
        *fresh = {};
        fresh.kind = OFFLOAD_RES_INT;
        fresh.int_result = 42;
    } else {
        if (stale != null) scheduler_free_offload_completion(stale);
        if (fresh != null) scheduler_free_offload_completion(fresh);
    }

    if (ok) {
        bool pushed_stale = scheduler_publish_offload_ready(0, stale, 7000);
        if (!pushed_stale) scheduler_free_offload_completion(stale);
        drain_wakeups();
        ok = pushed_stale &&
             g_scheduler.pending_offloads[0].active &&
             !g_scheduler.pending_offloads[0].completed &&
             g_scheduler.pending_offloads[0].completion == null &&
             g_scheduler.fibers[0].state == FIBER_BLOCKED;

        bool pushed_fresh = scheduler_publish_offload_ready(0, fresh, 7001);
        if (!pushed_fresh) scheduler_free_offload_completion(fresh);
        drain_wakeups();
        bool delivered = pushed_fresh &&
                         g_scheduler.pending_offloads[0].active &&
                         g_scheduler.pending_offloads[0].completed &&
                         g_scheduler.pending_offloads[0].completion == fresh &&
                         g_scheduler.fibers[0].state == FIBER_READY;
        Value* consumed = delivered ? scheduler_consume_pending_offload(0, interp) : null;
        bool consumed_ok = consumed != null &&
                           consumed.tag == INT &&
                           consumed.int_val == 42;
        bool slot_reset = !g_scheduler.pending_offloads[0].active &&
                          !g_scheduler.pending_offloads[0].completed &&
                          g_scheduler.pending_offloads[0].completion == null;
        ok = ok && delivered && consumed_ok && slot_reset;
    }

    scheduler_test_cleanup_single_fiber_no_async(saved_async);

    if (ok) {
        emit_pass_literal("[PASS] offload wakeup epoch guard drops stale recycled-slot completion");
        (*pass)++;
    } else {
        io::printn("[FAIL] offload wakeup epoch guard drops stale recycled-slot completion");
        (*fail)++;
    }
}

fn void run_scheduler_atomic_cas_semantics_test(Interp* interp, int* pass, int* fail) {
    setup(interp, "(define sched-cas (atomic 7))");
    EvalResult ok_swap = run("(block (atomic-cas! sched-cas 7 9) (atomic-read sched-cas))", interp);
    EvalResult bad_swap = run("(block (atomic-cas! sched-cas 7 11) (atomic-read sched-cas))", interp);
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
    run_scheduler_offload_wakeup_epoch_guard_test(interp, pass, fail);
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
    run_scheduler_thread_task_null_completion_fail_closed_boundary_tests(interp, pass, fail);
    run_scheduler_waiter_take_does_not_strand_blocked_fiber_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_cancel_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_worker_cancel_interleave_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_nonexecuted_payload_release_boundary_tests(interp, pass, fail);
    run_scheduler_worker_prestart_cancel_single_shared_release_boundary_tests(interp, pass, fail);
    run_scheduler_dns_connect_request_token_guard_boundary_tests(interp, pass, fail);
    run_scheduler_task_join_timeout_then_join_boundary_tests(interp, pass, fail);
    run_scheduler_shared_retire_queue_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_ready_barrier_boundary_tests(interp, pass, fail);
    run_scheduler_mixed_boundary_state_restore_tests(interp, pass, fail);
    run_scheduler_wakeup_offload_error_boundary_tests(interp, pass, fail);
}

fn void run_scheduler_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_scheduler_tests, pass, fail);
}
