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
    scheduler_test_expect_truthy_success(
        interp,
        "(block "
        "  (let loop (i 0) "
        "    (if (= i 300) "
        "      true "
        "      (block "
        "        (define f (spawn (lambda () i))) "
        "        (await f) "
        "        (loop (+ i 1))))))",
        "[PASS] sequential spawn+await reuses scheduler capacity beyond MAX_FIBERS",
        "[FAIL] sequential spawn+await capacity reuse",
        pass,
        fail
    );
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

    scheduler_test_expect_int_exact(
        interp,
        "(block "
        "  (define p (spawn (lambda () (let (c (spawn (lambda () 7))) (+ 1 (await c)))))) "
        "  (await p))",
        8,
        "[PASS] await direct child",
        "[FAIL] await direct child (got %d)",
        pass,
        fail
    );
}

fn void run_scheduler_await_structure_tests(Interp* interp, int* pass, int* fail) {
    scheduler_test_expect_int_exact(
        interp,
        "(block "
        "  (define sched-done 0) "
        "  (define p "
        "    (spawn (lambda () "
        "      (block "
        "        (spawn (lambda () (block (set! sched-done 1) 99))) "
        "        42)))) "
        "  (await p) "
        "  sched-done)",
        1,
        "[PASS] parent waits for unawaited child",
        "[FAIL] parent waits for unawaited child (got %d)",
        pass,
        fail
    );

    scheduler_test_expect_error(
        interp,
        "(block "
        "  (define b (spawn (lambda () 99))) "
        "  (define a (spawn (lambda () (await b)))) "
        "  (await a))",
        "[PASS] await non-child rejected",
        "[FAIL] await non-child rejected",
        pass,
        fail
    );

    {
        Interp* foreign = (Interp*)mem::malloc(Interp.sizeof);
        bool cross_owner_ok = foreign != null;
        usz foreign_id = usz.max;
        if (cross_owner_ok) {
            foreign.init();
            cross_owner_ok = bootstrap_runtime_interp(foreign);
            if (cross_owner_ok) {
                EvalResult spawned = run("(spawn (lambda () 33))", foreign);
                cross_owner_ok = !spawned.error.has_error &&
                                 spawned.value != null &&
                                 spawned.value.tag == INT &&
                                 spawned.value.int_val >= 0;
                if (cross_owner_ok) foreign_id = (usz)spawned.value.int_val;
            }
        }

        if (cross_owner_ok) {
            char[96] buf;
            if (try char[] src = io::bprintf(&buf, "(await %d)", (long)foreign_id)) {
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
            } else {
                cross_owner_ok = false;
            }
        }

        if (foreign != null) {
            if (foreign_id != usz.max) {
                char[96] cleanup_buf;
                if (try char[] cleanup_src = io::bprintf(&cleanup_buf, "(await %d)", (long)foreign_id)) {
                    (void)run(cleanup_src, foreign);
                }
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
        "await: scheduler exceeded max rounds before target completed",
        pass,
        fail
    );

    test_error_contains(
        interp,
        "run-fibers surfaces round-limit failure",
        "(block "
        "  (spawn (lambda () (let loop (i 0) (block (yield nil) (loop i))))) "
        "  (run-fibers))",
        "run-fibers: scheduler exceeded max rounds before all fibers completed",
        pass,
        fail
    );

    scheduler_test_expect_truthy_success(
        interp,
        "(block "
        "  (spawn (lambda () 7)) "
        "  (run-fibers) "
        "  true)",
        "[PASS] scheduler recovers after run-loop failure",
        "[FAIL] scheduler recovers after run-loop failure",
        pass,
        fail
    );
}
fn void run_scheduler_tests(Interp* interp, int* pass, int* fail) {
    emit_test_section_banner("Scheduler Tests");
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
    run_scheduler_pending_close_rollback_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_completion_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_null_completion_fail_closed_boundary_tests(interp, pass, fail);
    run_scheduler_os_thread_null_completion_fail_closed_boundary_tests(interp, pass, fail);
    run_scheduler_waiter_take_does_not_strand_blocked_fiber_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_cancel_boundary_tests(interp, pass, fail);
    run_scheduler_cancel_conversion_payload_boundary_tests(interp, pass, fail);
    run_scheduler_join_wait_failure_mapping_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_worker_cancel_interleave_boundary_tests(interp, pass, fail);
    run_scheduler_thread_task_nonexecuted_payload_release_boundary_tests(interp, pass, fail);
    run_scheduler_worker_prestart_cancel_single_shared_release_boundary_tests(interp, pass, fail);
    run_scheduler_dns_connect_request_token_guard_boundary_tests(interp, pass, fail);
    run_scheduler_task_join_timeout_then_join_boundary_tests(interp, pass, fail);
    run_scheduler_custom_offload_task_join_boundary_tests(interp, pass, fail);
    run_scheduler_shared_retire_queue_boundary_tests(interp, pass, fail);
    run_scheduler_wakeup_ready_barrier_boundary_tests(interp, pass, fail);
    run_scheduler_mixed_boundary_state_restore_tests(interp, pass, fail);
    run_scheduler_wakeup_offload_error_boundary_tests(interp, pass, fail);
}

fn void run_scheduler_tests_isolated(int* pass, int* fail) {
    run_group_isolated(&run_scheduler_tests, pass, fail);
}
