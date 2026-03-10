module lisp;

import std::core::mem;
import std::io;

fn void scheduler_test_pump_wakeups() @inline {
    if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
        (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
    }
    drain_wakeups();
}

fn bool scheduler_test_enqueue_offload_with_retry(OffloadWork work, usz max_spins = 512) {
    for (usz spin = 0; spin < max_spins; spin++) {
        if (scheduler_enqueue_offload(work)) return true;
        if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
            (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
        }
        (void)scheduler_wait_thread_task_waiters(1);
    }
    return false;
}

fn void run_scheduler_offload_worker_retry_full_wakeup_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: offload worker completion remains lossless while the reliable
    // wakeup queue already has heavy backlog, then succeeds after drain without
    // mutating interpreter boundary/runtime fields.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 8; i++) {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_offload(&saved_async);

        usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();
        usz pushed_fill = 0;
        for (usz j = 0; j < 512; j++) {
            if (scheduler_publish_poll_error(MAX_FIBERS + i * 512 + j + 1, -1)) {
                pushed_fill++;
            }
        }
        if (pushed_fill != 512) {
            ok = false;
            failed_step = i;
            failed_phase = "prefill";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }

        OffloadWork work = {};
        work.fiber_id = 0;
        work.task_id = NO_TASK;
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = 0;
        work.shared = null;
        bool queued = scheduler_enqueue_offload(work);
        if (!queued) {
            ok = false;
            failed_step = i;
            failed_phase = "enqueue-offload";
            scheduler_test_cleanup_single_fiber_no_async(saved_async);
            break;
        }

        // Release the backlog and wait until worker completion is delivered to
        // the active pending-offload slot, then consume it to avoid leaks.
        scheduler_test_pump_wakeups();
        bool completion_ready = false;
        for (usz spin = 0; spin < 160; spin++) {
            c_usleep(1000);
            scheduler_test_pump_wakeups();
            if (g_scheduler.pending_offloads[0].active &&
                g_scheduler.pending_offloads[0].completed &&
                g_scheduler.pending_offloads[0].completion != null &&
                g_scheduler.fibers[0].state == FIBER_READY) {
                completion_ready = true;
                break;
            }
        }

        bool consume_ok = false;
        if (completion_ready) {
            Value* consumed = scheduler_consume_pending_offload(0, interp);
            consume_ok = consumed != null && consumed.tag != ERROR;
        }

        // Defensive final drain in case a producer raced after consume.
        scheduler_test_pump_wakeups();
        c_usleep(1000);
        scheduler_test_pump_wakeups();

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool queue_empty = reliable_wakeup_pending_count() == 0;
        bool slot_cleared = !g_scheduler.pending_offloads[0].active &&
                            !g_scheduler.pending_offloads[0].completed &&
                            g_scheduler.pending_offloads[0].completion == null;
        usz failure_delta = g_scheduler.wakeup_enqueue_failures.load() - failures_before;
        usz enqueue_delta = g_scheduler.reliable_wakeup_enqueues.load() - enqueues_before;
        // Global scheduler state is shared across isolated test groups, so
        // unrelated in-flight async completions may legitimately add extra
        // reliable-wakeup enqueues beyond this cycle's expected +1 offload
        // completion signal.
        bool enqueue_ok = failure_delta == 0 && enqueue_delta >= pushed_fill + 1;
        bool cycle_ok = completion_ready && consume_ok && queue_empty && slot_cleared && enqueue_ok && state_ok;
        scheduler_test_cleanup_single_fiber_no_async(saved_async);
        if (!cycle_ok) {
            ok = false;
            failed_step = i;
            if (!enqueue_ok) {
                failed_phase = "enqueue";
            } else if (!completion_ready || !consume_ok || !slot_cleared) {
                failed_phase = "completion";
            } else if (!queue_empty) {
                failed_phase = "queue";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler offload worker reliable queue backlog boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler offload worker reliable queue backlog boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_offload_ready_barrier_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: direct reliable offload delivery plus unrelated invalid events
    // still preserve boundary/runtime state and consume cleanly.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    void* saved_async = null;
    scheduler_test_prepare_blocked_pending_offload(&saved_async);

    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) {
        scheduler_test_cleanup_single_fiber_no_async(saved_async);
        io::printn("[FAIL] scheduler wakeup offload-ready barrier boundary restore (alloc)");
        (*fail)++;
        return;
    }
    *completion = {};
    completion.kind = OFFLOAD_RES_NIL;

    bool pushed_invalid = scheduler_publish_poll_error(MAX_FIBERS + 222, -13);
    bool pushed_ready = scheduler_publish_offload_ready(0, completion);
    drain_wakeups();
    bool delivered_ok = pushed_invalid &&
                        pushed_ready &&
                        reliable_wakeup_pending_count() == 0 &&
                        g_scheduler.pending_offloads[0].active &&
                        g_scheduler.pending_offloads[0].completed &&
                        g_scheduler.pending_offloads[0].completion == completion &&
                        g_scheduler.fibers[0].state == FIBER_READY;
    bool state_ok_second = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

    Value* consumed = scheduler_consume_pending_offload(0, interp);
    bool consume_ok = consumed != null &&
                      consumed.tag == NIL &&
                      !g_scheduler.pending_offloads[0].active &&
                      !g_scheduler.pending_offloads[0].completed &&
                      g_scheduler.pending_offloads[0].completion == null;
    bool state_ok_third = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

    bool ok = delivered_ok && consume_ok && state_ok_second && state_ok_third;

    scheduler_test_cleanup_single_fiber_no_async(saved_async);

    if (ok) {
        emit_pass_literal("[PASS] scheduler reliable offload-ready boundary restore");
        (*pass)++;
    } else {
        io::printn("[FAIL] scheduler reliable offload-ready boundary restore");
        (*fail)++;
    }
}

fn void run_scheduler_thread_task_worker_cancel_interleave_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: real worker enqueue + immediate task cancel converges to one
    // cancel completion and preserves interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 16; i++) {
        if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
            (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
        }

        usz task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            break;
        }

        OffloadWork work = {};
        work.fiber_id = NO_FIBER;
        work.task_id = task_id;
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = 10;
        work.shared = null;
        bool enqueued = scheduler_test_enqueue_offload_with_retry(work);
        if (!enqueued) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "enqueue";
            break;
        }

        bool already_done = false;
        bool cancel_ok = scheduler_cancel_thread_task(task_id, &already_done);
        bool cancel_state_ok = cancel_ok && !already_done;
        if (!cancel_state_ok) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "cancel";
            break;
        }

        OffloadCompletion* completion = null;
        for (usz spin = 0; spin < 240; spin++) {
            if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
                (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
            }
            completion = scheduler_take_thread_task_completion(task_id);
            if (completion != null) break;
            (void)scheduler_wait_thread_task_waiters(1);
        }

        bool completion_ok = completion != null &&
                             completion.kind == OFFLOAD_RES_ERROR &&
                             !scheduler_thread_task_exists(task_id);
        if (completion != null) {
            scheduler_free_offload_completion(completion);
        } else {
            scheduler_drop_thread_task(task_id);
        }

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool cycle_ok = completion_ok && state_ok;
        if (!cycle_ok) {
            ok = false;
            failed_step = i;
            if (!completion_ok) {
                failed_phase = "completion";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler worker cancel interleave boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler worker cancel interleave boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_task_join_timeout_then_join_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: timeout join does not consume task completion; a later join can
    // complete successfully and clear task entry, with boundary/runtime state preserved.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 12; i++) {
        if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
            (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
        }

        usz task_id = scheduler_alloc_thread_task();
        if (task_id == NO_TASK) {
            ok = false;
            failed_step = i;
            failed_phase = "alloc";
            break;
        }
        uint generation = 0;
        if (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "generation";
            break;
        }

        OffloadWork work = {};
        work.fiber_id = NO_FIBER;
        work.task_id = task_id;
        work.op = OFFLOAD_SLEEP_MS;
        work.int_arg = 75;
        work.shared = null;
        bool enqueued = scheduler_test_enqueue_offload_with_retry(work);
        if (!enqueued) {
            scheduler_drop_thread_task(task_id);
            ok = false;
            failed_step = i;
            failed_phase = "enqueue";
            break;
        }

        Value* timeout_v = scheduler_task_join_impl(task_id, generation, 0, null, interp);
        bool timeout_ok = timeout_v != null && timeout_v.tag == ERROR;
        bool exists_after_timeout = scheduler_thread_task_exists(task_id, generation);

        Value* join_v = scheduler_task_join_impl(task_id, generation, 250, null, interp);
        bool join_ok = join_v != null && join_v.tag != ERROR;
        bool gone_after_join = !scheduler_thread_task_exists(task_id, generation);

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool cycle_ok = timeout_ok &&
                        exists_after_timeout &&
                        join_ok &&
                        gone_after_join &&
                        state_ok;
        if (!cycle_ok) {
            scheduler_drop_thread_task(task_id, generation);
            ok = false;
            failed_step = i;
            if (!timeout_ok || !exists_after_timeout) {
                failed_phase = "timeout";
            } else if (!join_ok || !gone_after_join) {
                failed_phase = "join";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler timeout-then-join boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler timeout-then-join boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_shared_retire_queue_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: shared-handle retire queue enqueues/releases deterministically,
    // overflow path falls back to immediate free, and domain safe points drain retired payloads.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    char[] failed_phase = "";

    if (!scheduler_shared_registry_ensure_ready()) {
        ok = false;
        failed_phase = "registry-init";
    }

    SharedRegistry* registry = &g_scheduler.shared_registry;
    usz overflow_before = 0;
    if (ok) {
        scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);
        overflow_before = registry.retire_overflow_count;
    }

    if (ok) {
        SharedHandle* first = scheduler_publish_shared("phase7");
        if (first == null) {
            ok = false;
            failed_phase = "publish-initial";
        } else {
            scheduler_release_shared(first);
            if (registry.retire_count == 0) {
                ok = false;
                failed_phase = "enqueue-initial";
            }
        }
    }

    if (ok) {
        scheduler_domain_safe_point(scheduler_primary_domain());
        if (registry.retire_count != 0) {
            ok = false;
            failed_phase = "drain-initial";
        }
    }

    if (ok) {
        usz stress_total = SHARED_RETIRED_QUEUE_CAPACITY + 8;
        for (usz i = 0; i < stress_total; i++) {
            SharedHandle* h = scheduler_publish_shared("q");
            if (h == null) {
                ok = false;
                failed_phase = "publish-stress";
                break;
            }
            scheduler_release_shared(h);
        }
    }

    if (ok) {
        bool queue_full = registry.retire_count == SHARED_RETIRED_QUEUE_CAPACITY;
        bool overflow_recorded = registry.retire_overflow_count >= overflow_before + 8;
        if (!queue_full || !overflow_recorded) {
            ok = false;
            failed_phase = "overflow-accounting";
        }
    }

    if (ok) {
        scheduler_shared_registry_drain_retired(SHARED_RETIRED_QUEUE_CAPACITY);
        if (registry.retire_count != 0) {
            ok = false;
            failed_phase = "drain-final";
        }
    }

    bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
    if (!state_ok && ok) {
        ok = false;
        failed_phase = "boundary";
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler shared retire queue boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler shared retire queue boundary restore (%s)",
            (ZString)failed_phase
        );
        (*fail)++;
    }
}
