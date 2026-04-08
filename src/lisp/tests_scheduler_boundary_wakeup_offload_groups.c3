module lisp;

import std::io;

fn void run_scheduler_wakeup_wraparound_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: reliable wakeup flooding drains fully and must not mutate
    // interpreter boundary/runtime fields.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;

    for (usz i = 0; i < 12; i++) {
        void* saved_async = null;
        scheduler_test_prepare_no_async(&saved_async);
        usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();

        usz pushed = 0;
        for (usz j = 0; j < 1024; j++) {
            usz fid = MAX_FIBERS + i * 1024 + j + 1;
            if (scheduler_publish_poll_error(fid, -1)) pushed++;
        }
        drain_wakeups();
        usz pending = reliable_wakeup_pending_count();
        usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();
        usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
        scheduler_test_cleanup_no_async(saved_async);

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool queue_ok = pushed == 1024 &&
                        pending == 0 &&
                        (enqueues_after - enqueues_before) == 1024 &&
                        (failures_after - failures_before) == 0;
        if (!state_ok || !queue_ok) {
            ok = false;
            failed_step = i;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup wraparound boundary restore");
        (*pass)++;
    } else {
        io::printfn("[FAIL] scheduler wakeup wraparound boundary restore at step %d", (int)failed_step);
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_mixed_event_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: mixed reliable event ordering for one blocked fiber must keep
    // first-event-wins completion behavior and preserve interpreter boundary/runtime state.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);

    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 12; i++) {
        void* saved_async = null;
        scheduler_test_prepare_blocked_pending_read(&saved_async);

        bool poll_first = (i & 1) == 0;
        bool pushed_a = false;
        bool pushed_b = false;
        if (poll_first) {
            pushed_a = scheduler_publish_poll_error(0, -17);
            pushed_b = scheduler_publish_timer_expired(0);
        } else {
            pushed_a = scheduler_publish_timer_expired(0);
            pushed_b = scheduler_publish_poll_error(0, -17);
        }

        drain_wakeups();

        PendingTcpRead op = g_scheduler.pending_reads[0];
        FiberState state = g_scheduler.fibers[0].state;
        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);

        bool core_ok = pushed_a &&
                       pushed_b &&
                       reliable_wakeup_pending_count() == 0 &&
                       op.completed &&
                       state == FIBER_READY;
        bool order_ok = poll_first
                        ? (!op.timed_out && op.error_code == -17)
                        : (op.timed_out && op.error_code == 0);
        if (!core_ok || !order_ok || !state_ok) {
            ok = false;
            failed_step = i;
            if (!core_ok) {
                failed_phase = "core";
            } else if (!order_ok) {
                failed_phase = "order";
            } else {
                failed_phase = "boundary";
            }
        }

        scheduler_test_cleanup_single_fiber_no_async(saved_async);

        if (!ok) break;
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler wakeup mixed-event boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler wakeup mixed-event boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}

fn void run_scheduler_wakeup_drop_counter_boundary_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: reliable wakeup observability increments deterministically
    // and does not mutate interpreter boundary/runtime fields.
    SchedulerBoundarySnapshot saved = scheduler_capture_runtime_boundary_snapshot(interp);
    bool ok = true;
    usz failed_step = 0;
    char[] failed_phase = "";

    for (usz i = 0; i < 12; i++) {
        void* saved_async = null;
        scheduler_test_prepare_no_async(&saved_async);
        usz failures_before = g_scheduler.wakeup_enqueue_failures.load();
        usz enqueues_before = g_scheduler.reliable_wakeup_enqueues.load();

        usz pushed = 0;
        for (usz j = 0; j < 3; j++) {
            if (scheduler_publish_poll_error(MAX_FIBERS + i * 10 + j + 1, -(int)(j + 1))) pushed++;
        }

        drain_wakeups();
        usz failures_after = g_scheduler.wakeup_enqueue_failures.load();
        usz enqueues_after = g_scheduler.reliable_wakeup_enqueues.load();

        bool state_ok = scheduler_runtime_boundary_matches_snapshot(interp, &saved);
        bool drop_ok = pushed == 3 &&
                       (failures_after - failures_before) == 0 &&
                       (enqueues_after - enqueues_before) == 3;
        bool queue_ok = reliable_wakeup_pending_count() == 0;
        scheduler_test_cleanup_no_async(saved_async);

        if (!drop_ok || !queue_ok || !state_ok) {
            ok = false;
            failed_step = i;
            if (!drop_ok) {
                failed_phase = "observability";
            } else if (!queue_ok) {
                failed_phase = "queue";
            } else {
                failed_phase = "boundary";
            }
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] scheduler reliable wakeup observability boundary restore");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] scheduler reliable wakeup observability boundary restore at step %d (%s)",
            (int)failed_step,
            (ZString)failed_phase
        );
        (*fail)++;
    }
}
