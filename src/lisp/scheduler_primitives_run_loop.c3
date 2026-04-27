module lisp;

import main;

// ============================================================
// Scheduler core — round-robin resume loop
// ============================================================

fn bool scheduler_resume_ready_round(Interp* interp, usz target, bool stop_on_target, bool* target_done) {
    bool ran_ready = false;
    usz fc = g_scheduler.fiber_count;
    usz divisor = fc > 0 ? fc : (usz)1;
    usz off = g_scheduler.resume_offset % divisor;

    for (usz j = 0; j < fc; j++) {
        usz i = (off + j) % fc;
        if (g_scheduler.fibers[i].state != FIBER_READY) continue;
        ran_ready = true;
        scheduler_resume_fiber(i, interp);
        if (stop_on_target && g_scheduler.fibers[target].state == FIBER_DONE) {
            if (target_done != null) *target_done = true;
            break;
        }
    }
    g_scheduler.resume_offset = off + 1;
    return ran_ready;
}

fn bool scheduler_wait_blocked_once() {
    if (!scheduler_has_blocked_fibers()) return false;
    if (!scheduler_wait_for_io_once()) {
        if (reliable_wakeup_pending_count() == 0) return false;
        drain_wakeups();
        return true;
    }
    drain_wakeups();
    return true;
}

fn bool scheduler_advance_round(
    Interp* interp,
    usz target,
    bool stop_on_target,
    bool* target_done_out = null
) {
    dispatch_pending_signal_callbacks(interp);
    drain_wakeups();

    bool target_done = false;
    bool ran_ready = scheduler_resume_ready_round(interp, target, stop_on_target, &target_done);
    if (target_done_out != null) {
        *target_done_out = target_done;
    }
    if (target_done) return true;
    if (ran_ready) return true;
    if (scheduler_wait_blocked_once()) return true;
    dispatch_pending_signal_callbacks(interp);
    return false;
}

fn void scheduler_abort_all_work() {
    scheduler_close_pending_io_for_all_fibers();
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        scheduler_release_fiber_coroutine(i, null);
        g_scheduler.fibers[i] = {};
    }

    if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
        (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
    }

    scheduler_reset_wakeup_queue();
    g_scheduler.fiber_count = 0;
    g_scheduler.current_fiber_id = NO_FIBER;
    g_scheduler.resume_offset = 0;
}

fn Value* scheduler_fail_run_loop(char[] code, char[] message, Interp* interp) {
    scheduler_abort_all_work();
    return scheduler_raise(interp, code, message);
}

fn Value* scheduler_run_until(usz target, Interp* interp) {
    if (g_scheduler.running) {
        return scheduler_raise(
            interp,
            "scheduler/reentrant-run-loop",
            "await: scheduler run loop is already active"
        );
    }
    g_scheduler.running = true;
    defer {
        g_scheduler.running = false;
        g_scheduler.current_fiber_id = NO_FIBER;
    }

    usz max_rounds = scheduler_max_rounds();
    usz round = 0;

    while (g_scheduler.fibers[target].state != FIBER_DONE && round < max_rounds) {
        bool target_done = false;
        bool progressed = scheduler_advance_round(interp, target, true, &target_done);

        if (target_done || g_scheduler.fibers[target].state == FIBER_DONE) {
            if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
                (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
            }
            drain_wakeups();
            return null;
        }
        if (!progressed) {
            return scheduler_fail_run_loop(
                "scheduler/await-stalled",
                "await: scheduler made no progress before target completed",
                interp
            );
        }
        round++;
    }

    if (g_scheduler.fibers[target].state != FIBER_DONE) {
        return scheduler_fail_run_loop(
            "scheduler/await-round-limit-exceeded",
            "await: scheduler exceeded max rounds before target completed",
            interp
        );
    }

    if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
        (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
    }
    drain_wakeups();
    return null;
}

fn void scheduler_reset_if_idle() {
    if (!scheduler_all_done()) return;

    if (g_scheduler.uv_loop_ready && g_scheduler.uv_loop != null) {
        (void)uv_run(g_scheduler.uv_loop, UV_RUN_NOWAIT);
    }

    scheduler_close_pending_io_for_all_fibers();

    for (usz i = 0; i < MAX_FIBERS; i++) {
        if (i < g_scheduler.fiber_count) {
            scheduler_release_fiber_coroutine(i, null);
        }
        g_scheduler.fibers[i] = {};
        g_scheduler.pending_reads[i] = {};
        g_scheduler.pending_offloads[i] = {};
        g_scheduler.pending_sleeps[i] = {};
        g_scheduler.pending_dns[i] = {};
        g_scheduler.pending_connects[i] = {};
        g_scheduler.pending_accepts[i] = {};
    }
    g_scheduler.fiber_count = 0;
    g_scheduler.current_fiber_id = NO_FIBER;
    scheduler_reset_wakeup_queue();
}

fn Value* scheduler_run_all(Interp* interp) {
    if (g_scheduler.running) {
        return scheduler_raise(
            interp,
            "scheduler/reentrant-run-loop",
            "run-fibers: scheduler run loop is already active"
        );
    }
    g_scheduler.running = true;
    defer {
        g_scheduler.running = false;
        g_scheduler.current_fiber_id = NO_FIBER;
    }

    // max_rounds guards against infinite-yield loops (fibers that never block
    // or complete).
    usz max_rounds = scheduler_max_rounds();
    usz round = 0;

    while (round < max_rounds) {
        bool progressed = scheduler_advance_round(interp, 0, false);

        if (scheduler_all_done()) {
            drain_wakeups();
            scheduler_reset_if_idle();
            return null;
        }
        if (!progressed) {
            return scheduler_fail_run_loop(
                "scheduler/run-fibers-stalled",
                "run-fibers: scheduler made no progress and not all fibers completed",
                interp
            );
        }
        round++;
    }

    if (!scheduler_all_done()) {
        return scheduler_fail_run_loop(
            "scheduler/run-fibers-round-limit-exceeded",
            "run-fibers: scheduler exceeded max rounds before all fibers completed",
            interp
        );
    }

    drain_wakeups();
    scheduler_reset_if_idle();
    return null;
}
