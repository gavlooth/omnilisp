module lisp;
import main;

// ============================================================
// Scheduler public primitives and run loops
// ============================================================

// ============================================================
// (fiber-cancel fiber-id) → Void
//
// Cancels a BLOCKED or READY fiber. Cleans up pending I/O,
// marks fiber DONE with error result, notifies parent.
// If the target fiber is already RUNNING or DONE, this is a no-op
// command completion and still returns Void.
// ============================================================

fn bool scheduler_fiber_is_cancellable(FiberEntry* f) @inline {
    return f.state == FIBER_READY || f.state == FIBER_BLOCKED;
}

fn void scheduler_cancel_fiber_children(usz fiber_id, Interp* interp) {
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        if (g_scheduler.fibers[i].parent_id == fiber_id &&
            g_scheduler.fibers[i].state != FIBER_DONE) {
            scheduler_cancel_fiber(i, interp);
        }
    }
}

fn bool scheduler_cancel_fiber(usz fiber_id, Interp* interp) {
    if (fiber_id >= g_scheduler.fiber_count) return false;
    FiberEntry* f = &g_scheduler.fibers[fiber_id];

    // Can only cancel READY or BLOCKED fibers
    if (!scheduler_fiber_is_cancellable(f)) return false;

    // Cancel all children first (recursive)
    scheduler_cancel_fiber_children(fiber_id, interp);

    // Complete with cancellation error
    Value* cancel_err = scheduler_raise(interp, "scheduler/fiber-cancelled", "fiber cancelled");
    scheduler_mark_done(fiber_id, cancel_err, interp);
    return true;
}

fn Value* prim_fiber_cancel(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    usz fiber_id = 0;
    Value* parse_err = scheduler_parse_existing_fiber_arg(
        args,
        "scheduler/fiber-cancel-expected-fiber-id",
        "fiber-cancel: expected fiber-id integer",
        "scheduler/fiber-cancel-invalid-fiber-id",
        "fiber-cancel: invalid fiber id",
        &fiber_id,
        interp
    );
    if (parse_err != null) return parse_err;

    // Cannot cancel self
    if (scheduler_is_running_fiber_context() && g_scheduler.current_fiber_id == fiber_id) {
        return scheduler_raise(
            interp,
            "scheduler/fiber-cancel-self",
            "fiber-cancel: fiber cannot cancel itself"
        );
    }

    if (!scheduler_cancel_fiber(fiber_id, interp)) return make_void(interp);
    return make_void(interp);
}

// ============================================================
// (spawn thunk) → fiber-id (integer)
//
// Creates a coroutine from thunk, adds to scheduler.
// ============================================================

fn Value* prim_spawn(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return scheduler_raise(interp, "scheduler/spawn-expected-thunk", "spawn: expected (spawn thunk)");

    Value* thunk = args[0];
    if (thunk == null || thunk.tag != CLOSURE) {
        return scheduler_raise(interp, "scheduler/spawn-arg-not-closure", "spawn: argument must be a closure");
    }

    // Create coroutine from thunk
    Value*[1] co_args;
    co_args[0] = thunk;
    Value* co = prim_coroutine(co_args[..], env, interp);
    if (co == null || co.tag == ERROR) return co;

    usz parent_id = NO_FIBER;
    if (scheduler_is_running_fiber_context()) {
        parent_id = g_scheduler.current_fiber_id;
    }

    usz id = scheduler_add_fiber(co, parent_id, interp);
    if (id >= MAX_FIBERS) return scheduler_raise(interp, "scheduler/spawn-scheduler-full", "spawn: scheduler full");

    if (parent_id < g_scheduler.fiber_count) {
        g_scheduler.fibers[parent_id].live_children++;
    }

    return make_int(interp, (long)id);
}

fn Value* scheduler_result_or_nil(usz fiber_id, Interp* interp) @inline {
    Value* result = g_scheduler.fibers[fiber_id].result;
    return result != null ? result : make_nil(interp);
}

fn Value* scheduler_await_in_fiber_context(usz target, Interp* interp) {
    usz waiter_id = g_scheduler.current_fiber_id;
    FiberEntry* waiter = &g_scheduler.fibers[waiter_id];

    if (target == waiter_id) {
        return scheduler_raise(interp, "scheduler/await-self", "await: fiber cannot await itself");
    }
    if (g_scheduler.fibers[target].parent_id != waiter_id) {
        return scheduler_raise(
            interp,
            "scheduler/await-non-child",
            "await: fiber can only await direct children"
        );
    }
    if (waiter.waiting_children) {
        return scheduler_raise(
            interp,
            "scheduler/await-parent-already-waiting",
            "await: parent is already waiting for child completion"
        );
    }

    if (g_scheduler.fibers[target].state == FIBER_DONE) {
        return scheduler_result_or_nil(target, interp);
    }

    waiter.awaiting_fiber = true;
    waiter.await_target = target;
    waiter.state = FIBER_BLOCKED;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) return y;

    if (g_scheduler.fibers[target].state != FIBER_DONE) {
        return scheduler_raise(
            interp,
            "scheduler/await-resumed-before-child-completed",
            "await: resumed before child completed"
        );
    }

    return scheduler_result_or_nil(target, interp);
}

// ============================================================
// (await fiber-id) → result value
//
// Runs the scheduler until the specified fiber completes.
// Returns the fiber's final result.
// ============================================================

fn Value* prim_await(Value*[] args, Env* env, Interp* interp) {
    usz target = 0;
    Value* parse_err = scheduler_parse_existing_fiber_arg(
        args,
        "scheduler/await-expected-fiber-id",
        "await: expected (await fiber-id)",
        "scheduler/await-invalid-fiber-id",
        "await: invalid fiber id",
        &target,
        interp
    );
    if (parse_err != null) return parse_err;

    if (scheduler_is_running_fiber_context()) {
        return scheduler_await_in_fiber_context(target, interp);
    }

    if (g_scheduler.fibers[target].state == FIBER_DONE) {
        return scheduler_result_or_nil(target, interp);
    }

    Value* run_err = scheduler_run_until(target, interp);
    if (run_err != null) return run_err;
    return scheduler_result_or_nil(target, interp);
}

// ============================================================
// (run-fibers) → Void
//
// Runs all spawned fibers to completion. Round-robin scheduling.
// ============================================================

fn Value* prim_run_fibers(Value*[] args, Env* env, Interp* interp) {
    Value* run_err = scheduler_run_all(interp);
    if (run_err != null) return run_err;
    return make_void(interp);
}

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
    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        scheduler_close_pending_tcp_read(i);
        scheduler_close_pending_offload(i);
        scheduler_close_pending_sleep(i);
        scheduler_close_pending_dns(i);
        scheduler_close_pending_tcp_connect(i);
        scheduler_close_pending_tcp_accept(i);
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

    for (usz i = 0; i < g_scheduler.fiber_count; i++) {
        scheduler_close_pending_tcp_read(i);
        scheduler_close_pending_offload(i);
        scheduler_close_pending_sleep(i);
        scheduler_close_pending_dns(i);
        scheduler_close_pending_tcp_connect(i);
        scheduler_close_pending_tcp_accept(i);
    }

    g_scheduler.fiber_count = 0;
    g_scheduler.current_fiber_id = NO_FIBER;
    scheduler_reset_wakeup_queue();
    for (usz i = 0; i < MAX_FIBERS; i++) {
        g_scheduler.fibers[i] = {};
        g_scheduler.pending_reads[i] = {};
        g_scheduler.pending_offloads[i] = {};
        g_scheduler.pending_sleeps[i] = {};
        g_scheduler.pending_dns[i] = {};
        g_scheduler.pending_connects[i] = {};
        g_scheduler.pending_accepts[i] = {};
    }
}

fn Value* scheduler_run_all(Interp* interp) {
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
