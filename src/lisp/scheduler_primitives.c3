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
    if (!scheduler_fiber_belongs_to_interp(fiber_id, interp)) return false;
    FiberEntry* f = &g_scheduler.fibers[fiber_id];

    // Can only cancel READY or BLOCKED fibers
    if (!scheduler_fiber_is_cancellable(f)) return false;
    f.cancel_requested = true;

    // Cancel all children first (recursive)
    scheduler_cancel_fiber_children(fiber_id, interp);

    // Child cancellation can complete this fiber (for example when the final
    // awaited child transitions the parent from waiting_children -> DONE).
    if (f.state == FIBER_DONE) return true;
    // Re-check cancellability to avoid state races before completion.
    if (!scheduler_fiber_is_cancellable(f)) {
        f.cancel_requested = false;
        return false;
    }

    // Complete with cancellation error
    Value* cancel_err = scheduler_raise(interp, "scheduler/fiber-cancelled", "fiber cancelled");
    scheduler_complete_fiber(fiber_id, cancel_err, interp);
    return true;
}

<*
@require interp != null
*>
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
    if (!scheduler_fiber_belongs_to_interp(fiber_id, interp)) {
        return scheduler_raise(
            interp,
            "scheduler/fiber-owner-mismatch",
            "fiber-cancel: fiber belongs to a different interpreter"
        );
    }

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

<*
@require interp != null
*>
fn Value* prim_spawn(Value*[] args, Env* env, Interp* interp) {
    scheduler_reset_if_idle();

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

    bool promotion_failed = false;
    bool parent_invalid = false;
    usz id = scheduler_add_fiber(co, parent_id, interp, &promotion_failed, &parent_invalid);
    if (id >= MAX_FIBERS) {
        if (co != null && co.tag == COROUTINE && co.coroutine_val != null) {
            coroutine_ctx_cleanup(co, co.coroutine_val, interp);
        }
        if (parent_invalid) {
            return scheduler_raise(interp, "scheduler/spawn-invalid-parent", "spawn: invalid scheduler parent");
        }
        if (promotion_failed) {
            return scheduler_raise(interp, "scheduler/spawn-scheduler-promotion-failed", "spawn: failed to promote coroutine into root scope");
        }
        return scheduler_raise(interp, "scheduler/spawn-scheduler-full", "spawn: scheduler full");
    }

    if (parent_id != NO_FIBER && scheduler_parent_accepts_child(parent_id, interp)) {
        g_scheduler.fibers[parent_id].live_children++;
    }

    return make_int(interp, (long)id);
}

fn Value* scheduler_result_or_nil(usz fiber_id, Interp* interp) @inline {
    Value* result = g_scheduler.fibers[fiber_id].result;
    return result != null ? result : make_nil(interp);
}

fn Value* scheduler_await_in_fiber_context(usz target, Interp* interp) {
    Value* ctx_err = scheduler_require_fiber_root_context("await", interp);
    if (ctx_err != null) return ctx_err;
    if (!scheduler_fiber_belongs_to_interp(target, interp)) {
        return scheduler_raise(
            interp,
            "scheduler/fiber-owner-mismatch",
            "await: fiber belongs to a different interpreter"
        );
    }

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
    if (y != null && y.tag == ERROR) {
        waiter.awaiting_fiber = false;
        waiter.await_target = NO_FIBER;
        scheduler_restore_blocked_fiber_ready(waiter_id);
        return y;
    }

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

<*
@require interp != null
*>
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
    if (!scheduler_fiber_belongs_to_interp(target, interp)) {
        return scheduler_raise(
            interp,
            "scheduler/fiber-owner-mismatch",
            "await: fiber belongs to a different interpreter"
        );
    }

    if (scheduler_is_running_fiber_context()) {
        return scheduler_await_in_fiber_context(target, interp);
    }

    if (g_scheduler.fibers[target].state == FIBER_DONE) {
        Value* out = scheduler_result_or_nil(target, interp);
        scheduler_reset_if_idle();
        return out;
    }

    Value* run_err = scheduler_run_until(target, interp);
    if (run_err != null) return run_err;
    Value* out = scheduler_result_or_nil(target, interp);
    scheduler_reset_if_idle();
    return out;
}

// ============================================================
// (run-fibers) → Void
//
// Runs all spawned fibers to completion. Round-robin scheduling.
// ============================================================

<*
@require interp != null
*>
fn Value* prim_run_fibers(Value*[] args, Env* env, Interp* interp) {
    if (g_scheduler.running) {
        return scheduler_raise(
            interp,
            "scheduler/reentrant-run-loop",
            "run-fibers: scheduler run loop is already active"
        );
    }
    Value* run_err = scheduler_run_all(interp);
    if (run_err != null) return run_err;
    return make_void(interp);
}
