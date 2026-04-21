module lisp;

fn Value* scheduler_execute_offload_work(OffloadWork* work, Interp* interp) {
    if (work == null) {
        return scheduler_raise(interp, "scheduler/offload-expected-job-list", "offload: expected job list");
    }

    bool in_fiber = scheduler_is_running_fiber_context();
    if (!in_fiber) return scheduler_run_offload_via_queue_work(work, interp);
    Value* ctx_err = scheduler_require_fiber_root_context("offload", interp);
    if (ctx_err != null) {
        scheduler_release_shared(work.shared);
        scheduler_release_custom_ctx(work);
        return ctx_err;
    }

    usz fiber_id = g_scheduler.current_fiber_id;
    Value* start_err = scheduler_begin_fiber_offload(work, fiber_id, interp);
    if (start_err != null) return start_err;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) {
        scheduler_close_pending_offload(fiber_id);
        return y;
    }

    return scheduler_consume_pending_offload(fiber_id, interp);
}

fn Value* scheduler_run_internal_io_offload_job(Value* job, Interp* interp) {
    return scheduler_execute_offload_job(job, true, interp);
}

fn Value* scheduler_begin_fiber_offload(OffloadWork* work, usz fiber_id, Interp* interp) {
    PendingOffload* pending = &g_scheduler.pending_offloads[fiber_id];
    if (pending.active) {
        scheduler_release_shared(work.shared);
        scheduler_release_custom_ctx(work);
        return scheduler_raise(
            interp,
            "scheduler/offload-pending-work-exists",
            "offload: pending work already exists for this fiber"
        );
    }

    work.fiber_id = fiber_id;
    work.task_id = NO_TASK;
    work.wake_epoch = g_scheduler.fibers[fiber_id].wake_epoch;
    FiberState saved_state = g_scheduler.fibers[fiber_id].state;
    pending.active = true;
    pending.completed = false;
    pending.wake_epoch = work.wake_epoch;
    pending.completion = null;
    g_scheduler.fibers[fiber_id].state = FIBER_BLOCKED;
    if (!scheduler_enqueue_offload(*work)) {
        *pending = {};
        g_scheduler.fibers[fiber_id].state = saved_state;
        scheduler_release_shared(work.shared);
        scheduler_release_custom_ctx(work);
        return scheduler_raise(interp, "scheduler/offload-admission-full", "offload: admission cap reached");
    }
    return null;
}

fn Value* prim_offload(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len < 1) {
        return scheduler_raise(interp, "scheduler/offload-expected-job-list", "offload: expected job list");
    }
    return scheduler_execute_offload_job(args[0], false, interp);
}

fn Value* prim_offload_batch(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len < 1) {
        return scheduler_raise(interp, "scheduler/offload-expected-job-list", "offload: expected job list");
    }
    return scheduler_execute_offload_job_batch(args[0], false, interp);
}
