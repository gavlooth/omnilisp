module lisp;
import main;

fn usz scheduler_max_rounds() @inline {
    // ASAN runs are substantially slower; keep the infinite-yield guard
    // deterministic without burning minutes in scheduler round loops.
    if (main::stack_runtime_asan_enabled()) return 20000;
    return 100000;
}

fn Value* scheduler_run_offload_via_queue_work(OffloadWork* work, Interp* interp) {
    if (!g_scheduler.thread_task_mu_ready) {
        scheduler_release_shared(work.shared);
        return scheduler_raise(
            interp,
            "scheduler/offload-task-mutex-unavailable",
            "offload: task mutex unavailable"
        );
    }
    if (!g_scheduler.thread_task_cv_ready) {
        scheduler_release_shared(work.shared);
        return scheduler_raise(
            interp,
            "scheduler/offload-task-condition-unavailable",
            "offload: task condition variable unavailable"
        );
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        scheduler_release_shared(work.shared);
        return scheduler_raise(
            interp,
            "scheduler/offload-backend-unavailable",
            "offload: uv backend unavailable"
        );
    }

    usz task_id = scheduler_alloc_thread_task();
    if (task_id == NO_TASK) {
        scheduler_release_shared(work.shared);
        return scheduler_raise(
            interp,
            "scheduler/offload-task-table-full",
            "offload: task table full"
        );
    }

    work.fiber_id = NO_FIBER;
    work.task_id = task_id;
    work.wake_epoch = 0;
    if (!scheduler_enqueue_offload(*work)) {
        scheduler_release_shared(work.shared);
        scheduler_drop_thread_task(task_id);
        return scheduler_raise(
            interp,
            "scheduler/offload-admission-full",
            "offload: admission cap reached"
        );
    }

    uint generation = 0;
    if (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0) {
        scheduler_drop_thread_task(task_id);
        return scheduler_raise(
            interp,
            "scheduler/offload-task-handle-invalid",
            "offload: failed to create task generation"
        );
    }

    return scheduler_task_join_impl(task_id, generation, -1, null, interp);
}

fn Value* scheduler_parse_offload_job(
    Value*[] args,
    char[] expected_code,
    char[] expected_msg,
    bool allow_internal_io_ops,
    OffloadWork* work,
    Interp* interp
) {
    if (args.len < 1) return scheduler_raise(interp, expected_code, expected_msg);
    *work = {};
    return scheduler_build_offload_work(args[0], work, interp, allow_internal_io_ops);
}

fn Value* scheduler_execute_offload_job(Value* job, bool allow_internal_io_ops, Interp* interp) {
    scheduler_init();

    Value*[1] wrapped;
    wrapped[0] = job;

    OffloadWork work;
    Value* parse_err = scheduler_parse_offload_job(
        wrapped[..],
        "scheduler/offload-expected-job-list",
        "offload: expected job list",
        allow_internal_io_ops,
        &work,
        interp
    );
    if (parse_err != null) return parse_err;

    bool in_fiber = scheduler_is_running_fiber_context();
    if (!in_fiber) return scheduler_run_offload_via_queue_work(&work, interp);
    Value* ctx_err = scheduler_require_fiber_root_context("offload", interp);
    if (ctx_err != null) return ctx_err;

    usz fiber_id = g_scheduler.current_fiber_id;
    Value* start_err = scheduler_begin_fiber_offload(&work, fiber_id, interp);
    if (start_err != null) return start_err;

    Value*[1] yargs;
    yargs[0] = make_nil(interp);
    Value* y = prim_yield(yargs[..], null, interp);
    if (y != null && y.tag == ERROR) return y;

    return scheduler_consume_pending_offload(fiber_id, interp);
}

fn Value* scheduler_run_internal_io_offload_job(Value* job, Interp* interp) {
    return scheduler_execute_offload_job(job, true, interp);
}

fn Value* scheduler_begin_fiber_offload(OffloadWork* work, usz fiber_id, Interp* interp) {
    PendingOffload* pending = &g_scheduler.pending_offloads[fiber_id];
    if (pending.active) {
        scheduler_release_shared(work.shared);
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
