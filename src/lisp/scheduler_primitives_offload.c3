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

    usz task_id = work.task_id;
    bool allocated_task_id = false;
    if (task_id == NO_TASK) {
        task_id = scheduler_alloc_thread_task();
        allocated_task_id = true;
    }
    if (task_id == NO_TASK) {
        scheduler_release_shared(work.shared);
        return scheduler_raise(
            interp,
            "scheduler/offload-task-table-full",
            "offload: task table full"
        );
    }
    if (allocated_task_id) {
        work.task_id = task_id;
    } else {
        uint generation_probe = 0;
        if (!scheduler_get_thread_task_generation(task_id, &generation_probe) || generation_probe == 0) {
            scheduler_release_shared(work.shared);
            return scheduler_raise(
                interp,
                "scheduler/offload-task-handle-invalid",
                "offload: failed to create task generation"
            );
        }
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

    return scheduler_execute_offload_work(&work, interp);
}

fn Value* scheduler_execute_offload_job_batch(Value* jobs, bool allow_internal_io_ops, Interp* interp) {
    scheduler_init();

    if (jobs == null || is_nil(jobs)) {
        return make_nil(interp);
    }

    usz job_count = 0;
    Value* cur = jobs;
    while (is_cons(cur)) {
        job_count++;
        cur = cdr(cur);
    }
    if (!is_nil(cur)) {
        return scheduler_raise(interp, "scheduler/offload-expected-job-list", "offload: expected job list");
    }
    if (job_count == 0) {
        return make_nil(interp);
    }

    OffloadWork[] works = mem::new_array(OffloadWork, job_count);
    if (works.ptr == null) {
        return scheduler_raise(interp, "scheduler/offload-out-of-memory", "offload: out of memory");
    }
    defer mem::free(works.ptr);

    cur = jobs;
    for (usz i = 0; i < job_count; i++) {
        works[i] = {};
        Value* parse_err = scheduler_build_offload_work(car(cur), &works[i], interp, allow_internal_io_ops);
        if (parse_err != null) return parse_err;
        cur = cdr(cur);
    }

    SchedulerTaskHandle[] handles = mem::new_array(SchedulerTaskHandle, job_count);
    if (handles.ptr == null) {
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }
    defer mem::free(handles.ptr);

    Value* begin_err = scheduler_begin_offload_work_jobs(works[..], &handles[0], interp);
    if (begin_err != null) return begin_err;

    Value*[] results = mem::new_array(Value*, job_count);
    if (results.ptr == null) {
        return scheduler_raise(interp, "scheduler/offload-out-of-memory", "offload: out of memory");
    }
    defer mem::free(results.ptr);

    for (usz i = 0; i < job_count; i++) {
        Value* join_v = scheduler_task_join_impl(
            handles[i].task_id,
            handles[i].generation,
            -1,
            &handles[i],
            interp
        );
        if (join_v != null && join_v.tag == ERROR) {
            for (usz j = i + 1; j < job_count; j++) {
                if (scheduler_thread_task_exists(handles[j].task_id, handles[j].generation)) {
                    scheduler_drop_thread_task(handles[j].task_id, handles[j].generation);
                }
            }
            return join_v;
        }
        if (join_v == null) join_v = make_nil(interp);
        results[i] = join_v;
    }

    Value* results_rev = make_nil(interp);
    for (usz i = job_count; i > 0; i--) {
        results_rev = make_cons(interp, results[i - 1], results_rev);
    }
    return results_rev;
}

fn Value* scheduler_execute_custom_offload_job(OffloadCustomFn custom_fn, void* custom_ctx, Interp* interp) {
    scheduler_init();

    OffloadWork work = {};
    work.custom_fn = custom_fn;
    work.custom_ctx = custom_ctx;
    work.task_id = NO_TASK;
    return scheduler_execute_offload_work(&work, interp);
}

fn Value* scheduler_begin_custom_offload_task_job(
    OffloadCustomFn custom_fn,
    void* custom_ctx,
    SchedulerTaskHandle* handle,
    Interp* interp
) {
    scheduler_init();

    if (custom_fn == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: invalid custom callback");
    }
    if (handle == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing task handle");
    }

    usz task_id = scheduler_alloc_thread_task();
    if (task_id == NO_TASK) {
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }

    uint generation = 0;
    if (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0) {
        scheduler_drop_thread_task(task_id);
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }

    OffloadWork work = {};
    work.custom_fn = custom_fn;
    work.custom_ctx = custom_ctx;
    work.task_id = task_id;
    if (!scheduler_enqueue_offload(work)) {
        scheduler_drop_thread_task(task_id, generation);
        return scheduler_raise(interp, "scheduler/offload-admission-full", "offload: admission cap reached");
    }

    handle.task_id = task_id;
    handle.generation = generation;
    handle.owner_interp = interp;
    return null;
}

fn Value* scheduler_begin_custom_offload_task_jobs(
    OffloadCustomFn custom_fn,
    void** custom_ctxs,
    usz count,
    SchedulerTaskHandle* handles,
    Interp* interp
) {
    scheduler_init();

    if (custom_fn == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: invalid custom callback");
    }
    if (count > 0 && handles == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing task handle");
    }
    if (count > 0 && custom_ctxs == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing custom context array");
    }

    for (usz i = 0; i < count; i++) {
        handles[i] = {};
    }

    for (usz i = 0; i < count; i++) {
        Value* err = scheduler_begin_custom_offload_task_job(
            custom_fn,
            custom_ctxs != null ? custom_ctxs[i] : null,
            &handles[i],
            interp
        );
        if (err != null) {
            for (usz j = 0; j < i; j++) {
                if (handles[j].task_id != NO_TASK) {
                    scheduler_drop_thread_task(handles[j].task_id, handles[j].generation);
                    handles[j] = {};
                }
            }
            return err;
        }
    }
    return null;
}

fn Value* scheduler_execute_custom_offload_task_jobs(
    OffloadCustomFn custom_fn,
    void** custom_ctxs,
    usz count,
    Interp* interp
) {
    scheduler_init();

    if (custom_fn == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: invalid custom callback");
    }
    if (count > 0 && custom_ctxs == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing custom context array");
    }
    if (count == 0) {
        return make_void(interp);
    }

    SchedulerTaskHandle[] handles = mem::new_array(SchedulerTaskHandle, count);
    if (handles.ptr == null) {
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }
    defer mem::free(handles.ptr);

    Value* begin_err = scheduler_begin_custom_offload_task_jobs(
        custom_fn,
        custom_ctxs,
        count,
        &handles[0],
        interp
    );
    if (begin_err != null) return begin_err;

    for (usz i = 0; i < count; i++) {
        Value* join_v = scheduler_task_join_impl(
            handles[i].task_id,
            handles[i].generation,
            -1,
            &handles[i],
            interp
        );
        if (join_v != null && join_v.tag == ERROR) {
            for (usz j = i + 1; j < count; j++) {
                if (scheduler_thread_task_exists(handles[j].task_id, handles[j].generation)) {
                    scheduler_drop_thread_task(handles[j].task_id, handles[j].generation);
                }
            }
            return join_v;
        }
    }
    return make_void(interp);
}

fn Value* scheduler_execute_custom_offload_task_job(
    OffloadCustomFn custom_fn,
    void* custom_ctx,
    usz task_id,
    Interp* interp
) {
    scheduler_init();

    OffloadWork work = {};
    work.custom_fn = custom_fn;
    work.custom_ctx = custom_ctx;
    work.task_id = task_id;
    return scheduler_execute_offload_work(&work, interp);
}

fn Value* scheduler_begin_offload_work_job(
    OffloadWork work,
    SchedulerTaskHandle* handle,
    Interp* interp
) {
    scheduler_init();

    if (handle == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing task handle");
    }
    if (work.task_id != NO_TASK) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: batch work must not carry a task id");
    }

    usz task_id = scheduler_alloc_thread_task();
    if (task_id == NO_TASK) {
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }

    uint generation = 0;
    if (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0) {
        scheduler_drop_thread_task(task_id);
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }

    work.task_id = task_id;
    if (!scheduler_enqueue_offload(work)) {
        scheduler_drop_thread_task(task_id, generation);
        return scheduler_raise(interp, "scheduler/offload-admission-full", "offload: admission cap reached");
    }

    handle.task_id = task_id;
    handle.generation = generation;
    handle.owner_interp = interp;
    return null;
}

fn Value* scheduler_begin_offload_work_jobs(
    OffloadWork[] works,
    SchedulerTaskHandle* handles,
    Interp* interp
) {
    scheduler_init();

    if (works.len > 0 && handles == null) {
        return scheduler_raise(interp, "scheduler/offload-invalid-job", "offload: missing task handle");
    }

    for (usz i = 0; i < works.len; i++) {
        handles[i] = {};
    }

    for (usz i = 0; i < works.len; i++) {
        Value* err = scheduler_begin_offload_work_job(works[i], &handles[i], interp);
        if (err != null) {
            for (usz j = 0; j < i; j++) {
                if (handles[j].task_id != NO_TASK) {
                    scheduler_drop_thread_task(handles[j].task_id, handles[j].generation);
                    handles[j] = {};
                }
            }
            return err;
        }
    }
    return null;
}

fn Value* scheduler_execute_offload_work_jobs(
    OffloadWork[] works,
    Interp* interp
) {
    scheduler_init();

    if (works.len == 0) {
        return make_void(interp);
    }

    SchedulerTaskHandle[] handles = mem::new_array(SchedulerTaskHandle, works.len);
    if (handles.ptr == null) {
        return scheduler_raise(interp, "scheduler/offload-task-table-full", "offload: task table full");
    }
    defer mem::free(handles.ptr);

    Value* begin_err = scheduler_begin_offload_work_jobs(works, &handles[0], interp);
    if (begin_err != null) return begin_err;

    for (usz i = 0; i < works.len; i++) {
        Value* join_v = scheduler_task_join_impl(
            handles[i].task_id,
            handles[i].generation,
            -1,
            &handles[i],
            interp
        );
        if (join_v != null && join_v.tag == ERROR) {
            for (usz j = i + 1; j < works.len; j++) {
                if (scheduler_thread_task_exists(handles[j].task_id, handles[j].generation)) {
                    scheduler_drop_thread_task(handles[j].task_id, handles[j].generation);
                }
            }
            return join_v;
        }
    }
    return make_void(interp);
}

fn Value* scheduler_execute_offload_work(OffloadWork* work, Interp* interp) {
    if (work == null) {
        return scheduler_raise(interp, "scheduler/offload-expected-job-list", "offload: expected job list");
    }

    bool in_fiber = scheduler_is_running_fiber_context();
    if (!in_fiber) return scheduler_run_offload_via_queue_work(work, interp);
    Value* ctx_err = scheduler_require_fiber_root_context("offload", interp);
    if (ctx_err != null) return ctx_err;

    usz fiber_id = g_scheduler.current_fiber_id;
    Value* start_err = scheduler_begin_fiber_offload(work, fiber_id, interp);
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

fn Value* prim_offload_batch(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len < 1) {
        return scheduler_raise(interp, "scheduler/offload-expected-job-list", "offload: expected job list");
    }
    return scheduler_execute_offload_job_batch(args[0], false, interp);
}
