module lisp;
import main;
import std::core::mem;

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

    SchedulerTaskHandle admitted = {};
    Value* admit_err = scheduler_admit_offload_task_work(
        work,
        false,
        true,
        true,
        "scheduler/offload-task-handle-invalid",
        "offload: failed to create task generation",
        &admitted,
        interp
    );
    if (admit_err != null) return admit_err;

    return scheduler_task_join_impl(admitted.task_id, admitted.generation, -1, null, interp);
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

    Value* join_err = scheduler_join_offload_task_handles(
        &handles[0],
        job_count,
        &results[0],
        interp
    );
    if (join_err != null) return join_err;

    Value* results_rev = make_nil(interp);
    for (usz i = job_count; i > 0; i--) {
        results_rev = make_cons(interp, results[i - 1], results_rev);
    }
    return results_rev;
}
