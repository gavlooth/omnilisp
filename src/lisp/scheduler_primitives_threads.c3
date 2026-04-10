module lisp;

import std::core::mem;
import std::io;

int g_scheduler_force_result_list_cons_alloc_fail_nth = 0;

fn bool scheduler_should_force_result_list_cons_alloc_fail() @inline {
    if (g_scheduler_force_result_list_cons_alloc_fail_nth <= 0) return false;
    g_scheduler_force_result_list_cons_alloc_fail_nth--;
    return g_scheduler_force_result_list_cons_alloc_fail_nth == 0;
}

fn Value* scheduler_prepend_result_or_error(
    Value* item,
    Value* list,
    char[] code,
    char[] message,
    Interp* interp
) @inline {
    if (scheduler_should_force_result_list_cons_alloc_fail()) {
        return scheduler_raise(interp, code, message);
    }
    Value* next = make_cons(interp, item, list);
    if (next == null) {
        return scheduler_raise(interp, code, message);
    }
    if (next.tag == ERROR) return next;
    return next;
}

fn Value* prim_thread_spawn(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    if (!g_scheduler.thread_task_mu_ready) {
        return scheduler_raise(
            interp,
            "scheduler/thread-mutex-unavailable",
            "thread-spawn: thread mutex unavailable"
        );
    }

    OffloadWork work;
    Value* parse_err = scheduler_parse_offload_job(
        args,
        "scheduler/thread-spawn-expected-job-list",
        "thread-spawn: expected job list",
        false,
        &work,
        interp
    );
    if (parse_err != null) return parse_err;

    usz thread_id = NO_OS_THREAD;
    uint generation = 0;
    Value* spawn_err = scheduler_prepare_os_thread_spawn(&work, &thread_id, &generation, interp);
    if (spawn_err != null) return spawn_err;
    return scheduler_make_thread_handle_value(thread_id, generation, interp);
}

fn Value* prim_thread_spawn_batch(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    if (!g_scheduler.thread_task_mu_ready) {
        return scheduler_raise(
            interp,
            "scheduler/thread-mutex-unavailable",
            "thread-spawn-batch: thread mutex unavailable"
        );
    }
    if (args.len < 1) {
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-batch-expected-job-list",
            "thread-spawn-batch: expected job list"
        );
    }

    Value* jobs = args[0];
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
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-batch-expected-job-list",
            "thread-spawn-batch: expected job list"
        );
    }
    if (job_count == 0) {
        return make_nil(interp);
    }

    OffloadWork[] works = mem::new_array(OffloadWork, job_count);
    if (works.ptr == null) {
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-batch-out-of-memory",
            "thread-spawn-batch: out of memory"
        );
    }
    defer mem::free(works.ptr);

    cur = jobs;
    for (usz i = 0; i < job_count; i++) {
        works[i] = {};
        Value* parse_err = scheduler_build_offload_work(cur.cons_val.car, &works[i], interp, false);
        if (parse_err != null) return parse_err;
        cur = cdr(cur);
    }

    SchedulerOsThreadHandle[] handles = mem::new_array(SchedulerOsThreadHandle, job_count);
    if (handles.ptr == null) {
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-batch-out-of-memory",
            "thread-spawn-batch: out of memory"
        );
    }
    defer mem::free(handles.ptr);

    Value* begin_err = scheduler_begin_os_thread_work_jobs(works[..], &handles[0], interp);
    if (begin_err != null) return begin_err;

    Value*[] results = mem::new_array(Value*, job_count);
    if (results.ptr == null) {
        return scheduler_raise(interp, "scheduler/thread-spawn-batch-out-of-memory", "thread-spawn-batch: out of memory");
    }
    defer mem::free(results.ptr);

    for (usz i = 0; i < job_count; i++) {
        Value* join_v = scheduler_thread_join_impl(
            handles[i].thread_id,
            handles[i].generation,
            -1,
            &handles[i],
            interp
        );
        if (join_v != null && join_v.tag == ERROR) {
            for (usz j = i + 1; j < job_count; j++) {
                if (scheduler_os_thread_exists(handles[j].thread_id, handles[j].generation)) {
                    scheduler_drop_os_thread(handles[j].thread_id, handles[j].generation);
                }
            }
            return join_v;
        }
        if (join_v == null) join_v = make_nil(interp);
        results[i] = join_v;
    }

    Value* results_rev = make_nil(interp);
    for (usz i = job_count; i > 0; i--) {
        results_rev = scheduler_prepend_result_or_error(
            results[i - 1],
            results_rev,
            "scheduler/thread-spawn-batch-out-of-memory",
            "thread-spawn-batch: out of memory",
            interp
        );
        if (results_rev != null && results_rev.tag == ERROR) return results_rev;
    }
    return results_rev;
}

fn Value* prim_thread_join(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    Value* context_err = scheduler_validate_thread_join_context("thread-join", interp);
    if (context_err != null) return context_err;
    if (args.len < 1) {
        return scheduler_raise(
            interp,
            "scheduler/thread-join-expected-thread-handle",
            "thread-join: expected thread-handle"
        );
    }

    usz thread_id = NO_OS_THREAD;
    uint generation = 0;
    SchedulerOsThreadHandle* handle = null;
    Value* handle_err = scheduler_parse_thread_handle_arg(
        args[0],
        "scheduler/thread-join-expected-thread-handle",
        "thread-join: expected thread-handle",
        "scheduler/thread-join-invalid-thread-id",
        "thread-join: invalid thread handle",
        &thread_id,
        &generation,
        &handle,
        interp
    );
    if (handle_err != null) return handle_err;

    return scheduler_thread_join_impl(thread_id, generation, -1, handle, interp);
}

fn Value* prim_thread_join_timeout(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    Value* context_err = scheduler_validate_thread_join_context("thread-join-timeout", interp);
    if (context_err != null) return context_err;
    if (args.len < 1 || !is_cons(args[0])) {
        return scheduler_raise(
            interp,
            "scheduler/thread-join-timeout-expected-pair",
            "thread-join-timeout: expected (thread-handle . timeout-ms)"
        );
    }

    usz thread_id = NO_OS_THREAD;
    uint generation = 0;
    SchedulerOsThreadHandle* handle = null;
    long timeout_ms = 0;
    Value* parse_err = scheduler_parse_thread_join_timeout_pair(
        args[0],
        &thread_id,
        &generation,
        &handle,
        &timeout_ms,
        interp
    );
    if (parse_err != null) return parse_err;
    return scheduler_thread_join_impl(thread_id, generation, timeout_ms, handle, interp);
}

fn Value* prim_thread_cancel(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    if (!g_scheduler.thread_task_mu_ready) {
        return scheduler_raise(
            interp,
            "scheduler/thread-mutex-unavailable",
            "thread-cancel: thread mutex unavailable"
        );
    }
    if (args.len < 1) {
        return scheduler_raise(
            interp,
            "scheduler/thread-cancel-expected-thread-handle",
            "thread-cancel: expected thread-handle"
        );
    }

    usz thread_id = NO_OS_THREAD;
    uint generation = 0;
    Value* parse_err = scheduler_parse_thread_handle_arg(
        args[0],
        "scheduler/thread-cancel-expected-thread-handle",
        "thread-cancel: expected thread-handle",
        "scheduler/thread-cancel-invalid-thread-id",
        "thread-cancel: invalid thread handle",
        &thread_id,
        &generation,
        null,
        interp
    );
    if (parse_err != null) return parse_err;

    bool already_done = false;
    if (!scheduler_cancel_os_thread(thread_id, &already_done, generation)) {
        return scheduler_raise(
            interp,
            "scheduler/thread-cancel-thread-already-joined-or-invalid",
            "thread-cancel: thread already joined or invalid"
        );
    }
    if (already_done) return make_void(interp);
    return make_void(interp);
}
