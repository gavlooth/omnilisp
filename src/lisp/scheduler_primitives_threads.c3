module lisp;

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
