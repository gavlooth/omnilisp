module lisp;

fn Value* scheduler_true_value(Interp* interp) @inline {
    return interp.global_env.lookup(interp.sym_true);
}

<*
@require interp != null
*>
fn Value* prim_task_join(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    Value* context_err = scheduler_validate_task_join_context("task-join", interp);
    if (context_err != null) return context_err;
    if (args.len < 1) {
        return scheduler_raise(
            interp,
            "scheduler/task-join-expected-task-handle",
            "task-join: expected task-handle"
        );
    }

    usz task_id = NO_TASK;
    uint generation = 0;
    SchedulerTaskHandle* handle = null;
    Value* handle_err = scheduler_parse_task_handle_arg(
        args[0],
        "scheduler/task-join-expected-task-handle",
        "task-join: expected task-handle",
        "scheduler/task-join-invalid-task-id",
        "task-join: invalid task handle",
        &task_id,
        &generation,
        &handle,
        interp
    );
    if (handle_err != null) return handle_err;

    return scheduler_task_join_impl(task_id, generation, -1, handle, interp);
}

<*
@require interp != null
*>
fn Value* prim_task_join_timeout(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    Value* context_err = scheduler_validate_task_join_context("task-join-timeout", interp);
    if (context_err != null) return context_err;
    if (args.len < 1 || !is_cons(args[0])) {
        return scheduler_raise(
            interp,
            "scheduler/task-join-timeout-expected-pair",
            "task-join-timeout: expected (task-handle . timeout-ms)"
        );
    }

    usz task_id = NO_TASK;
    uint generation = 0;
    SchedulerTaskHandle* handle = null;
    long timeout_ms = 0;
    Value* parse_err = scheduler_parse_task_join_timeout_pair(
        args[0],
        &task_id,
        &generation,
        &handle,
        &timeout_ms,
        interp
    );
    if (parse_err != null) return parse_err;
    return scheduler_task_join_impl(task_id, generation, timeout_ms, handle, interp);
}

<*
@require interp != null
*>
fn Value* prim_task_cancel(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    if (!g_scheduler.thread_task_mu_ready) {
        return scheduler_raise(
            interp,
            "scheduler/task-mutex-unavailable",
            "task-cancel: task mutex unavailable"
        );
    }
    if (args.len < 1) {
        return scheduler_raise(
            interp,
            "scheduler/task-cancel-expected-task-handle",
            "task-cancel: expected task-handle"
        );
    }

    usz task_id = NO_TASK;
    uint generation = 0;
    Value* parse_err = scheduler_parse_task_handle_arg(
        args[0],
        "scheduler/task-cancel-expected-task-handle",
        "task-cancel: expected task-handle",
        "scheduler/task-cancel-invalid-task-id",
        "task-cancel: invalid task handle",
        &task_id,
        &generation,
        null,
        interp
    );
    if (parse_err != null) return parse_err;

    bool already_done = false;
    if (!scheduler_cancel_thread_task(task_id, &already_done, generation)) {
        return scheduler_raise(
            interp,
            "scheduler/task-cancel-task-already-joined-or-invalid",
            "task-cancel: task already joined or invalid"
        );
    }
    if (already_done) return make_void(interp);
    return make_void(interp);
}
