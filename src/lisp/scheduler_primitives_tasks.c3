module lisp;

import std::core::mem;

// ============================================================
// (__raw-task-spawn (op arg...)) -> task-handle
// (__raw-task-join task-handle) -> value
//
// Minimal threaded task API over sendable offload jobs.
// ============================================================

fn void scheduler_task_handle_finalizer(void* handle_ptr) {
    SchedulerTaskHandle* handle = (SchedulerTaskHandle*)handle_ptr;
    if (handle == null) return;
    if (handle.task_id < MAX_THREAD_TASKS && handle.generation != 0) {
        scheduler_drop_thread_task(handle.task_id, handle.generation);
    }
    mem::free(handle);
}

fn Value* scheduler_make_task_handle_value(usz task_id, uint generation, Interp* interp) {
    SchedulerTaskHandle* handle = (SchedulerTaskHandle*)mem::malloc(SchedulerTaskHandle.sizeof);
    if (handle == null) {
        scheduler_drop_thread_task(task_id, generation);
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-out-of-memory",
            "task-spawn: out of memory"
        );
    }
    handle.task_id = task_id;
    handle.generation = generation;

    Value* out = make_ffi_handle_ex(
        interp,
        handle,
        "task-handle",
        &scheduler_task_handle_finalizer,
        false,
        false
    );
    if (out == null) {
        scheduler_task_handle_finalizer(handle);
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-out-of-memory",
            "task-spawn: out of memory"
        );
    }
    return out;
}

fn Value* scheduler_parse_task_handle_arg(
    Value* arg,
    char[] expected_code,
    char[] expected_msg,
    char[] invalid_code,
    char[] invalid_msg,
    usz* task_id_out,
    uint* generation_out,
    SchedulerTaskHandle** handle_out,
    Interp* interp
) {
    if (task_id_out != null) *task_id_out = NO_TASK;
    if (generation_out != null) *generation_out = 0;
    if (handle_out != null) *handle_out = null;

    if (arg == null || arg.tag != FFI_HANDLE || arg.ffi_val == null || arg.ffi_val.lib_handle == null) {
        return scheduler_raise(interp, expected_code, expected_msg);
    }
    FfiHandle* box = arg.ffi_val;
    if (!scheduler_str_eq(box.lib_name[:box.name_len], "task-handle")) {
        return scheduler_raise(interp, expected_code, expected_msg);
    }

    SchedulerTaskHandle* handle = (SchedulerTaskHandle*)box.lib_handle;
    if (handle == null || handle.task_id >= MAX_THREAD_TASKS || handle.generation == 0) {
        return scheduler_raise(interp, invalid_code, invalid_msg);
    }

    if (task_id_out != null) *task_id_out = handle.task_id;
    if (generation_out != null) *generation_out = handle.generation;
    if (handle_out != null) *handle_out = handle;
    return null;
}

fn Value* scheduler_prepare_task_spawn(OffloadWork* work, usz* task_id_out, Interp* interp) {
    usz task_id = scheduler_alloc_thread_task();
    if (task_id == NO_TASK) {
        scheduler_release_shared(work.shared);
        return scheduler_raise(interp, "scheduler/task-spawn-task-table-full", "task-spawn: task table full");
    }

    work.fiber_id = NO_FIBER;
    work.task_id = task_id;
    if (!scheduler_enqueue_offload(*work)) {
        scheduler_release_shared(work.shared);
        scheduler_drop_thread_task(task_id);
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-admission-full",
            "task-spawn: admission cap reached"
        );
    }

    *task_id_out = task_id;
    return null;
}

fn Value* prim_task_spawn(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    scheduler_init();
    if (!g_scheduler.thread_task_mu_ready) {
        return scheduler_raise(
            interp,
            "scheduler/task-mutex-unavailable",
            "task-spawn: task mutex unavailable"
        );
    }
    OffloadWork work;
    Value* parse_err = scheduler_parse_offload_job(
        args,
        "scheduler/task-spawn-expected-job-list",
        "task-spawn: expected job list",
        false,
        &work,
        interp
    );
    if (parse_err != null) return parse_err;

    usz task_id = NO_TASK;
    Value* spawn_err = scheduler_prepare_task_spawn(&work, &task_id, interp);
    if (spawn_err != null) return spawn_err;

    uint generation = 0;
    if (!scheduler_get_thread_task_generation(task_id, &generation) || generation == 0) {
        scheduler_drop_thread_task(task_id);
        return scheduler_raise(
            interp,
            "scheduler/task-spawn-task-handle-invalid",
            "task-spawn: failed to create task handle"
        );
    }

    return scheduler_make_task_handle_value(task_id, generation, interp);
}

fn Value* scheduler_true_value(Interp* interp) @inline {
    return interp.global_env.lookup(interp.sym_true);
}

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
