module lisp;

import std::io;

fn Value* scheduler_validate_task_join_context(char[] op_name, Interp* interp) {
    if (!g_scheduler.thread_task_mu_ready) {
        char[96] ebuf;
        char[] msg = io::bprintf(&ebuf, "%s: task mutex unavailable", (ZString)op_name)!!;
        return scheduler_raise(interp, "scheduler/task-mutex-unavailable", msg);
    }
    if (!scheduler_is_running_fiber_context() && !g_scheduler.thread_task_cv_ready) {
        char[96] ebuf;
        char[] msg = io::bprintf(&ebuf, "%s: task condition variable unavailable", (ZString)op_name)!!;
        return scheduler_raise(interp, "scheduler/task-condition-unavailable", msg);
    }
    return null;
}

fn Value* scheduler_parse_task_join_timeout_pair(
    Value* pair,
    usz* task_id_out,
    uint* generation_out,
    SchedulerTaskHandle** handle_out,
    long* timeout_ms_out,
    Interp* interp
) {
    Value* task_ref = pair.cons_val.car;
    Value* timeout_val = pair.cons_val.cdr;
    if (!is_int(timeout_val)) {
        return scheduler_raise(
            interp,
            "scheduler/task-join-timeout-invalid-pair",
            "task-join-timeout: expected (task-handle . timeout-ms)"
        );
    }

    Value* task_err = scheduler_parse_task_handle_arg(
        task_ref,
        "scheduler/task-join-timeout-invalid-pair",
        "task-join-timeout: expected (task-handle . timeout-ms)",
        "scheduler/task-join-invalid-task-id",
        "task-join: invalid task handle",
        task_id_out,
        generation_out,
        handle_out,
        interp
    );
    if (task_err != null) return task_err;

    long timeout_ms = timeout_val.int_val;
    if (timeout_ms < 0) {
        return scheduler_raise(
            interp,
            "scheduler/task-join-timeout-negative-timeout",
            "task-join-timeout: timeout-ms must be >= 0"
        );
    }
    *timeout_ms_out = timeout_ms;
    return null;
}

fn Value* scheduler_parse_nonnegative_int_arg(
    Value*[] args,
    char[] expected_code,
    char[] expected_msg,
    char[] invalid_code,
    char[] invalid_msg,
    long* raw_id_out,
    Interp* interp
) {
    if (args.len < 1 || !is_int(args[0])) {
        return scheduler_raise(interp, expected_code, expected_msg);
    }
    long raw_id = args[0].int_val;
    if (raw_id < 0) return scheduler_raise(interp, invalid_code, invalid_msg);
    *raw_id_out = raw_id;
    return null;
}

fn Value* scheduler_validate_task_id(
    long raw_id,
    char[] invalid_code,
    char[] invalid_msg,
    usz* task_id_out,
    Interp* interp
) {
    if (raw_id < 0) return scheduler_raise(interp, invalid_code, invalid_msg);
    usz task_id = (usz)raw_id;
    if (task_id >= MAX_THREAD_TASKS) return scheduler_raise(interp, invalid_code, invalid_msg);
    *task_id_out = task_id;
    return null;
}

fn Value* scheduler_parse_existing_fiber_arg(
    Value*[] args,
    char[] expected_code,
    char[] expected_msg,
    char[] invalid_code,
    char[] invalid_msg,
    usz* fiber_id_out,
    Interp* interp
) {
    long raw_id = 0;
    Value* parse_err = scheduler_parse_nonnegative_int_arg(
        args,
        expected_code,
        expected_msg,
        invalid_code,
        invalid_msg,
        &raw_id,
        interp
    );
    if (parse_err != null) return parse_err;
    usz fiber_id = (usz)raw_id;
    if (fiber_id >= g_scheduler.fiber_count) {
        return scheduler_raise(interp, invalid_code, invalid_msg);
    }
    *fiber_id_out = fiber_id;
    return null;
}
