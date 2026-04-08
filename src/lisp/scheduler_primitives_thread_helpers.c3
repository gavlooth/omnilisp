module lisp;

import std::io;

fn Value* scheduler_thread_join_fiber_impl(
    usz thread_id,
    uint generation,
    long timeout_ms,
    SchedulerOsThreadHandle* handle,
    Interp* interp
) {
    if (timeout_ms < 0) {
        usz waiter_id = g_scheduler.current_fiber_id;
        while (true) {
            OffloadCompletion* completion = scheduler_take_os_thread_completion(thread_id, generation);
            if (completion != null) {
                Value* out = scheduler_value_from_offload_completion(completion, interp);
                scheduler_free_offload_completion(completion);
                if (handle != null) {
                    handle.thread_id = NO_OS_THREAD;
                    handle.generation = 0;
                }
                return out;
            }

            if (!scheduler_os_thread_exists(thread_id, generation)) {
                return scheduler_raise(
                    interp,
                    "scheduler/thread-join-thread-already-joined-or-invalid",
                    "thread-join: thread already joined or invalid"
                );
            }

            int wait_state = scheduler_register_os_thread_waiter(thread_id, generation, waiter_id);
            if (wait_state == SCHEDULER_JOIN_WAIT_READY) continue;
            if (wait_state == SCHEDULER_JOIN_WAIT_INVALID) {
                return scheduler_raise(
                    interp,
                    "scheduler/thread-join-thread-already-joined-or-invalid",
                    "thread-join: thread already joined or invalid"
                );
            }
            if (wait_state == SCHEDULER_JOIN_WAIT_CONFLICT) {
                return scheduler_raise(
                    interp,
                    "scheduler/thread-join-already-has-waiter",
                    "thread-join: thread already has a waiting fiber"
                );
            }

            g_scheduler.fibers[waiter_id].state = FIBER_BLOCKED;
            Value*[1] yargs;
            yargs[0] = make_nil(interp);
            Value* y = prim_yield(yargs[..], null, interp);
            if (y != null && y.tag == ERROR) {
                scheduler_clear_os_thread_waiter(thread_id, generation, waiter_id);
                return scheduler_wrap_join_wait_error(
                    y,
                    "scheduler/thread-join-wait-failed",
                    "thread-join: wait failed",
                    interp
                );
            }
        }
    }

    if (timeout_ms == 0) {
        return scheduler_raise(interp, "scheduler/thread-join-timeout", "thread-join: timeout");
    }

    usz waiter_id = g_scheduler.current_fiber_id;
    Value* timer_err = scheduler_begin_fiber_join_timeout_timer(
        waiter_id,
        timeout_ms,
        "scheduler/thread-join-wait-failed",
        "thread-join",
        interp
    );
    if (timer_err != null) return timer_err;
    defer scheduler_close_pending_sleep(waiter_id);

    while (true) {
        OffloadCompletion* completion = scheduler_take_os_thread_completion(thread_id, generation);
        if (completion != null) {
            Value* out = scheduler_value_from_offload_completion(completion, interp);
            scheduler_free_offload_completion(completion);
            if (handle != null) {
                handle.thread_id = NO_OS_THREAD;
                handle.generation = 0;
            }
            return out;
        }

        if (!scheduler_os_thread_exists(thread_id, generation)) {
            return scheduler_raise(
                interp,
                "scheduler/thread-join-thread-already-joined-or-invalid",
                "thread-join: thread already joined or invalid"
            );
        }

        if (scheduler_fiber_join_timeout_elapsed(waiter_id)) {
            scheduler_clear_os_thread_waiter(thread_id, generation, waiter_id);
            return scheduler_raise(interp, "scheduler/thread-join-timeout", "thread-join: timeout");
        }

        int wait_state = scheduler_register_os_thread_waiter(thread_id, generation, waiter_id);
        if (wait_state == SCHEDULER_JOIN_WAIT_READY) continue;
        if (wait_state == SCHEDULER_JOIN_WAIT_INVALID) {
            return scheduler_raise(
                interp,
                "scheduler/thread-join-thread-already-joined-or-invalid",
                "thread-join: thread already joined or invalid"
            );
        }
        if (wait_state == SCHEDULER_JOIN_WAIT_CONFLICT) {
            return scheduler_raise(
                interp,
                "scheduler/thread-join-already-has-waiter",
                "thread-join: thread already has a waiting fiber"
            );
        }

        g_scheduler.fibers[waiter_id].state = FIBER_BLOCKED;
        Value*[1] yargs;
        yargs[0] = make_nil(interp);
        Value* y = prim_yield(yargs[..], null, interp);
        if (y != null && y.tag == ERROR) {
            scheduler_clear_os_thread_waiter(thread_id, generation, waiter_id);
            return scheduler_wrap_join_wait_error(
                y,
                "scheduler/thread-join-wait-failed",
                "thread-join: wait failed",
                interp
            );
        }
    }
}

fn Value* scheduler_thread_join_impl(
    usz thread_id,
    uint generation,
    long timeout_ms,
    SchedulerOsThreadHandle* handle,
    Interp* interp
) {
    if (scheduler_is_running_fiber_context()) {
        Value* ctx_err = scheduler_require_fiber_root_context("thread-join", interp);
        if (ctx_err != null) return ctx_err;
        return scheduler_thread_join_fiber_impl(thread_id, generation, timeout_ms, handle, interp);
    }

    while (true) {
        OffloadCompletion* completion = scheduler_take_os_thread_completion(thread_id, generation);
        if (completion != null) {
            Value* out = scheduler_value_from_offload_completion(completion, interp);
            scheduler_free_offload_completion(completion);
            if (handle != null) {
                handle.thread_id = NO_OS_THREAD;
                handle.generation = 0;
            }
            return out;
        }

        if (!scheduler_os_thread_exists(thread_id, generation)) {
            return scheduler_raise(
                interp,
                "scheduler/thread-join-thread-already-joined-or-invalid",
                "thread-join: thread already joined or invalid"
            );
        }

        if (timeout_ms == 0) {
            return scheduler_raise(interp, "scheduler/thread-join-timeout", "thread-join: timeout");
        }

        int wait_state = scheduler_wait_os_thread_completion_state(thread_id, generation, timeout_ms);
        if (wait_state == SCHEDULER_JOIN_WAIT_READY) continue;
        if (wait_state == SCHEDULER_JOIN_WAIT_TIMEOUT) {
            return scheduler_raise(interp, "scheduler/thread-join-timeout", "thread-join: timeout");
        }
        if (wait_state == SCHEDULER_JOIN_WAIT_INVALID) {
            return scheduler_raise(
                interp,
                "scheduler/thread-join-thread-already-joined-or-invalid",
                "thread-join: thread already joined or invalid"
            );
        }
        return scheduler_raise(interp, "scheduler/thread-join-wait-failed", "thread-join: wait failed");
    }
}

fn Value* scheduler_validate_thread_join_context(char[] op_name, Interp* interp) {
    if (!g_scheduler.thread_task_mu_ready) {
        char[96] ebuf;
        if (try char[] msg = io::bprintf(&ebuf, "%s: thread mutex unavailable", (ZString)op_name)) {
            return scheduler_raise(interp, "scheduler/thread-mutex-unavailable", msg);
        }
        return scheduler_raise(interp, "scheduler/thread-mutex-unavailable", "thread join: thread mutex unavailable");
    }
    if (!scheduler_is_running_fiber_context() && !g_scheduler.thread_task_cv_ready) {
        char[96] ebuf;
        if (try char[] msg = io::bprintf(&ebuf, "%s: thread condition variable unavailable", (ZString)op_name)) {
            return scheduler_raise(interp, "scheduler/thread-condition-unavailable", msg);
        }
        return scheduler_raise(interp, "scheduler/thread-condition-unavailable", "thread join: thread condition variable unavailable");
    }
    return null;
}

fn Value* scheduler_parse_thread_join_timeout_pair(
    Value* pair,
    usz* thread_id_out,
    uint* generation_out,
    SchedulerOsThreadHandle** handle_out,
    long* timeout_ms_out,
    Interp* interp
) {
    Value* thread_ref = pair.cons_val.car;
    Value* timeout_val = pair.cons_val.cdr;
    if (!is_int(timeout_val)) {
        return scheduler_raise(
            interp,
            "scheduler/thread-join-timeout-invalid-pair",
            "thread-join-timeout: expected (thread-handle . timeout-ms)"
        );
    }

    Value* thread_err = scheduler_parse_thread_handle_arg(
        thread_ref,
        "scheduler/thread-join-timeout-invalid-pair",
        "thread-join-timeout: expected (thread-handle . timeout-ms)",
        "scheduler/thread-join-invalid-thread-id",
        "thread-join: invalid thread handle",
        thread_id_out,
        generation_out,
        handle_out,
        interp
    );
    if (thread_err != null) return thread_err;

    long timeout_ms = timeout_val.int_val;
    if (timeout_ms < 0) {
        return scheduler_raise(
            interp,
            "scheduler/thread-join-timeout-negative-timeout",
            "thread-join-timeout: timeout-ms must be >= 0"
        );
    }
    *timeout_ms_out = timeout_ms;
    return null;
}
