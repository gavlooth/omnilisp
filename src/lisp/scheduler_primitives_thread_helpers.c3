module lisp;

import std::core::mem;
import std::io;

fn void scheduler_os_thread_handle_finalizer(void* handle_ptr) {
    SchedulerOsThreadHandle* handle = (SchedulerOsThreadHandle*)handle_ptr;
    if (handle == null) return;
    if (handle.thread_id < MAX_OS_THREADS && handle.generation != 0) {
        scheduler_drop_os_thread(handle.thread_id, handle.generation);
    }
    mem::free(handle);
}

fn Value* scheduler_make_thread_handle_value(usz thread_id, uint generation, Interp* interp) {
    SchedulerOsThreadHandle* handle = (SchedulerOsThreadHandle*)mem::malloc(SchedulerOsThreadHandle.sizeof);
    if (handle == null) {
        scheduler_drop_os_thread(thread_id, generation);
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-out-of-memory",
            "thread-spawn: out of memory"
        );
    }
    handle.thread_id = thread_id;
    handle.generation = generation;

    Value* out = make_ffi_handle_ex(
        interp,
        handle,
        "thread-handle",
        &scheduler_os_thread_handle_finalizer,
        false,
        false
    );
    if (out == null) {
        scheduler_os_thread_handle_finalizer(handle);
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-out-of-memory",
            "thread-spawn: out of memory"
        );
    }
    return out;
}

fn Value* scheduler_parse_thread_handle_arg(
    Value* arg,
    char[] expected_code,
    char[] expected_msg,
    char[] invalid_code,
    char[] invalid_msg,
    usz* thread_id_out,
    uint* generation_out,
    SchedulerOsThreadHandle** handle_out,
    Interp* interp
) {
    if (thread_id_out != null) *thread_id_out = NO_OS_THREAD;
    if (generation_out != null) *generation_out = 0;
    if (handle_out != null) *handle_out = null;

    if (arg == null || arg.tag != FFI_HANDLE || arg.ffi_val == null || arg.ffi_val.lib_handle == null) {
        return scheduler_raise(interp, expected_code, expected_msg);
    }
    FfiHandle* box = arg.ffi_val;
    if (!scheduler_str_eq(box.lib_name[:box.name_len], "thread-handle")) {
        return scheduler_raise(interp, expected_code, expected_msg);
    }

    SchedulerOsThreadHandle* handle = (SchedulerOsThreadHandle*)box.lib_handle;
    if (handle == null || handle.thread_id >= MAX_OS_THREADS || handle.generation == 0) {
        return scheduler_raise(interp, invalid_code, invalid_msg);
    }

    if (thread_id_out != null) *thread_id_out = handle.thread_id;
    if (generation_out != null) *generation_out = handle.generation;
    if (handle_out != null) *handle_out = handle;
    return null;
}

fn Value* scheduler_prepare_os_thread_spawn(OffloadWork* work, usz* thread_id_out, uint* generation_out, Interp* interp) {
    usz thread_id = scheduler_alloc_os_thread();
    if (thread_id == NO_OS_THREAD) {
        scheduler_release_shared(work.shared);
        return scheduler_raise(interp, "scheduler/thread-spawn-thread-table-full", "thread-spawn: thread table full");
    }

    uint generation = 0;
    if (!scheduler_get_os_thread_generation(thread_id, &generation) || generation == 0) {
        scheduler_drop_os_thread(thread_id);
        scheduler_release_shared(work.shared);
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-thread-handle-invalid",
            "thread-spawn: failed to create thread handle"
        );
    }

    work.fiber_id = NO_FIBER;
    work.task_id = NO_TASK;
    if (!scheduler_start_os_thread(thread_id, generation, *work)) {
        scheduler_drop_os_thread(thread_id, generation);
        scheduler_release_shared(work.shared);
        return scheduler_raise(
            interp,
            "scheduler/thread-spawn-create-failed",
            "thread-spawn: failed to start OS thread"
        );
    }

    *thread_id_out = thread_id;
    *generation_out = generation;
    return null;
}

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
        char[] msg = io::bprintf(&ebuf, "%s: thread mutex unavailable", (ZString)op_name)!!;
        return scheduler_raise(interp, "scheduler/thread-mutex-unavailable", msg);
    }
    if (!scheduler_is_running_fiber_context() && !g_scheduler.thread_task_cv_ready) {
        char[96] ebuf;
        char[] msg = io::bprintf(&ebuf, "%s: thread condition variable unavailable", (ZString)op_name)!!;
        return scheduler_raise(interp, "scheduler/thread-condition-unavailable", msg);
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
