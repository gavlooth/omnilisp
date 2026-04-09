module lisp;

import std::core::mem;
import std::io;

fn Value* scheduler_begin_fiber_join_timeout_timer(
    usz fiber_id,
    long timeout_ms,
    char[] wait_failed_code,
    char[] op_name,
    Interp* interp
) {
    if (timeout_ms <= 0) return null;
    if (fiber_id >= g_scheduler.fiber_count) {
        return scheduler_raise(interp, wait_failed_code, "join wait: invalid fiber id");
    }
    if (!g_scheduler.uv_loop_ready || g_scheduler.uv_loop == null) {
        char[96] ebuf;
        if (try char[] msg = io::bprintf(&ebuf, "%s: libuv loop unavailable", (ZString)op_name)) {
            return scheduler_raise(interp, wait_failed_code, msg);
        }
        return scheduler_raise(interp, wait_failed_code, "join wait: libuv loop unavailable");
    }

    PendingSleep* sleep = &g_scheduler.pending_sleeps[fiber_id];
    if (sleep.active) {
        char[96] ebuf;
        if (try char[] msg = io::bprintf(&ebuf, "%s: wait timer already active", (ZString)op_name)) {
            return scheduler_raise(interp, wait_failed_code, msg);
        }
        return scheduler_raise(interp, wait_failed_code, "join wait: timer already active");
    }

    usz timer_size = uv_handle_size(UV_HANDLE_TIMER);
    if (timer_size == 0) {
        char[96] ebuf;
        if (try char[] msg = io::bprintf(&ebuf, "%s: libuv timer unavailable", (ZString)op_name)) {
            return scheduler_raise(interp, wait_failed_code, msg);
        }
        return scheduler_raise(interp, wait_failed_code, "join wait: libuv timer unavailable");
    }

    void* timer_handle = mem::malloc(timer_size);
    if (timer_handle == null) {
        return scheduler_raise(interp, wait_failed_code, "join wait: out of memory");
    }
    if (uv_timer_init(g_scheduler.uv_loop, timer_handle) != 0) {
        mem::free(timer_handle);
        char[96] ebuf;
        if (try char[] msg = io::bprintf(&ebuf, "%s: uv_timer_init failed", (ZString)op_name)) {
            return scheduler_raise(interp, wait_failed_code, msg);
        }
        return scheduler_raise(interp, wait_failed_code, "join wait: uv_timer_init failed");
    }

    sleep.active = true;
    sleep.completed = false;
    sleep.timer_handle = timer_handle;

    if (uv_timer_start(timer_handle, &scheduler_uv_sleep_timer_cb, (ulong)timeout_ms, 0) != 0) {
        uv_close(timer_handle, &scheduler_free_handle_cb);
        g_scheduler.pending_sleeps[fiber_id] = {};
        char[96] ebuf;
        if (try char[] msg = io::bprintf(&ebuf, "%s: uv_timer_start failed", (ZString)op_name)) {
            return scheduler_raise(interp, wait_failed_code, msg);
        }
        return scheduler_raise(interp, wait_failed_code, "join wait: uv_timer_start failed");
    }

    return null;
}

fn bool scheduler_fiber_join_timeout_elapsed(usz fiber_id) @inline {
    if (fiber_id >= g_scheduler.fiber_count) return false;
    PendingSleep* sleep = &g_scheduler.pending_sleeps[fiber_id];
    return sleep.active && sleep.completed;
}

fn void scheduler_uv_join_timeout_noop_cb(void* handle) {
    (void)handle;
}

fn Value* scheduler_task_join_fiber_until_complete(
    usz task_id,
    uint generation,
    SchedulerTaskHandle* handle,
    Interp* interp
) {
    usz waiter_id = g_scheduler.current_fiber_id;
    while (true) {
        OffloadCompletion* completion = scheduler_take_thread_task_completion(task_id, generation);
        if (completion != null) {
            Value* out = scheduler_value_from_offload_completion(completion, interp);
            scheduler_free_offload_completion(completion);
            if (handle != null) {
                handle.task_id = NO_TASK;
                handle.generation = 0;
            }
            return out;
        }

        if (!scheduler_thread_task_exists(task_id, generation)) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-task-already-joined-or-invalid",
                "task-join: task already joined or invalid"
            );
        }

        int wait_state = scheduler_register_thread_task_waiter(task_id, generation, waiter_id);
        if (wait_state == SCHEDULER_JOIN_WAIT_READY) continue;
        if (wait_state == SCHEDULER_JOIN_WAIT_INVALID) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-task-already-joined-or-invalid",
                "task-join: task already joined or invalid"
            );
        }
        if (wait_state == SCHEDULER_JOIN_WAIT_CONFLICT) {
            return scheduler_raise(
                interp,
                "scheduler/task-join-already-has-waiter",
                "task-join: task already has a waiting fiber"
            );
        }

        g_scheduler.fibers[waiter_id].state = FIBER_BLOCKED;
        Value*[1] yargs;
        yargs[0] = make_nil(interp);
        Value* y = prim_yield(yargs[..], null, interp);
        if (y != null && y.tag == ERROR) {
            scheduler_clear_thread_task_waiter(task_id, generation, waiter_id);
            scheduler_restore_blocked_fiber_ready(waiter_id);
            return scheduler_wrap_join_wait_error(
                y,
                "scheduler/task-join-wait-failed",
                "task-join: wait failed",
                interp
            );
        }
    }
}

fn Value* scheduler_wrap_join_wait_error(
    Value* wait_err,
    char[] code,
    char[] message,
    Interp* interp
) {
    if (wait_err == null) return null;

    Value* cause_data = wait_err;
    if (interp.flags.raise_pending) {
        if (interp.raise_payload != null) cause_data = interp.raise_payload;
        interp.flags.raise_pending = false;
        interp.raise_payload = null;
        interp.raise_msg_len = 0;
        interp.raise_msg[0] = 0;
    }
    return scheduler_raise(interp, code, message, cause_data);
}
